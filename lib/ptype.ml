open Ast

(** A simple type: a type variable or a function type. *)
type ptype = 
  | Var of string
  | Arr of ptype * ptype
  | N
  | List of ptype
  | Forall of string * ptype

(**
  Converts a type to a string.
  @param t The type to convert.
  @return The string representation of [t].
*)
let rec to_string_ptype (t : ptype) : string = 
  match t with
  | Var x -> x
  | Arr (x, y) -> 
      let s_left = match x with
        | Arr _ -> "(" ^ (to_string_ptype x) ^ ")"
        | _ -> to_string_ptype x
      in
      s_left ^ " -> " ^ (to_string_ptype y)
  | N -> "N"
  | List t' -> "List " ^ (to_string_ptype t')
  | Forall (x, t') -> "forall " ^ x ^ ". " ^ (to_string_ptype t')

(**
  Prints a type followed by a newline.
  @param t The type to print.
  @return [unit].
*)
let pretty_printer_ptype (t : ptype) =
  print_endline (to_string_ptype t)

(** An equation between two types. *)
type equation = ptype * ptype

(**
  Prints a type equation followed by a newline.
  @param eq The equation to print.
  @return [unit].
*)
let pretty_printer_equation (eq : equation) =
  let (t1, t2) = eq in
  print_endline ((to_string_ptype t1) ^ " = " ^ (to_string_ptype t2))

(**
  Prints each equation on a separate line.
  @param eqs The equations to print.
  @return [unit].
*)
let print_equations_list (eqs : equation list) =
  List.iter pretty_printer_equation eqs

let cpt_type = ref 0

(**
  Generates a fresh type-variable name.
  @return A fresh name of the form [Tn].
*)
let gen_new_type () : string = 
  let new_type = "T" ^ string_of_int !cpt_type in
  cpt_type := !cpt_type + 1;
  new_type

(** 
  Collects all free type variables in a type, excluding those bound by Forall.
  @param bound The list of currently bound type-variable names.
  @param t The type to inspect.
  @return A list of free type-variable names in [t].
*)
let rec free_type_vars (bound : string list) (t : ptype) : string list =
  match t with
  | Var x -> if List.mem x bound then [] else [x]
  | Arr (t1, t2) -> free_type_vars bound t1 @ free_type_vars bound t2
  | N -> []
  | List tl -> free_type_vars bound tl
  | Forall (x, body) -> free_type_vars (x :: bound) body

(** 
  Collects all free type variables in the typing environment.
  @param env The typing environment.
  @return A list of free type-variable names in the environment.
*)
let free_vars_env (env : (string * ptype) list) : string list =
  List.concat_map (fun (_, ty) -> free_type_vars [] ty) env

(** 
  Removes duplicate elements from a list while preserving the order.
  @param lst The list to deduplicate.
  @return A new list with duplicates removed.
*)
let uniq lst =
  List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] lst

(** 
  Generalizes a type by quantifying over its free type variables that are not
  present in the typing environment.
  @param t The type to generalize.
  @param env The typing environment.
  @return A generalized type with Forall quantifiers for each free variable
          not in the environment.
*)
let generalize_type (t : ptype) (env : (string * ptype) list) : ptype =
  let env_fvs = free_vars_env env in
  let t_fvs = uniq (free_type_vars [] t) in
  let vars_to_generalize = List.filter (fun v -> not (List.mem v env_fvs)) t_fvs in
  List.fold_right (fun x acc -> Forall (x, acc)) vars_to_generalize t

(**
  Checks whether a type variable occurs in a type.
  @param v The name of the type variable to find.
  @param t The type to inspect.
  @return [true] if [v] occurs in [t], and [false] otherwise.
*)
let rec check_occur (v : string) (t : ptype) : bool = 
  match t with
  | Var (x) -> x = v
  | Arr(t1, t2) -> (check_occur v t1) || (check_occur v t2)
  | List t' -> check_occur v t'
  | Forall (x, body) -> if x = v then false else check_occur v body
  | N -> false

(**
  Replaces every occurrence of a type variable in a type.
  @param t1 The type in which to perform the replacement.
  @param v The type variable to replace.
  @param t2 The replacement type.
  @return The resulting type.
*)
let rec substitute_type (t1 : ptype) (v : string) (t2 : ptype) : ptype =
  match t1 with
  | Var (x) when x = v -> t2
  | Arr (tg, td) -> 
      Arr(substitute_type tg v t2, substitute_type td v t2)
  | List tl -> List (substitute_type tl v t2)
  | Forall (x, body) ->
    if x = v then
      Forall (x, body)
    else
      Forall (x, substitute_type body v t2)
  | _ -> t1

let rec generate_equation_aux
    (t : term)
    (env : (string * ptype) list)
    (tc : ptype) : equation list =
  match t with
  | Var v ->
      (match List.assoc_opt v env with
      | Some tv -> [(tv, tc)]
      | None -> failwith ("Free variable not typable : " ^ v))

  | Abs (x, body) ->
      let t_a = Var (gen_new_type ()) in
      let t_r = Var (gen_new_type ()) in
      (tc, Arr (t_a, t_r)) :: generate_equation_aux body ((x, t_a) :: env) t_r

  | App (m, n) ->
      let t_a = Var (gen_new_type ()) in
      let eqs_m = generate_equation_aux m env (Arr (t_a, tc)) in
      let eqs_n = generate_equation_aux n env t_a in
      eqs_m @ eqs_n

  | Int _ ->
      [(tc, N)]

  | Add (m, n) | Sub (m, n) ->
      let eqs_m = generate_equation_aux m env N in
      let eqs_n = generate_equation_aux n env N in
      (tc, N) :: (eqs_m @ eqs_n)

  | List ts ->
      let t_elem = Var (gen_new_type ()) in
      let eqs = List.concat_map (fun t_i -> generate_equation_aux t_i env t_elem) ts in
      (tc, List t_elem) :: eqs

  | Hd t_list ->
      let t_elem = Var (gen_new_type ()) in
      let eqs_list = generate_equation_aux t_list env (List t_elem) in
      (tc, t_elem) :: eqs_list

  | Tl t_list ->
      let t_elem = Var (gen_new_type ()) in
      let eqs_list = generate_equation_aux t_list env (List t_elem) in
      (tc, List t_elem) :: eqs_list

  | Cons (head, tail) ->
      let t_elem = Var (gen_new_type ()) in
      let eqs_head = generate_equation_aux head env t_elem in
      let eqs_tail = generate_equation_aux tail env (List t_elem) in
      (tc, List t_elem) :: (eqs_head @ eqs_tail)

  | Izte (cond, t_then, t_else) ->
      let eqs_cond = generate_equation_aux cond env N in
      let eqs_then = generate_equation_aux t_then env tc in
      let eqs_else = generate_equation_aux t_else env tc in
      eqs_cond @ eqs_then @ eqs_else

  | Iete (cond, t_then, t_else) ->
      let t_elem = Var (gen_new_type ()) in
      let eqs_cond = generate_equation_aux cond env (List t_elem) in
      let eqs_then = generate_equation_aux t_then env tc in
      let eqs_else = generate_equation_aux t_else env tc in
      eqs_cond @ eqs_then @ eqs_else

  | Fix t_fix ->
      let t_a = Var (gen_new_type ()) in
      let eqs_t = generate_equation_aux t_fix env (Arr (t_a, t_a)) in
      (tc, t_a) :: eqs_t

  | Let (x, t1, t2) ->
      (* infer t1 type and substitutions *)
      let (ty_t1, subst_t1) = infer_term_type t1 env 1000 in
      (* update environment with new constraints *)
      let updated_env = apply_subst_env subst_t1 env in
      (* generalize the type of t1 in the updated environment *)
      let gen_ty = generalize_type ty_t1 updated_env in
      (* type t2 in the updated environment *)
      generate_equation_aux t2 ((x, gen_ty) :: updated_env) tc

(**
  Generates the type equations for a term.
  @param t The term whose type is to be inferred.
  @return The equations to solve in order to determine its type.
  @raise Failure if the term contains a free variable.
*)
and generate_equation (t : term) : equation list =
  generate_equation_aux t [] (Var (gen_new_type ()))

(** 
  Apply a substitution list to a type. 
  @param subst The substitution list, where each element is a pair of a type variable and its replacement type.
  @param t The type to which the substitutions will be applied.
  @return The type after applying all substitutions.
*)
and apply_subst (subst : (string * ptype) list) (t : ptype) : ptype =
  List.fold_left (fun acc (v, rep) -> substitute_type acc v rep) t subst

(** 
  Apply a substitution list to all types in an environment.
  @param subst The substitution list, where each element is a pair of a type variable and its replacement type.
  @param env The typing environment, which is a list of pairs of variable names and their corresponding types.
  @return The environment after applying all substitutions to the types.
*)
and apply_subst_env (subst : (string * ptype) list) (env : (string * ptype) list) : (string * ptype) list =
  List.map (fun (x, ty) -> (x, apply_subst subst ty)) env


(**
  Infers the type of a term in a given environment.
  @param t The term whose type is to be inferred.
  @param env The typing environment mapping variable names to types.
  @param timeout The maximum number of unification steps allowed.
  @return The inferred type of [t].
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
and infer_term_type
    (t : term)
    (env : (string * ptype) list)
    (timeout : int) : ptype * (string * ptype) list =
  let alpha = Var (gen_new_type ()) in
  let eqs = generate_equation_aux t env alpha in
  let subst = solve_system eqs timeout in
  let resolved_alpha = apply_subst subst alpha in
  (resolved_alpha, subst)


(**
  Applies a type-variable replacement to every equation in a system.
  @param eqs The equation system to transform.
  @param v The type variable to replace.
  @param t The replacement type.
  @return The transformed equation system.
*)
and substitute_equation_system (eqs : equation list) (v : string) (t : ptype) : equation list =
  match eqs with
  | [] -> []
  | (t1, t2)::q -> 
    ((substitute_type t1 v t), (substitute_type t2 v t)) :: substitute_equation_system q v t 


(**
  Processes the first equation in a type-equation system.
  Returns the substitution (if any) and the remaining system. Equal types are
  discarded, variable equations are substituted, and function types are
  decomposed into their domains and codomains.
  @param eqs The equation system to reduce.
  @return A substitution and the remaining equation system after one step.
  @raise Failure if the occurs check fails.
*)
and unification_step
    (eqs : equation list) : (string * ptype) option * equation list =
  match eqs with
  | [] -> None, []
  | (t1, t2) :: q ->
      (* Discard equations whose two sides are already equal. *)
      if t1 = t2 then
        None, q
      else
        match (t1, t2) with
        (* When one side is a forall type, rename its bound variable 
        and keep the eqation without the forall *)
        | Forall (x, body), t2 | t2, Forall (x, body) ->
            let fresh_var = gen_new_type () in
            let renamed_body = substitute_type body x (Var fresh_var) in
            None, (renamed_body, t2) :: q

        (* Substitute a type variable when possible. *)
        | Var x, td | td, Var x ->
            if check_occur x td then
              failwith ("Occur-check failed: " ^ x ^ " occurs in type")
            else
              Some (x, td), substitute_equation_system q x td
        (* Decompose two function types. *)
        | Arr (tga, tgr), Arr (tda, tdr) ->
            None, (tga, tda) :: (tgr, tdr) :: q
        
        (* When both sides are list types, equate their elements types *)
        | List t1_elem, List t2_elem ->
            None, (t1_elem, t2_elem) :: q

        | _ ->
            failwith ("Cannot unify " ^ to_string_ptype t1 ^ " with " ^ to_string_ptype t2)

(**
  Solves a type-equation system by unification.
  @param eqs The equation system to solve.
  @param timeout The maximum number of unification steps allowed.
  @return The substitutions mapping type variables to their resolved types.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
and solve_system (eqs : equation list) (timeout : int) : (string * ptype) list =
  match eqs with
  | [] -> []
  | _ when timeout <= 0 -> failwith "Timeout exceeded during unification"
  | _ ->
      let step_substitution, remaining_eqs = unification_step eqs in
      let substitutions = solve_system remaining_eqs (timeout - 1) in
      match step_substitution with
      | None -> substitutions
      | Some (variable, ty) ->
          let resolved_ty =
            List.fold_left
              (fun acc (v, replacement) -> substitute_type acc v replacement)
              ty substitutions
          in
          (variable, resolved_ty) :: substitutions


(**
  Infers the type of a lambda term.
  @param t The lambda term to infer the type for.
  @param timeout The maximum number of unification steps allowed.
  @return The inferred type of the term.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
and infer_type (t : term) (timeout : int) : ptype =
  let (ty, _) = infer_term_type t [] timeout in
  generalize_type ty []

(** Prints a list of type substitutions.
  @param subs The substitutions to print.
  @return [unit].
*)
and print_substitutions (subs : (string * ptype) list) : unit =
  List.iter (fun (x, t) -> 
    print_endline ("  " ^ x ^ " : " ^ to_string_ptype t)
  ) subs