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
  | Var (x) when x = v -> true
  | Arr(t1, t2) -> (check_occur v t1) || (check_occur v t2)
  | _ -> false

(**
  Replaces every occurrence of a type variable in a type.
  @param t1 The type in which to perform the replacement.
  @param v The type variable to replace.
  @param t2 The replacement type.
  @return The resulting type.
*)
let rec subsitute_type (t1 : ptype) (v : string) (t2 : ptype) : ptype =
  match t1 with
  | Var (x) when x = v -> t2
  | Arr (tg, td) -> 
      Arr(
        subsitute_type tg v t2, 
        subsitute_type td v t2)
  | _ -> t1

(** 
  Instantiates a type by replacing its bound type variables with fresh ones.
  @param t The type to instantiate.
  @return The instantiated type.
*)
let rec instantiate (t : ptype) : ptype =
  match t with
  | Forall (x, body) ->
      let fresh = Var (gen_new_type ()) in
      let body' = subsitute_type body x fresh in
      instantiate body'
  | other -> other


(**
  Generates the type equations for a term.
  @param t The term whose type is to be inferred.
  @return The equations to solve in order to determine its type.
  @raise Failure if the term contains a free variable.
*)
let rec generate_equation (t : term) : equation list =
  let rec aux 
      (t : term) 
      (env : (string * ptype) list) 
      (tc : ptype) : equation list =
    match t with
    | Var v ->
        (match List.assoc_opt v env with
        | Some tv ->
            (* If tv is a polymorphic scheme, instantiate it. *)
            [(instantiate tv, tc)]
        | None -> failwith ("Free variable not typable : " ^ v))

    | Abs (x, body) ->
        let t_a = Var (gen_new_type ()) in
        let t_r = Var (gen_new_type ()) in
        (tc, Arr (t_a, t_r)) :: aux body ((x, t_a) :: env) t_r

    | App (m, n) ->
        let t_a = Var (gen_new_type ()) in
        let eqs_m = aux m env (Arr (t_a, tc)) in
        let eqs_n = aux n env t_a in
        eqs_m @ eqs_n

    | Int _ ->
        [(tc, N)]

    | Add (m, n) | Sub (m, n) ->
        let eqs_m = aux m env N in
        let eqs_n = aux n env N in
        (tc, N) :: (eqs_m @ eqs_n)

    | List ts ->
        let t_elem = Var (gen_new_type ()) in
        let eqs = List.concat_map (fun t_i -> aux t_i env t_elem) ts in
        (tc, List t_elem) :: eqs

    | Hd t_list ->
        let t_elem = Var (gen_new_type ()) in
        let eqs_list = aux t_list env (List t_elem) in
        (tc, t_elem) :: eqs_list

    | Tl t_list ->
        let t_elem = Var (gen_new_type ()) in
        let eqs_list = aux t_list env (List t_elem) in
        (tc, List t_elem) :: eqs_list

    | Cons (head, tail) ->
        let t_elem = Var (gen_new_type ()) in
        let eqs_head = aux head env t_elem in
        let eqs_tail = aux tail env (List t_elem) in
        (tc, List t_elem) :: (eqs_head @ eqs_tail)

    | Izte (cond, t_then, t_else) ->
        let eqs_cond = aux cond env N in
        let eqs_then = aux t_then env tc in
        let eqs_else = aux t_else env tc in
        eqs_cond @ eqs_then @ eqs_else

    | Iete (cond, t_then, t_else) ->
        let t_elem = Var (gen_new_type ()) in
        let eqs_cond = aux cond env (List t_elem) in
        let eqs_then = aux t_then env tc in
        let eqs_else = aux t_else env tc in
        eqs_cond @ eqs_then @ eqs_else

    | Fix t_fix ->
        let t_a = Var (gen_new_type ()) in
        let eqs_t = aux t_fix env (Arr (t_a, t_a)) in
        (tc, t_a) :: eqs_t

    | Let (x, t1, t2) ->
        let ty_t1 = infer_term_type t1 env 1000 in
        let gen_ty = generalize_type ty_t1 env in
        aux t2 ((x, gen_ty) :: env) tc
  in
  aux t [] (Var (gen_new_type ()))

and infer_term_type 
    (t : term) 
    (env : (string * ptype) list) 
    (timeout : int) : ptype =
  let alpha = Var (gen_new_type ()) in
  let rec aux (t : term) (env : (string * ptype) list) (tc : ptype) : equation list =
    match t with
    | Var v ->
        (match List.assoc_opt v env with
        | Some tv -> [(instantiate tv, tc)]
        | None -> failwith ("Free var: " ^ v))

    | Abs (x, body) ->
        let t_a = Var (gen_new_type ()) in
        let t_r = Var (gen_new_type ()) in
        (tc, Arr (t_a, t_r)) :: aux body ((x, t_a) :: env) t_r

    | App (m, n) ->
        let t_a = Var (gen_new_type ()) in
        (aux m env (Arr (t_a, tc))) @ (aux n env t_a)

    | Int _ -> [(tc, N)]
    | Add (m, n) | Sub (m, n) -> (tc, N) :: (aux m env N @ aux n env N)

    | List ts ->
        let t_elem = Var (gen_new_type ()) in
        (tc, List t_elem) :: List.concat_map (fun ti -> aux ti env t_elem) ts

    | Hd tl ->
        let t_elem = Var (gen_new_type ()) in
        (tc, t_elem) :: aux tl env (List t_elem)

    | Tl tl ->
        let t_elem = Var (gen_new_type ()) in
        (tc, List t_elem) :: aux tl env (List t_elem)

    | Cons (h, tl) ->
        let t_elem = Var (gen_new_type ()) in
        (tc, List t_elem) :: (aux h env t_elem @ aux tl env (List t_elem))

    | Izte (c, th, el) -> aux c env N @ aux th env tc @ aux el env tc

    | Iete (c, th, el) ->
        let t_elem = Var (gen_new_type ()) in
        aux c env (List t_elem) @ aux th env tc @ aux el env tc

    | Fix f ->
        let t_a = Var (gen_new_type ()) in
        (tc, t_a) :: aux f env (Arr (t_a, t_a))

    | Let (x, e1, e2) ->
        let ty_e1 = infer_term_type e1 env timeout in
        let gen_ty = generalize_type ty_e1 env in
        aux e2 ((x, gen_ty) :: env) tc
  in
  let eqs = aux t env alpha in
  let subst = solve_system eqs timeout in
  List.fold_left (fun acc (v, ty) -> subsitute_type acc v ty) alpha subst


(**
  Applies a type-variable replacement to every equation in a system.
  @param eqs The equation system to transform.
  @param v The type variable to replace.
  @param t The replacement type.
  @return The transformed equation system.
*)
and subsitute_equation_system (eqs : equation list) (v : string) (t : ptype) : equation list =
  match eqs with
  | [] -> []
  | (t1, t2)::q -> 
    ((subsitute_type t1 v t), (subsitute_type t2 v t)) :: subsitute_equation_system q v t 


(**
  Processes the first equation in a type-equation system.
  Equal types are discarded, variable equations are substituted, and two
  function types are decomposed into their domains and codomains.
  @param eqs The equation system to reduce.
  @return The remaining equation system after one step.
  @raise Failure if the occurs check fails.
*)
and unification_step (eqs : equation list) : equation list =
  match eqs with
  | [] -> []
  | (t1, t2) :: q ->
      (* Discard equations whose two sides are already equal. *)
      if t1 = t2 then
        q
      else
        match (t1, t2) with
        (* Substitute a type variable when possible. *)
        | Var x, td ->
            if check_occur x td then
              failwith ("Occur-check failed: " ^ x ^ " occurs in type")
            else
              subsitute_equation_system q x td
        | td, Var x ->
            if check_occur x td then
              failwith ("Occur-check failed: " ^ x ^ " occurs in type")
            else
              subsitute_equation_system q x td
        (* Decompose two function types. *)
        | Arr (tga, tgr), Arr (tda, tdr) ->
            (tga, tda) :: (tgr, tdr) :: q
        | _ -> failwith "Not implemented yet"

(**
  Solves a type-equation system by unification.
  @param eqs The equation system to solve.
  @param timeout The maximum number of unification steps allowed.
  @return The substitutions mapping type variables to their resolved types.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
and solve_system (eqs : equation list) (timeout : int) : (string * ptype) list =
  if timeout <= 0 then
    failwith "Timeout exceeded during unification"
  else
    match eqs with
    | [] -> []
    | (t1, t2) :: q ->
        if t1 = t2 then
          solve_system q (timeout - 1)
        else
          match (t1, t2) with
          | Var x, td | td, Var x ->
              if check_occur x td then
                failwith ("Occur-check failed on variable: " ^ x)
              else
                (* Apply the substitution to the remaining equations. *)
                let remaining_eqs = subsitute_equation_system q x td in
                (* Solve the reduced system. *)
                let subst = solve_system remaining_eqs (timeout - 1) in
                (* Resolve the replacement type and record the substitution. *)
                let td_resolved = List.fold_left (fun acc (v, t) -> subsitute_type acc v t) td subst in
                (x, td_resolved) :: subst

          | Arr (tga, tgr), Arr (tda, tdr) ->
              solve_system ((tga, tda) :: (tgr, tdr) :: q) (timeout - 1)
          | _ -> failwith "Not implemented yet"



(** Infers the types of variables in a lambda term.
  @param t The lambda term to infer types for.
  @param timeout The maximum number of unification steps allowed.
  @return A list of type substitutions for the variables in the term.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
and type_inference (t : term) (timeout : int) : (string * ptype) list =
  let eqs = generate_equation t in
  solve_system eqs timeout


(** Prints a list of type substitutions.
  @param subs The substitutions to print.
  @return [unit].
*)
and print_substitutions (subs : (string * ptype) list) : unit =
  List.iter (fun (x, t) -> 
    print_endline ("  " ^ x ^ " : " ^ to_string_ptype t)
  ) subs