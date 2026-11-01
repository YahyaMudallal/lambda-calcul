(** A term in the untyped lambda calculus. *)
type term = 
  | Var of string
  | Abs of string * term
  | App of term * term

(**
  Converts a term to a readable string.
  @param t The term to convert.
  @return The string representation of [t].
*)
let rec to_string (t : term) : string =
  match t with
    | Var(x) -> x
    | Abs(x, body) -> "λ" ^ x ^ "." ^ "(" ^ to_string body ^ ")"
    | App(t1, t2) ->
      let s1 = match t1 with
        | Abs _ -> "(" ^ to_string t1 ^ ")"
        | _ -> to_string t1
      in
      let s2 = match t2 with
        | App _ | Abs _ -> "(" ^ to_string t2 ^ ")"
        | _ -> to_string t2
      in
      s1 ^ " " ^ s2

(**
  Prints a term followed by a newline.
  @param t The term to print.
  @return [unit].
*)
let pretty_printer (t : term) =
  print_string (to_string t); print_newline ()

(* Global counter used to generate fresh variable names. *)
let cpt = ref 0


(**
  Collects the names of all bound and free variables in a term.
  Duplicate names are retained.
  @param t The term to inspect.
  @return The list of variable names encountered.
*)
let rec collect_all_vars (t : term) : string list =
  match t with
  | Var x -> [x]
  | Abs (x, body) -> x :: collect_all_vars body
  | App (t1, t2) -> collect_all_vars t1 @ collect_all_vars t2


(**
  Generates a fresh variable name based on an existing name.
  @param old_var The name used as the new name's prefix.
  @param existing_vars Names that must not be used.
  @return A name that is not present in [existing_vars].
*)
let gen_new_var (old_var : string) (existing_vars : string list): string =
  let new_var = ref (old_var ^ string_of_int !cpt) in
  cpt := !cpt + 1;
  while List.mem !new_var existing_vars do
    new_var := old_var ^ string_of_int !cpt;
    cpt := !cpt + 1
  done;
  !new_var

(**
  Renames bound variables according to the supplied environment.
  @param t The term to convert.
  @param env The mapping from original bound names to their new names.
  @param existing_vars Names to avoid when generating new names.
  @return The term with its bound variables renamed.
*)
let rec convert (t : term) (env : (string * string) list) (existing_vars : string list) : term =
  match t with
  | Var x ->
      (match List.assoc_opt x env with
        | Some new_x -> Var new_x
        | None -> Var x)
  | Abs (x, body) ->
      let new_var = gen_new_var x existing_vars in
      let new_env = (x, new_var) :: env in
      Abs (new_var, convert body new_env existing_vars)
  | App (t1, t2) ->
      App (convert t1 env existing_vars, convert t2 env existing_vars)

(**
  Renames bound variables so they are distinct from names already in the term.
  Free variables are left unchanged.
  @param t The term to convert.
  @return A term alpha-equivalent to [t].
*)
let alpha_conversion (t : term) : term =
  let existing_vars = collect_all_vars t in
  convert t [] existing_vars

(**
  Substitutes [t2] for the free occurrences of [x] in [t1].
  Bound variables in [t1] are renamed first to avoid variable capture.
  @param t1 The term in which to substitute.
  @param t2 The term to substitute.
  @param x The name of the variable to replace.
  @return The term resulting from the substitution.
*)
let substitution (t1 : term) (t2 : term) (x : string) : term = 
  
  (*
    Rename the bound variables in t1 while avoiding names in t2, then replace
    the free occurrences of x.
  *)

  let existing_vars = collect_all_vars t1 @ collect_all_vars t2 in

  let t1_converted = convert t1 [] existing_vars in

  let rec substitue (t1 : term): term =
    match t1 with 
    | Var(v) -> 
      if String.compare x v = 0 then
        t2
      else Var(v)
    | Abs(v, body) -> 
        if String.compare v x = 0 then
          Abs(v, body)
        else Abs(v, substitue body)
    | App (m, n) -> 
        App(substitue m, substitue n)
  in

  substitue t1_converted

(**
  Performs at most one left-to-right call-by-value reduction.
  @param t The term to reduce.
  @return [Some] of the reduced term, or [None] if no reduction is possible.
*)
let rec evaluate (t : term) : term option =
  match t with
  (* Variables and abstractions cannot be reduced. *)
  | Var _ | Abs _ ->
      None

  | App (m, n) -> 
    (* Try to evaluate the function part first. *)
    match evaluate m with
    | Some m' -> 
        Some (App (m', n))
    | None -> 
      (* If it is irreducible, try to evaluate the argument. *)
      match evaluate n with
      | Some n' -> Some (App (m, n'))
      | None ->
        (* Apply the abstraction once both parts are values. *)
        match m with
        | App _ | Var _ -> None
        | Abs (x, body) -> 
          (* Rename bound variables in the argument to avoid capture. *)
          Some (substitution body (alpha_conversion n) x) 

(**
  Prints successive reductions up to a normal form or the iteration limit.
  The initial term is not printed.
  @param t The term whose reductions are printed.
  @param max_iteration The maximum number of reductions to perform.
  @return [unit].
*)
let rec reduce (t : term) (max_iteration : int) = 
  if max_iteration > 0 then
    match evaluate t with
    | Some t' -> pretty_printer t'; reduce t' (max_iteration - 1)
    | None -> ()