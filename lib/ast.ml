(** A term in the untyped lambda calculus. *)
type term = 
  | Var of string
  | Abs of string * term
  | App of term * term
  | Int of int
  | Add of term * term
  | Sub of term * term
  | List of term list
  | Hd of term
  | Tl of term

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
    | Int(n) -> string_of_int n
    | Add(t1, t2) -> "(" ^ to_string t1 ^ " + " ^ to_string t2 ^ ")"
    | Sub(t1, t2) -> "(" ^ to_string t1 ^ " - " ^ to_string t2 ^ ")"
    | List(ts) -> "[" ^ String.concat "; " (List.map to_string ts) ^ "]"
    | Hd(t) -> "(hd " ^ to_string t ^ ")"
    | Tl(t) -> "(tl " ^ to_string t ^ ")"

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
  | Int _ -> []
  | Add (t1, t2) -> collect_all_vars t1 @ collect_all_vars t2
  | Sub (t1, t2) -> collect_all_vars t1 @ collect_all_vars t2
  | List ts -> List.flatten (List.map collect_all_vars ts)
  | Hd t -> collect_all_vars t
  | Tl t -> collect_all_vars t


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
  | Int n -> Int n
  | Add (t1, t2) -> 
      Add (convert t1 env existing_vars, convert t2 env existing_vars)
  | Sub (t1, t2) -> 
      Sub (convert t1 env existing_vars, convert t2 env existing_vars)
  | List ts -> List (List.map (fun t -> convert t env existing_vars) ts)
  | Hd t -> Hd (convert t env existing_vars)
  | Tl t -> Tl (convert t env existing_vars)

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
    | Int(n) -> Int(n)
    | Add(m, n) -> Add(substitue m, substitue n)
    | Sub(m, n) -> Sub(substitue m, substitue n)
    | List(ts) -> List(List.map substitue ts)
    | Hd(t) -> Hd(substitue t)
    | Tl(t) -> Tl(substitue t)
  in

  substitue t1_converted

(** 
  Checks if a term is a value (cannot be reduced further).
  @param t The term to check.
  @return [true] if [t] is a value, [false] otherwise.
*)
let is_val (t : term) : bool =
  match t with
  | Abs _ | Int _ -> true
  | _ -> false

(**
  Performs at most one left-to-right call-by-value reduction.
  @param t The term to reduce.
  @return [Some] of the reduced term, or [None] if no reduction is possible.
*)
let rec evaluate (t : term) : term option =

  (**
    Reduces a list of terms, returning [None] if no reduction is possible.
    @param ts The list of terms to reduce.
    @return [Some] of the reduced list, or [None] if no reduction is possible.
  *)
  let rec reduce_list (ts : term list) : term option =
    match ts with
    | [] -> None
    | t :: rest ->
        if not (is_val t) then
          (match evaluate t with
          | Some t' -> Some (List (t' :: rest))
          | None -> None)
        else
          (match reduce_list rest with
          | Some (List(ts')) -> Some (List (t :: ts'))
          | None -> None
          | _ -> failwith "Unexpected pattern in reduce_list")
  in

  match t with
  | Var _ | Abs _ | Int _ -> None

  | App (m, n) ->
      if not (is_val m) then
        (* try to reduce the left side *)
        (match evaluate m with
        | Some m' -> Some (App (m', n))
        | None -> None)
      else if not (is_val n) then
        (* try to reduce the right side *)
        (match evaluate n with
        | Some n' -> Some (App (m, n'))
        | None -> None)
      else
        (* Both are values: perform beta-reduction if m is an abstraction *)
        (match m with
        | Abs (x, body) -> Some (substitution body n x)
        | _ -> None)

  | Add (m, n) ->
      if not (is_val m) then
        (* try to reduce the left side *)
        (match evaluate m with
        | Some m' -> Some (Add (m', n))
        | None -> None)
      else if not (is_val n) then
        (* try to reduce the right side *)
        (match evaluate n with
        | Some n' -> Some (Add (m, n'))
        | None -> None)
      else
        (* Both are values : perform addition if both are integers *)
        (match (m, n) with
        | (Int a, Int b) -> Some (Int (a + b))
        | _ -> None)

  | Sub (m, n) ->
      if not (is_val m) then
        (* try to reduce the left side *)
        (match evaluate m with
        | Some m' -> Some (Sub (m', n))
        | None -> None)
      else if not (is_val n) then
        (* try to reduce the right side *)
        (match evaluate n with
        | Some n' -> Some (Sub (m, n'))
        | None -> None)
      else
        (* Both are values : perform subtraction if both are integers *)
        (match (m, n) with
        | (Int a, Int b) -> Some (Int (a - b))
        | _ -> None)

    | List ts ->
      reduce_list ts

    | Hd t ->
      if not (is_val t) then
        (* if t is not a value, try to reduce it *)
        (match evaluate t with
        | Some t' -> Some (Hd t')
        | None -> None)
      else
        (* t is a value, so we can reduce the head operation *)
        (match t with
        | List (h :: _) -> Some h
        | List [] -> failwith "Cannot take head of an empty list"
        | _ -> failwith "Head operation applied to a non-list value")

    | Tl t ->
      if not (is_val t) then
        (* if t is not a value, try to reduce it *)
        (match evaluate t with
        | Some t' -> Some (Tl t')
        | None -> None)
      else
        (* t is a value, so we can reduce the tail operation *)
        (match t with
        | List (_ :: tail) -> Some (List tail)
        | List [] -> failwith "Cannot take tail of an empty list"
        | _ -> failwith "Tail operation applied to a non-list value")

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