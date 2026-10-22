(** Definition of a lambda term *)
type term = 
  | Var of string
  | Abs of string * term
  | App of term * term

(** 
  Return the string of a lambda term
  @param t lambda term to get the string
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
  Print a lambda term in a human readable format  
  @param t the lambda term to print
*)
let pretty_printer (t : term) =
  print_string (to_string t); print_newline ()

(* Global counter used to defined non used name for variables *)
let cpt = ref 0


(** 
  Collect all variables linked and free of a term
  @param t the term to collect the variables from.
*)
let rec collect_all_vars (t : term) : string list =
  match t with
  | Var x -> [x]
  | Abs (x, body) -> x :: collect_all_vars body
  | App (t1, t2) -> collect_all_vars t1 @ collect_all_vars t2


(** 
  Generate a new fresh variable wich is unique 
  @param old_var the old varible to rename.  
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
  Rename the linked variables 
  @param t the term to convert
  @param env the current environment of linked variables and their new names.
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
  Alpha convert the linked variables of the term to respect the convention of barendregt.
  @param t the term to alpha convert.
*)
let alpha_conversion (t : term) : term =
  let existing_vars = collect_all_vars t in
  convert t [] existing_vars

(**
  Do the substitution of the variable x in t1 by t2.
  @param t1 the term to substitute the x variable
  @param t2 the term to subtitute with
  @param x the variable to substitute
  @return the final term
*)
let substitution (t1 : term) (t2 : term) (x : string) : term = 
  
  (*
    First alpha convert the term t1 while takin in account the variables of t2 to respectect the convention of barendregt, then substitute the linked varible x in t1 by t2. 
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
  Evaluate a lambda term by following the Left-to-Right Call-by-Value strategy.
  @param t the term to evaluate
  @return the evaluated term if it can be reducted, None if it can't
*)
let rec evaluate (t : term) : term option =
  match t with
  (* can't reduce a variable or an abstraction *)
  | Var _ | Abs _ ->
      None

  | App (m, n) -> 
    (* first try to evaluate m (left) *)
    match evaluate m with
    | Some m' -> 
        Some (App (m', n))
    | None -> 
      (* then if m can't be reduce try to evaluate n (right) *)
      match evaluate n with
      | Some n' -> Some (App (m, n'))
      | None ->
        (* m nor n can be reduced then if m an abstraction substitute its body by n *)
        match m with
        | App _ | Var _ -> None
        | Abs (x, body) -> 
          (* alpha convert n to ensure barendregt convention *)
          Some (substitution body (alpha_conversion n) x) 

(**
  Print the reduces of a term consecutively.
  @param t the term to show the reduction
  @param max_iteration the maximum number of iteration to avoid infinite loop for non reducable term.
*)
let rec reduce (t : term) (max_iteration : int) = 
  if max_iteration > 0 then
    match evaluate t with
    | Some t' -> pretty_printer t'; reduce t' (max_iteration - 1)
    | None -> ()