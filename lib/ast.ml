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
  Alpha convert the linked variables of the term to respect the convention of barendregt.
  @param t the term to alpha convert.
*)
let alpha_conversion (t : term) : term =

  (** 
    Collect all variables linked and free of a term
    @param t the term to collect the variables from.
  *)
  let rec collect_all_vars (t : term) : string list =
    match t with
    | Var x -> [x]
    | Abs (x, body) -> x :: collect_all_vars body
    | App (t1, t2) -> collect_all_vars t1 @ collect_all_vars t2
  in

  let existing_vars = collect_all_vars t in

  (** 
    Generate a new fresh variable wich is unique 
    @param old_var the old varible to rename.  
  *)
  let gen_new_var (old_var : string) : string =
    let new_var = ref (old_var ^ string_of_int !cpt) in
    cpt := !cpt + 1;
    while List.mem !new_var existing_vars do
      new_var := old_var ^ string_of_int !cpt;
      cpt := !cpt + 1
    done;
    !new_var
  in

  (**
    Rename the linked variables 
    @param t the term to convert
    @param env the current environment of linked variables and their  
  *)
  let rec convert (t : term) (env : (string * string) list) : term =
    match t with
    | Var x ->
        (match List.assoc_opt x env with
         | Some new_x -> Var new_x
         | None -> Var x)
    | Abs (x, body) ->
        let new_var = gen_new_var x in
        let new_env = (x, new_var) :: env in
        Abs (new_var, convert body new_env)
    | App (t1, t2) ->
        App (convert t1 env, convert t2 env)
  in

  convert t []