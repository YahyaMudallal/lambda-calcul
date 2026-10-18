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


let alpa_convertion (t : term) (v : string) : term =
  let cpt = ref 0 in 
  
  let gen_new_var 
  (old_var : string) 
  (linked_vars : string list) 
  (free_vars : string list) : string =
    let new_var = ref (old_var ^ string_of_int !cpt) in
    while ((List.mem !new_var linked_vars) || (List.mem !new_var free_vars)) do
      cpt := !cpt + 1;
      new_var := old_var ^ (string_of_int !cpt);
    done; !new_var in

  let rec collect_free_variables 
  (t: term) 
  (linked_vars : string list) : 
  (string list) =
    match t with
      | Var(x) ->
          if List.mem x linked_vars then []
          else [x]
      | Abs(x, body) -> 
          collect_free_variables body (x::linked_vars)
      | App(t1, t2) -> 
          (collect_free_variables t1 linked_vars) @ (collect_free_variables t2 linked_vars) in

  let rec convert 
    (t : term) 
    (v : string) 
    (linked_vars : string list) 
    (free_vars : string list)
    (current_new_var: string)
    : term = 
    match t with
    | Var(x) -> 
        if List.mem x linked_vars then
          if String.compare x v = 0 then Var(current_new_var)
          else Var(x)
        else Var(x)
    | Abs(x, body) -> 
        if String.compare x v = 0 then
          let new_var = (gen_new_var x linked_vars free_vars) in
          Abs(new_var, convert body v (new_var :: linked_vars) free_vars new_var )
        else 
          Abs(x, convert body v (x :: linked_vars) free_vars current_new_var )
    | App(t1, t2) -> 
        App(
          convert t1 v linked_vars free_vars current_new_var,
          convert t2 v linked_vars free_vars current_new_var
        ) in 

  let free_vars = collect_free_variables t [] in
  convert t v [] free_vars ""