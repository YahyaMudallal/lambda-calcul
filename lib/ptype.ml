open Ast

type ptype = 
  | Var of string
  | Arr of ptype * ptype

(* return the string of a type *)
let rec to_string_ptype (t : ptype) : string = 
  match t with
  | Var x -> x
  | Arr (x, y) -> 
      let s_left = match x with
        | Arr _ -> "(" ^ to_string_ptype x ^ ")"
        | Var _ -> to_string_ptype x
      in
      s_left ^ " -> " ^ (to_string_ptype y)

(* print a type *)
let pretty_printer_ptype (t : ptype) =
  print_endline (to_string_ptype t)

type equation = ptype * ptype

(* print an equation *)
let pretty_printer_equation (eq : equation) =
  let (t1, t2) = eq in
  print_endline ((to_string_ptype t1) ^ " = " ^ (to_string_ptype t2))

(* print a list of equations *)
let print_equations_list (eqs : equation list) =
  List.iter pretty_printer_equation eqs

let cpt_type = ref 0

(** Génère un nom de variable de type fraîche *)
let gen_new_type () : string = 
  let new_type = "T" ^ string_of_int !cpt_type in
  cpt_type := !cpt_type + 1;
  new_type

(**
  Génère les équations de typage d'un terme.
  @param t le terme à typer
  @return liste des équations générées
*)
let rec generate_equation 
  (t : term) 
  : equation list =
  let rec aux
    (t : term) 
    (env : (string * ptype) list) 
    (tc : ptype)
    : equation list =
    match t with
      | Var v -> 
          (match List.assoc_opt v env with
          | Some tv -> [(tv, tc)]
          | None -> failwith ("Variable libre non typable: " ^ v))

      | Abs (x, body) -> 
          let t_a = Var (gen_new_type ()) in
          let t_r = Var (gen_new_type ()) in
          (* T = Ta -> Tr *)
          (tc, Arr (t_a, t_r)) :: (aux body ((x, t_a) :: env) t_r)

      | App (m, n) -> 
          let t_a = Var (gen_new_type ()) in
          let eqs_m = aux m env (Arr (t_a, tc)) in
          let eqs_n = aux n env t_a in
          eqs_m @ eqs_n
  in
  aux t [] (Var(gen_new_type ()))

(** check that the variable v apear in the type t *)
let rec check_occur (v : string) (t : ptype) : bool = 
  match t with
  | Var (x) when x = v -> true
  | Arr(t1, t2) -> (check_occur v t1) || (check_occur v t2)
  | _ -> false

(* replace the variable v in type t1 by type t2 *)
let rec subsitute_type (t1 : ptype) (v : string) (t2 : ptype) : ptype =
  match t1 with
  | Var (x) when x = v -> t2
  | Arr (tg, td) -> 
      Arr(
        subsitute_type tg v t2, 
        subsitute_type td v t2)
  | _ -> t1

(* replace the variable v in all the types of the equation system eqs by the type t *)
let rec subsitute_equation_system (eqs : equation list) (v : string) (t : ptype) : equation list =
  match eqs with
  | [] -> []
  | (t1, t2)::q -> 
    ((subsitute_type t1 v t), (subsitute_type t2 v t)) :: subsitute_equation_system q v t 

