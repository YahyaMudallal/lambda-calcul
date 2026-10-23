type term = Ast.term =
  | Var of string
  | Abs of string * term
  | App of term * term

let to_string = Ast.to_string

let pretty_printer = Ast.pretty_printer

let parse = Parser.parse

let alpha_conversion = Ast.alpha_conversion

let substitution = Ast.substitution

let evaluate = Ast.evaluate

let reduce = Ast.reduce

type ptype = Ptype.ptype =
    Var of string
  | Arr of ptype * ptype

let to_string_ptype = Ptype.to_string_ptype
let pretty_printer_ptype = Ptype.pretty_printer_ptype

type equation = Ptype.equation

let generate_equation = Ptype.generate_equation

let pretty_printer_equation = Ptype.pretty_printer_equation

let print_equations_list = Ptype.print_equations_list