type term = Ast.term =
  | Var of string
  | Abs of string * term
  | App of term * term

let to_string = Ast.to_string

let pretty_printer = Ast.pretty_printer

let parse = Parser.parse

let alpha_conversion = Ast.alpha_conversion