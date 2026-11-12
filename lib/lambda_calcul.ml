(** A term in the untyped lambda calculus. *)
type term = Ast.term =
  | Var of string
  | Abs of string * term
  | App of term * term
  | Int of int
  | Add of term * term
  | Sub of term * term
  | List of term list
  | Hd of term
  | Tl of term
  | Cons of term * term
  | Izte of term * term * term
  | Iete of term * term * term
  | Fix of term
  | Let of string * term * term

(**
  Converts a term to a readable string.
  @param t The term to convert.
  @return Its string representation.
*)
let to_string = Ast.to_string

(**
  Prints a term followed by a newline.
  @param t The term to print.
  @return [unit].
*)
let pretty_printer = Ast.pretty_printer

(**
  Parses a string as a lambda-calculus term.
  @param s The expression to parse.
  @return The parsed term.
  @raise Failure if the expression is invalid or malformed.
*)
let parse = Parser.parse

(**
  Renames the bound variables in a term.
  @param t The term to convert.
  @return An alpha-equivalent term with fresh bound-variable names.
*)
let alpha_conversion = Ast.alpha_conversion

(**
  Substitutes a term for the free occurrences of a variable.
  @param t1 The term in which to substitute.
  @param t2 The replacement term.
  @param x The variable to replace.
  @return The term resulting from the substitution.
*)
let substitution = Ast.substitution

(**
  Performs at most one left-to-right call-by-value reduction.
  @param t The term to reduce.
  @return [Some] of the reduced term, or [None] if no reduction is possible.
*)
let evaluate = Ast.evaluate

(**
  Prints successive reductions up to a normal form or an iteration limit.
  @param t The term whose reductions are printed.
  @param max_iteration The maximum number of reductions to perform.
  @return [unit].
*)
let reduce = Ast.reduce

(** A simple type: a type variable or a function type. *)
type ptype = Ptype.ptype =
    Var of string
  | Arr of ptype * ptype
  | N
  | List of ptype
  | Forall of string * ptype

(**
  Converts a type to a string.
  @param t The type to convert.
  @return Its string representation.
*)
let to_string_ptype = Ptype.to_string_ptype

(**
  Prints a type followed by a newline.
  @param t The type to print.
  @return [unit].
*)
let pretty_printer_ptype = Ptype.pretty_printer_ptype

(** An equation between two types. *)
type equation = Ptype.equation

(**
  Generates the type equations for a term.
  @param t The term whose type is to be inferred.
  @return The equations to solve in order to determine its type.
  @raise Failure if the term contains a free variable.
*)
let generate_equation = Ptype.generate_equation

(**
  Prints a type equation followed by a newline.
  @param eq The equation to print.
  @return [unit].
*)
let pretty_printer_equation = Ptype.pretty_printer_equation

(**
  Prints each equation on a separate line.
  @param eqs The equations to print.
  @return [unit].
*)
let print_equations_list = Ptype.print_equations_list

(**
  Processes the first equation in a type-equation system.
  Equal types are discarded, variable equations are substituted, and two
  function types are decomposed into their domains and codomains.
  @param eqs The equation system to reduce.
  @return The remaining equation system after one step.
  @raise Failure if the occurs check fails.
*)
let unification_step = Ptype.unification_step

(**
  Solves a type-equation system by unification.
  @param eqs The equation system to solve.
  @param timeout The maximum number of unification steps allowed.
  @return The substitutions mapping type variables to their resolved types.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
let solve_system = Ptype.solve_system

(**
  Infers the type of a lambda term.
  @param t The lambda term to infer the type for.
  @param timeout The maximum number of unification steps allowed.
  @return The inferred type of the term.
  @raise Failure if the limit is reached, the occurs check fails, or the types
         cannot be unified.
*)
let infer_type = Ptype.infer_type

(** Prints a list of type substitutions.
  @param subs The substitutions to print.
  @return [unit].
*)
let print_substitutions = Ptype.print_substitutions