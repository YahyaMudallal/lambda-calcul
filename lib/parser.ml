open Ast

(** A lexical token recognized by the term parser. *)
type token =
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | PLUS
  | MINUS
  | CONS
  | ASSIGN
  | BANG
  | EQUAL
  | LET
  | IN
  | HD
  | TL
  | IZTE
  | IETE
  | IFZERO
  | IFEMPTY
  | IF
  | THEN
  | ELSE
  | FIX
  | REF
  | REGION
  | UNIT
  | INT of int
  | IDENT of string
  | STRING of string

(**
  Checks whether a character is an ASCII digit.
*)
let is_digit (c : char) : bool = c >= '0' && c <= '9'

(**
  Checks whether a character can start an identifier.
*)
let is_ident_start (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(**
  Checks whether a character can be part of an identifier.
*)
let is_ident_char (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\''

(**
  Splits a string into lexical tokens.
  @param s The string to tokenize.
  @return The tokens in their original order.
  @raise Failure if the string contains an invalid character or unclosed literal.
*)
let tokenise (s : string) : token list =
  let n = String.length s - 1 in
  let tokens = ref [] in
  let i = ref 0 in
  while !i <= n do
    match s.[!i] with
    | ' ' | '\t' | '\n' | '\r' -> incr i
    | '@' | '\\' | '^' -> tokens := LAMBDA :: !tokens; incr i
    | '.' -> tokens := DOT :: !tokens; incr i
    | '(' -> tokens := LPAREN :: !tokens; incr i
    | ')' -> tokens := RPAREN :: !tokens; incr i
    | '[' -> tokens := LBRACKET :: !tokens; incr i
    | ']' -> tokens := RBRACKET :: !tokens; incr i
    | ';' -> tokens := SEMICOLON :: !tokens; incr i
    | '+' -> tokens := PLUS :: !tokens; incr i
    | '-' -> tokens := MINUS :: !tokens; incr i
    | '!' -> tokens := BANG :: !tokens; incr i
    | '=' -> tokens := EQUAL :: !tokens; incr i
    | ':' ->
        if !i + 1 <= n && s.[!i + 1] = ':' then begin
          tokens := CONS :: !tokens;
          i := !i + 2
        end else if !i + 1 <= n && s.[!i + 1] = '=' then begin
          tokens := ASSIGN :: !tokens;
          i := !i + 2
        end else
          failwith "Invalid character"
    | '"' ->
        let j = ref (!i + 1) in
        while !j <= n && s.[!j] <> '"' do
          incr j
        done;
        if !j > n then failwith "Unterminated string literal";
        let str = String.sub s (!i + 1) (!j - !i - 1) in
        tokens := STRING str :: !tokens;
        i := !j + 1
    | c when !i + 1 <= n && c = '\xce' && s.[!i + 1] = '\xbb' ->
        tokens := LAMBDA :: !tokens;
        i := !i + 2
    | c when is_digit c ->
        let j = ref !i in
        while !j <= n && is_digit s.[!j] do
          incr j
        done;
        let num_str = String.sub s !i (!j - !i) in
        tokens := INT (int_of_string num_str) :: !tokens;
        i := !j
    | c when is_ident_start c ->
        let j = ref !i in
        while !j <= n && is_ident_char s.[!j] do
          incr j
        done;
        let ident = String.sub s !i (!j - !i) in
        let tok = match ident with
          | "let" -> LET
          | "in" -> IN
          | "hd" -> HD
          | "tl" -> TL
          | "izte" -> IZTE
          | "iete" -> IETE
          | "ifzero" -> IFZERO
          | "ifempty" -> IFEMPTY
          | "if" -> IF
          | "then" -> THEN
          | "else" -> ELSE
          | "fix" -> FIX
          | "ref" -> REF
          | "region" -> REGION
          | "unit" -> UNIT
          | _ -> IDENT ident
        in
        tokens := tok :: !tokens;
        i := !j
    | _ -> failwith "Invalid character"
  done;
  List.rev !tokens

(**
  Checks whether the token list contains a 'THEN' token at current nesting depth.
*)
let has_then (tokens : token list) : bool =
  let rec loop depth = function
    | [] -> false
    | (LPAREN | LBRACKET) :: q -> loop (depth + 1) q
    | (RPAREN | RBRACKET) :: q ->
        if depth = 0 then false else loop (depth - 1) q
    | THEN :: _ when depth = 0 -> true
    | _ :: q -> loop depth q
  in
  loop 0 tokens

(**
  Parses a token list into a lambda term.
  @param tokens The tokens to parse.
  @return The term represented by [tokens].
  @raise Failure if the input is empty, malformed, or contains trailing tokens.
*)
let parse_tokens (tokens : token list) : term =

  let rec parse_expr (tokens : token list) : term * token list =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | LAMBDA :: q ->
        let rec parse_params = function
          | IDENT x :: DOT :: q' -> ([x], q')
          | IDENT x :: q' ->
              let (xs, rest) = parse_params q' in
              (x :: xs, rest)
          | _ -> failwith "Expected identifier after lambda"
        in
        let (params, rest) = parse_params q in
        let (body, rest') = parse_expr rest in
        let abs_term = List.fold_right (fun x acc -> Abs (x, acc)) params body in
        (abs_term, rest')

    | LET :: q ->
        (match q with
        | IDENT x :: EQUAL :: q' ->
            let (t1, rest1) = parse_expr q' in
            (match rest1 with
            | IN :: rest2 ->
                let (t2, rest3) = parse_expr rest2 in
                (Let (x, t1, t2), rest3)
            | _ -> failwith "Expected 'in' in let expression")
        | IDENT _ :: _ -> failwith "Expected '=' after identifier in let expression"
        | _ -> failwith "Expected identifier after let")

    | IFZERO :: q ->
        let (cond, rest1) = parse_expr q in
        (match rest1 with
        | THEN :: rest2 ->
            let (t_then, rest3) = parse_expr rest2 in
            (match rest3 with
            | ELSE :: rest4 ->
                let (t_else, rest5) = parse_expr rest4 in
                (Izte (cond, t_then, t_else), rest5)
            | _ -> failwith "Expected 'else' in ifzero expression")
        | _ -> failwith "Expected 'then' in ifzero expression")

    | IFEMPTY :: q ->
        let (cond, rest1) = parse_expr q in
        (match rest1 with
        | THEN :: rest2 ->
            let (t_then, rest3) = parse_expr rest2 in
            (match rest3 with
            | ELSE :: rest4 ->
                let (t_else, rest5) = parse_expr rest4 in
                (Iete (cond, t_then, t_else), rest5)
            | _ -> failwith "Expected 'else' in ifempty expression")
        | _ -> failwith "Expected 'then' in ifempty expression")

    | IF :: IDENT "zero" :: q ->
        parse_expr (IFZERO :: q)

    | IF :: IDENT "empty" :: q ->
        parse_expr (IFEMPTY :: q)

    | IF :: _ ->
        failwith "Expected 'zero' or 'empty' after 'if'"

    | IZTE :: q ->
        if has_then q then
          parse_expr (IFZERO :: q)
        else
          let (t1, rest1) = parse_prefix_expr q in
          let (t2, rest2) = parse_prefix_expr rest1 in
          let (t3, rest3) = parse_prefix_expr rest2 in
          (Izte (t1, t2, t3), rest3)

    | IETE :: q ->
        if has_then q then
          parse_expr (IFEMPTY :: q)
        else
          let (t1, rest1) = parse_prefix_expr q in
          let (t2, rest2) = parse_prefix_expr rest1 in
          let (t3, rest3) = parse_prefix_expr rest2 in
          (Iete (t1, t2, t3), rest3)

    | _ -> parse_assign_expr tokens

  and parse_assign_expr (tokens : token list) : term * token list =
    let (t1, rest) = parse_cons_expr tokens in
    match rest with
    | ASSIGN :: rest' ->
        let (t2, rest'') = parse_assign_expr rest' in
        (Assign (t1, t2), rest'')
    | _ -> (t1, rest)

  and parse_cons_expr (tokens : token list) : term * token list =
    let (t1, rest) = parse_arith_expr tokens in
    match rest with
    | CONS :: rest' ->
        let (t2, rest'') = parse_cons_expr rest' in
        (Cons (t1, t2), rest'')
    | _ -> (t1, rest)

  and parse_arith_expr (tokens : token list) : term * token list =
    let (t1, rest) = parse_app_expr tokens in
    let rec loop acc = function
      | PLUS :: q ->
          let (t2, rest') = parse_app_expr q in
          loop (Add (acc, t2)) rest'
      | MINUS :: q ->
          let (t2, rest') = parse_app_expr q in
          loop (Sub (acc, t2)) rest'
      | other -> (acc, other)
    in
    loop t1 rest

  and parse_app_expr (tokens : token list) : term * token list =
    let (t1, rest) = parse_prefix_expr tokens in
    parse_application t1 rest

  and parse_application (t1 : term) (tokens : token list) : term * token list =
    match tokens with
    | [] -> (t1, [])
    | (IDENT _ | INT _ | STRING _ | LPAREN | LBRACKET | UNIT | BANG | HD | TL | FIX | REF | REGION) :: _ ->
        let (t2, rest) = parse_prefix_expr tokens in
        parse_application (App (t1, t2)) rest
    | _ -> (t1, tokens)

  and parse_prefix_expr (tokens : token list) : term * token list =
    match tokens with
    | BANG :: q ->
        let (t, rest) = parse_prefix_expr q in
        (Deref t, rest)
    | HD :: q ->
        let (t, rest) = parse_prefix_expr q in
        (Hd t, rest)
    | TL :: q ->
        let (t, rest) = parse_prefix_expr q in
        (Tl t, rest)
    | REF :: q ->
        let (t, rest) = parse_prefix_expr q in
        (Ref t, rest)
    | FIX :: q ->
        (match q with
        | LAMBDA :: _ ->
            let (t, rest) = parse_expr q in
            (Fix t, rest)
        | _ ->
            let (t, rest) = parse_prefix_expr q in
            (Fix t, rest))
    | REGION :: q ->
        (match q with
        | IDENT r :: q' -> (Region r, q')
        | STRING r :: q' -> (Region r, q')
        | _ -> failwith "Expected region name after 'region'")
    | _ -> parse_atom tokens

  and parse_atom (tokens : token list) : term * token list =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | IDENT x :: q -> (Var x, q)
    | INT n :: q -> (Int n, q)
    | STRING s :: q -> (Var s, q)
    | UNIT :: q -> (Unit, q)
    | MINUS :: INT n :: q -> (Int (-n), q)
    | MINUS :: q ->
        let (t, rest) = parse_atom q in
        (Sub (Int 0, t), rest)
    | LPAREN :: RPAREN :: q -> (Unit, q)
    | LPAREN :: UNIT :: RPAREN :: q -> (Unit, q)
    | LPAREN :: q ->
        let (t, rest) = parse_expr q in
        (match rest with
        | RPAREN :: rest' -> (t, rest')
        | _ -> failwith "Expected closing parenthesis")
    | LBRACKET :: q ->
        parse_list q
    | _ -> failwith "Not expected"

  and parse_list (tokens : token list) : term * token list =
    match tokens with
    | RBRACKET :: q -> (List [], q)
    | _ ->
        let (elem, rest) = parse_expr tokens in
        let rec loop acc = function
          | SEMICOLON :: RBRACKET :: q ->
              (List (List.rev acc), q)
          | SEMICOLON :: q ->
              let (next_elem, next_rest) = parse_expr q in
              loop (next_elem :: acc) next_rest
          | RBRACKET :: q ->
              (List (List.rev acc), q)
          | _ -> failwith "Expected ';' or ']' in list"
        in
        loop [elem] rest
  in

  let (t, rest) = parse_expr tokens in
  if rest <> [] then failwith "Unexpected tokens after parsing"
  else t

(**
  Parses a string and constructs the corresponding lambda term.
  @param s The expression to parse.
  @return The term represented by [s].
  @raise Failure if a character is invalid or the expression is malformed.
*)
let parse (s : string) : term =
  let tokens = tokenise s in
  parse_tokens tokens
