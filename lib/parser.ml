open Ast

(** A lexical token recognized by the term parser. *)
type token =
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  | IDENT of string

(**
  Checks whether a character is an ASCII letter or digit.
  @param c The character to check.
  @return [true] if [c] is a letter or digit, and [false] otherwise.
*)
let is_alphanum (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

(**
  Splits a string into lexical tokens.
  Identifiers contain ASCII letters and digits; only ordinary spaces are ignored.
  @param s The string to tokenize.
  @return The tokens in their original order.
  @raise Failure if the string contains an invalid character.
*)
let tokenise (s : string) : token list =
  let n = String.length s - 1 in
  let tokens = ref [] in
  let i = ref 0 in
  while !i <= n do
    match s.[!i] with
      | '@' -> tokens := LAMBDA :: !tokens; incr i
      | '.' -> tokens := DOT :: !tokens; incr i
      | '(' -> tokens := LPAREN :: !tokens; incr i
      | ')' -> tokens := RPAREN :: !tokens; incr i
      | ' ' -> incr i
      | c when is_alphanum c -> (** c alphanumeric character *)
        
        (* Extract the identifier *)
        let j = ref !i in
        while !j <= n && is_alphanum s.[!j] do
          incr j
        done;
        let ident = String.sub s !i (!j - !i) in
        
        tokens := IDENT(ident) :: !tokens;
        i := !j (** Skip the last character of the identifier *)
      | _ -> failwith "Invalid character"
  done; List.rev !tokens

(**
  Parses a token list into a lambda term.
  @param tokens The tokens to parse.
  @return The term represented by [tokens].
  @raise Failure if the input is empty, malformed, or contains trailing tokens.
*)
let parse_tokens (tokens : token list) : term =

  (**
    Parses a variable or a parenthesized expression.
    @param tokens The tokens to consume.
    @param parse_term The function used to parse the contents of parentheses.
    @return The parsed term and the remaining tokens.
    @raise Failure if the atomic expression is invalid or incomplete.
  *)
  let rec parse_atom 
  (tokens : token list) 
  (parse_term : token list -> (term * token list))
  : (term * token list) =
    match tokens with
      | [] -> failwith "Unexpected end of input"
      | IDENT(x)::q -> (Var(x), q)
      | LPAREN::q ->
        let (t, rest) = parse_term q in
        (match rest with
          | RPAREN::rest' -> (t, rest')
          | _ -> failwith "Expected closing parenthesis")
      | _ -> failwith "Not expected" in        
  
  (**
    Extends an application, associating its arguments from left to right.
    @param t1 The function or partial application parsed so far.
    @param tokens The remaining tokens.
    @param parse_term The function used to parse subterms.
    @return The constructed term and the remaining tokens.
  *)
  let rec parse_application 
    (t1 : term) 
    (tokens : token list) 
    (parse_term : token list -> (term * token list)) 
    : (term * token list) =
    match tokens with
      | [] -> (t1, [])
      | RPAREN::q -> (t1, tokens)
      | (IDENT _ | LPAREN)::_ ->
        let (t2, rest) = parse_atom tokens parse_term in
        parse_application (App(t1, t2)) rest parse_term
      | _ -> (t1, tokens) in

  (**
    Parses an abstraction or application from the remaining tokens.
    @param tokens The tokens to parse.
    @return The parsed term and the remaining tokens.
    @raise Failure if the expression is incomplete or malformed.
  *)
  let rec parse_term (tokens : token list) : (term * token list) =
    match tokens with
      | [] -> failwith "Unexpected end of input"
      | LAMBDA::q ->
        (match q with
          | IDENT(x)::DOT::q' ->
            let (body, rest) = parse_term q' in
            (Abs(x, body), rest)
          | _ -> failwith "Expected identifier after lambda")
      | _ ->
        let (t1, rest) = parse_atom tokens parse_term in
        parse_application t1 rest parse_term
    in
  
  let (t, rest) = parse_term tokens in
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
