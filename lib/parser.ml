open Ast

(**
  Definition of a token
*)
type token =
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  | IDENT of string

(** 
  Check if a character is alphabetic 
  @param c the character to check
*)
let is_alphanum (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

(** 
  Tokenise a string into a list of tokens
  @param s the string to tokenise
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
  Parse a list of tokens into a lambda term
  @param tokens the list of tokens to parse
*)
let parse_tokens (tokens : token list) : term =

  (** 
    Parse an atomic expression
    @param tokens the list of tokens to parse
    @param parse_term the function to parse a term
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
    Parse an application
    @param t1 the first term of the application
    @param tokens the list of tokens to parse
    @param parse_term the function to parse a term
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
    Parse a term
    @param tokens the list of tokens to parse
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
  Parse a string into a lambda term 
  @param s the string to parse
  @return the parsed lambda term
*)
let parse (s : string) : term =
  let tokens = tokenise s in
  parse_tokens tokens
