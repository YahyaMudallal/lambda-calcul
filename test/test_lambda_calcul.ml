open Lambda_calcul

(* Definition of Alcotest comparator for terms *)
let pp_term ppf (t : term) = Format.fprintf ppf "%s" (to_string t)
let term_testable : term Alcotest.testable = Alcotest.testable pp_term ( = )

(* Pretty Printing Tests *)
let test_to_string () =
  let t : term = Abs ("a", App (Var "a", Var "b")) in
  Alcotest.(check string) "to_string abstraction" "λa.(a b)" (to_string t)

(* Parsing Tests *)
let test_parse_variable () =
  let expected : term = Var "x" in
  let real = parse "x" in
  Alcotest.(check term_testable) "Variable simple" expected real

let test_parse_abstraction () =
  let expected : term = Abs ("x", Var "x") in
  let real = parse "@x.x" in
  Alcotest.(check term_testable) "Identité" expected real;
  let real_hat = parse "^x.x" in
  Alcotest.(check term_testable) "Identité avec ^" expected real_hat;
  let real_slash = parse "\\x.x" in
  Alcotest.(check term_testable) "Identité avec \\" expected real_slash;
  let real_utf8 = parse "λx.x" in
  Alcotest.(check term_testable) "Identité avec λ" expected real_utf8

let test_parse_application () =
  let expected : term = App (Var "f", Var "x") in
  let real = parse "(f x)" in
  Alcotest.(check term_testable) "Application f x" expected real

let test_parse_curry () =
  let expected : term = Abs ("x", Abs ("y", App (Var "x", Var "y"))) in
  let real = parse "@x.@y.(x y)" in
  Alcotest.(check term_testable) "Fonction curryfiée" expected real;
  let real_multi = parse "@x y.(x y)" in
  Alcotest.(check term_testable) "Fonction curryfiée multi-arguments" expected real_multi

let test_parse_integers () =
  Alcotest.(check term_testable) "Entier 42" (Int 42 : term) (parse "42");
  Alcotest.(check term_testable) "Entier 0" (Int 0 : term) (parse "0");
  Alcotest.(check term_testable) "Entier négatif -5" (Int (-5) : term) (parse "-5")

let test_parse_arithmetic () =
  let expected_add : term = Add (Var "x", Int 1) in
  Alcotest.(check term_testable) "Addition simple" expected_add (parse "x + 1");
  Alcotest.(check term_testable) "Addition parenthésée" expected_add (parse "(x + 1)");
  let expected_sub : term = Sub (Int 10, Int 3) in
  Alcotest.(check term_testable) "Soustraction" expected_sub (parse "10 - 3");
  let expected_chain : term = Sub (Add (Int 1, Int 2), Int 3) in
  Alcotest.(check term_testable) "Chaîne gauche-associative" expected_chain (parse "1 + 2 - 3")

let test_parse_list () =
  Alcotest.(check term_testable) "Liste vide" (List [] : term) (parse "[]");
  Alcotest.(check term_testable) "Liste vide parenthésée" (List [] : term) (parse "([])");
  let expected_list : term = List [Int 1; Int 2; Int 3] in
  Alcotest.(check term_testable) "Liste [1; 2; 3]" expected_list (parse "[1; 2; 3]");
  Alcotest.(check term_testable) "Liste avec point-virgule final" expected_list (parse "[1; 2; 3;]")

let test_parse_cons () =
  let expected : term = Cons (Int 1, List [Int 2]) in
  Alcotest.(check term_testable) "Cons simple" expected (parse "1 :: [2]");
  let expected_assoc : term = Cons (Int 1, Cons (Int 2, List [])) in
  Alcotest.(check term_testable) "Cons associatif à droite" expected_assoc (parse "1 :: 2 :: []")

let test_parse_hd_tl () =
  Alcotest.(check term_testable) "hd simple" (Hd (Var "l") : term) (parse "hd l");
  Alcotest.(check term_testable) "hd parenthésé" (Hd (Var "l") : term) (parse "(hd l)");
  Alcotest.(check term_testable) "tl simple" (Tl (Var "l") : term) (parse "tl l");
  Alcotest.(check term_testable) "hd !l" (Hd (Deref (Var "l")) : term) (parse "hd !l")

let test_parse_branching () =
  let expected_izte : term = Izte (Var "c", Int 1, Int 2) in
  Alcotest.(check term_testable) "izte forme 3 arguments" expected_izte (parse "izte c 1 2");
  Alcotest.(check term_testable) "izte parenthésé" expected_izte (parse "(izte c 1 2)");
  Alcotest.(check term_testable) "ifzero then else" expected_izte (parse "ifzero c then 1 else 2");
  Alcotest.(check term_testable) "if zero then else" expected_izte (parse "if zero c then 1 else 2");
  Alcotest.(check term_testable) "izte then else" expected_izte (parse "izte c then 1 else 2");

  let expected_iete : term = Iete (Var "c", Int 1, Int 2) in
  Alcotest.(check term_testable) "iete forme 3 arguments" expected_iete (parse "iete c 1 2");
  Alcotest.(check term_testable) "iete parenthésé" expected_iete (parse "(iete c 1 2)");
  Alcotest.(check term_testable) "ifempty then else" expected_iete (parse "ifempty c then 1 else 2");
  Alcotest.(check term_testable) "if empty then else" expected_iete (parse "if empty c then 1 else 2");
  Alcotest.(check term_testable) "iete then else" expected_iete (parse "iete c then 1 else 2")

let test_parse_imperative () =
  Alcotest.(check term_testable) "unit ()" (Unit : term) (parse "()");
  Alcotest.(check term_testable) "unit mot-clé" (Unit : term) (parse "unit");
  Alcotest.(check term_testable) "(unit)" (Unit : term) (parse "(unit)");
  Alcotest.(check term_testable) "ref simple" (Ref (Int 5) : term) (parse "ref 5");
  Alcotest.(check term_testable) "ref parenthésé" (Ref (Int 5) : term) (parse "(ref 5)");
  Alcotest.(check term_testable) "deref simple" (Deref (Var "x") : term) (parse "!x");
  Alcotest.(check term_testable) "deref parenthésé" (Deref (Var "x") : term) (parse "(!x)");
  Alcotest.(check term_testable) "assignation" (Assign (Var "x", Int 42) : term) (parse "x := 42");
  Alcotest.(check term_testable) "assignation parenthésée" (Assign (Var "x", Int 42) : term) (parse "(x := 42)");
  Alcotest.(check term_testable) "region" (Region "r0" : term) (parse "region r0");
  Alcotest.(check term_testable) "region parenthésée" (Region "r0" : term) (parse "(region r0)")

let test_parse_let_fix () =
  let expected_let : term = Let ("x", Int 1, Add (Var "x", Int 2)) in
  Alcotest.(check term_testable) "let in" expected_let (parse "let x = 1 in x + 2");
  Alcotest.(check term_testable) "(let in)" expected_let (parse "(let x = 1 in (x + 2))");
  let expected_fix : term = Fix (Abs ("f", Var "f")) in
  Alcotest.(check term_testable) "fix simple" expected_fix (parse "fix (@f.f)");
  Alcotest.(check term_testable) "fix avec lambda direct" expected_fix (parse "fix @f.f")

let test_parse_subject_example () =
  let raw = "let l = ref [] in let _ = l := [(^x.x)] in (hd !l) + 2" in
  let expected : term =
    Let (
      "l",
      Ref (List []),
      Let (
        "_",
        Assign (Var "l", List [Abs ("x", Var "x")]),
        Add (Hd (Deref (Var "l")), Int 2)
      )
    )
  in
  Alcotest.(check term_testable) "Exemple sujet PDF page 4" expected (parse raw)

let test_roundtrip () =
  let terms : term list = [
    Var "x";
    Abs ("x", Var "x");
    App (Var "f", Var "x");
    Int 42;
    Add (Var "x", Int 1);
    Sub (Var "x", Int 1);
    List [];
    List [Int 1; Int 2; Int 3];
    Hd (Var "l");
    Tl (Var "l");
    Cons (Int 1, List [Int 2]);
    Izte (Int 0, Int 1, Int 2);
    Iete (List [], Int 1, Int 2);
    Fix (Abs ("f", Var "f"));
    Let ("x", Int 1, Var "x");
    Deref (Var "x");
    Ref (Int 1);
    Assign (Var "x", Int 1);
    Region "r0";
    Unit;
  ] in
  List.iter (fun (t : term) ->
    let s = to_string t in
    let parsed = parse s in
    Alcotest.(check term_testable) ("Roundtrip: " ^ s) t parsed
  ) terms

(* Invalid parsing tests *)
let test_parse_invalid_char () =
  Alcotest.check_raises "Caractère invalide"
    (Failure "Invalid character")
    (fun () -> ignore (parse "@x.?"))

(** Main function to run the tests *)
let () =
  let open Alcotest in
  run "Tests Lambda-Calcul" [
    ( "pretty_printer",
      [
        test_case "Formatage basique" `Quick test_to_string;
      ] );
    ( "parser",
      [
        test_case "Variable" `Quick test_parse_variable;
        test_case "Abstraction" `Quick test_parse_abstraction;
        test_case "Application" `Quick test_parse_application;
        test_case "Curryfication" `Quick test_parse_curry;
        test_case "Entiers" `Quick test_parse_integers;
        test_case "Arithmétique" `Quick test_parse_arithmetic;
        test_case "Listes" `Quick test_parse_list;
        test_case "Cons" `Quick test_parse_cons;
        test_case "Hd et Tl" `Quick test_parse_hd_tl;
        test_case "Branchements" `Quick test_parse_branching;
        test_case "Traits impératifs" `Quick test_parse_imperative;
        test_case "Let et Fix" `Quick test_parse_let_fix;
        test_case "Exemple sujet PDF" `Quick test_parse_subject_example;
        test_case "Roundtrip to_string / parse" `Quick test_roundtrip;
        test_case "Erreur caractère" `Quick test_parse_invalid_char;
      ] );
  ]
