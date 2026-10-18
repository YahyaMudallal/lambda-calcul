open Lambda_calcul

(* Definition of Alcotest comparator for terms *)
let pp_term ppf t = Format.fprintf ppf "%s" (to_string t)
let term_testable = Alcotest.testable pp_term ( = )

(* Pretty Printing Tests *)
let test_to_string () =
  let t = Abs ("a", App (Var "a", Var "b")) in
  Alcotest.(check string) "to_string abstraction" "λa.(a b)" (to_string t)

(* Parsing Tests *)
let test_parse_variable () =
  let expected = Var "x" in
  let real = parse "x" in
  Alcotest.(check term_testable) "Variable simple" expected real

let test_parse_abstraction () =
  let expected = Abs ("x", Var "x") in
  let real = parse "@x.x" in
  Alcotest.(check term_testable) "Identité" expected real

let test_parse_application () =
  let expected = App (Var "f", Var "x") in
  let real = parse "(f x)" in
  Alcotest.(check term_testable) "Application f x" expected real

let test_parse_curry () =
  let expected = Abs ("x", Abs ("y", App (Var "x", Var "y"))) in
  let real = parse "@x.@y.(x y)" in
  Alcotest.(check term_testable) "Fonction curryfiée" expected real

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
        test_case "Erreur caractère" `Quick test_parse_invalid_char;
      ] );
  ]