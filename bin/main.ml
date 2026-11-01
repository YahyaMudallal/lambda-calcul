open Lambda_calcul

let () = 
  let term = parse "@a.(a b)" in
  pretty_printer term;
  let term2 = parse "@x.@y.(x y)" in
  pretty_printer term2;

  let term3 = alpha_conversion term in
  pretty_printer term3;

  let term4 = parse "((@x.x ((@x.x) y)) x (@x.(x y)))" in
  pretty_printer term4;
  
  let term5 = alpha_conversion term4 in
  pretty_printer term5;
  
  let term6 = Abs("x", Var("x5")) in
  pretty_printer term6;
  
  let term7 = parse "x" in
  pretty_printer term7;

  let term8 = substitution term6 term7 "x5" in
  pretty_printer term8;

  print_endline "start reduction of";
  pretty_printer term4;
  reduce term4 1000;
  
  let term9 = parse "(@x.x x) (@x.x x)" in
  print_endline "start reduction of";
  pretty_printer term9;
  reduce term9 5;

  let type1 : ptype = Var("a") in
  pretty_printer_ptype type1;

  print_endline "equations for the term";
  pretty_printer term2;
  let eqs_term2 : equation list = generate_equation term2 in
  print_equations_list eqs_term2;


  let test_term (name : string) (raw_term : string) (timeout : int) =
    print_endline ("=== Test : " ^ name ^ " ===");
    print_endline ("Terme : " ^ raw_term);
    try
      let t = parse raw_term in
      let substs = type_inference t timeout in
      print_endline "Succès ! Substitutions trouvées :";
      print_substitutions substs
    with
    | Failure msg -> print_endline ("Échec attendu/obtenu : " ^ msg)
    | e -> print_endline ("Erreur inattendue : " ^ Printexc.to_string e);
    print_newline () in

  let timeout = 100 in

  (* 1. L'identité : λx. x *)
  test_term "Identité (I)" "@x.x" timeout;

  (* 2. Le combinateur K : λx. λy. x *)
  test_term "Combinateur K" "@x.@y.x" timeout;

  (* 3. Le combinateur K appliqué à une variable muette (Faux booléen) : λx. λy. y *)
  test_term "Combinateur K' / False" "@x.@y.y" timeout;

  (* 4. Application simple : (λx. x) (λy. y) *)
  test_term "Application I I" "(@x.x) (@y.y)" timeout;

  (* 5. Double application : (λf. λx. f (f x)) *)
  test_term "Entier de Church 2" "@f.@x.(f (f x))" timeout;

  (* 6. Terme S K K : typable en λ-calcul simplement typé (équivalent à I) *)
  test_term "S K K" "(@x.@y.@z.((x z) (y z))) (@x.@y.x) (@x.@y.x)" timeout;

  (* 7. Auto-application non typable : λx. (x x) (doit lever un occur-check) *)
  test_term "Auto-application Omega_1 (non typable)" "@x.(x x)" timeout;

  (* 8. Combinateur Omega complet : (λx. x x) (λx. x x) *)
  test_term "Omega (non typable)" "(@x.(x x)) (@x.(x x))" timeout;

  (* 9. Terme S I I I : typable en λ-calcul simplement typé *)
  test_term "S I I I" "(@x.@y.@z.((x z) (y z))) (@x.x) (@x.x) (@x.x)" timeout;
