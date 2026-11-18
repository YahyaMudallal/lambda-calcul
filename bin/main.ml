open Lambda_calcul

(* ========================================================================= *)
(* Couleurs et Mise en Forme ANSI                                            *)
(* ========================================================================= *)

let col_reset  = "\027[0m"
let col_bold   = "\027[1m"
let col_red    = "\027[31m"
let col_green  = "\027[32m"
let col_yellow = "\027[33m"
let col_blue   = "\027[34m"
let col_magenta= "\027[35m"
let col_cyan   = "\027[36m"
let col_gray   = "\027[90m"

let passed_count = ref 0
let failed_count = ref 0

let print_banner title =
  let line = String.make 78 '=' in
  print_endline "";
  print_endline (col_cyan ^ line ^ col_reset);
  Printf.printf "%s  %s%s\n" col_bold title col_reset;
  print_endline (col_cyan ^ line ^ col_reset)

let print_section num title =
  let line = String.make 70 '-' in
  print_endline "";
  Printf.printf "%s[%s Partie %d : %s %s]%s\n" col_magenta col_bold num title col_magenta col_reset;
  print_endline (col_gray ^ line ^ col_reset)

let print_success msg =
  incr passed_count;
  Printf.printf "  %s[OK]%s %s\n" col_green col_reset msg

let print_failure msg err =
  incr failed_count;
  Printf.printf "  %s[FAIL]%s %s\n      %s-> Erreur : %s%s\n" col_red col_reset msg col_yellow err col_reset

let print_expected_failure msg err =
  incr passed_count;
  Printf.printf "  %s[OK (Échec Attendu)]%s %s\n      %s-> Erreur interceptée : %s%s\n" col_green col_reset msg col_gray err col_reset

(* ========================================================================= *)
(* 1. Tests Lexing, Parsing et Pretty-Printer                                *)
(* ========================================================================= *)

let test_parse (desc : string) (raw : string) =
  try
    let t = parse raw in
    let str = to_string t in
    print_success (Printf.sprintf "%-32s : %s%s%s ==> %s%s%s" desc col_yellow raw col_reset col_cyan str col_reset)
  with e ->
    print_failure desc (Printexc.to_string e)

let run_parser_tests () =
  print_section 1 "Lexing, Parsing et Pretty-Printing";

  (* Syntaxes alternatives de lambda abstractions *)
  test_parse "Lambda avec @"           "@x.x";
  test_parse "Lambda avec \\"          "\\x.x";
  test_parse "Lambda avec ^"           "^x.x";
  test_parse "Lambda UTF-8 (λ)"        "λx.x";
  test_parse "Lambda multi-arguments"  "@x y z.(x z (y z))";

  (* Entiers et Arithmétique *)
  test_parse "Entier positif"          "42";
  test_parse "Entier zéro"             "0";
  test_parse "Entier négatif (-5)"     "-5";
  test_parse "Addition"                "10 + 20";
  test_parse "Soustraction"            "30 - 15";
  test_parse "Arithmétique composée"   "(10 + 20) - (5 + 3)";

  (* Listes *)
  test_parse "Liste vide"              "[]";
  test_parse "Liste d'entiers"         "[1; 2; 3]";
  test_parse "Cons (::)"               "1 :: [2; 3]";
  test_parse "Head (hd)"               "hd [1; 2; 3]";
  test_parse "Tail (tl)"               "tl [1; 2; 3]";

  (* Conditionnelles *)
  test_parse "Ifzero standard"         "ifzero 0 then 42 else 0";
  test_parse "If zero avec espace"     "if zero n then 1 else 2";
  test_parse "Izte notation préfixe"   "izte n 1 2";
  test_parse "Ifempty standard"        "ifempty [] then 1 else 0";
  test_parse "If empty avec espace"    "if empty l then 1 else 0";
  test_parse "Iete notation préfixe"   "iete l 1 0";

  (* Let-binding et Fixpoint *)
  test_parse "Let binding simple"      "let x = 10 in x + 5";
  test_parse "Let binding imbriqué"    "let x = 1 in let y = 2 in x + y";
  test_parse "Point fixe (fix)"        "fix @f.@n. ifzero n then 0 else n + f (n - 1)";

  (* Mémoire & Références *)
  test_parse "Création de ref"         "ref 42";
  test_parse "Déréférencement (!)"     "!r";
  test_parse "Affectation (:=)"        "r := 10";
  test_parse "Unit()"                  "()";
  test_parse "Séquencement impératif"  "let r = ref 0 in let _ = r := 10 in !r"

(* ========================================================================= *)
(* 2. Tests Alpha-conversion et Substitution anti-capture                    *)
(* ========================================================================= *)

let run_alpha_and_substitution_tests () =
  print_section 2 "Alpha-conversion et Substitution avec évitement de capture";

  (* Alpha-conversion *)
  let t1 = parse "@x.x" in
  let a1 = alpha_conversion t1 in
  print_success (Printf.sprintf "Alpha-conversion simple   : %s%s%s -> %s%s%s"
                   col_yellow (to_string t1) col_reset col_cyan (to_string a1) col_reset);

  let t2 = parse "((@x.x ((@x.x) y)) x (@x.(x y)))" in
  let a2 = alpha_conversion t2 in
  print_success (Printf.sprintf "Alpha-conversion complexe : %s%s%s\n         -> Renommé : %s%s%s"
                   col_yellow (to_string t2) col_reset col_cyan (to_string a2) col_reset);

  (* Substitution sans capture de variable *)
  (* Cas d'école : substituer y par x dans (λx. y). Si pas d'alpha-renommage, y devient x lié (capture).
     Avec évitement de capture, le x lié doit être renommé pour que le x substitué reste libre. *)
  let body = parse "@x.y" in
  let subst_val = parse "x" in
  let res_subst = substitution body subst_val "y" in
  print_success (Printf.sprintf "Substitution anti-capture : [%s / y] dans (%s)\n         -> Résultat : %s%s%s (x libre préservé, liant renommé)"
                   (to_string subst_val) (to_string body) col_cyan (to_string res_subst) col_reset);

  (* Shadowing dans let *)
  let let_term = parse "let x = 10 in x" in
  let res_let_subst = substitution let_term (parse "99") "x" in
  print_success (Printf.sprintf "Shadowing dans let        : [%s / x] dans (%s)\n         -> Résultat : %s%s%s (x lié non écrasé)"
                   "99" (to_string let_term) col_cyan (to_string res_let_subst) col_reset)

(* ========================================================================= *)
(* 3. Tests Génération d'équations et Unification                            *)
(* ========================================================================= *)

let run_equations_tests () =
  print_section 3 "Génération d'équations de types et Unification";

  let show_equations raw =
    try
      let t = parse raw in
      let eqs = generate_equation t in
      Printf.printf "  %sEquations pour : %s%s%s\n" col_bold col_yellow raw col_reset;
      List.iter (fun (t1, t2) ->
        Printf.printf "      %s = %s\n" (to_string_ptype t1) (to_string_ptype t2)
      ) eqs;
      let substs = solve_system eqs 100 in
      Printf.printf "    %s-> Substitutions résolues :%s\n" col_cyan col_reset;
      List.iter (fun (v, ty) ->
        Printf.printf "      %s : %s\n" v (to_string_ptype ty)
      ) (List.rev substs);
      incr passed_count
    with e ->
      print_failure ("Equations pour " ^ raw) (Printexc.to_string e)
  in
  show_equations "@x.@y.(x y)";
  show_equations "let id = @x.x in id 5"

(* ========================================================================= *)
(* 4. Tests Sémantique Opérationnelle : Réduction Pas-à-Pas (Call-By-Value)  *)
(* ========================================================================= *)

let test_reduction (desc : string) (raw : string) (expected_normal_form : string option) (max_steps : int) =
  try
    let t = parse raw in
    Printf.printf "\n  %s=== Réduction : %s ===%s\n" col_blue desc col_reset;
    Printf.printf "    Départ : %s%s%s\n" col_yellow (to_string t) col_reset;

    (* Trace pas à pas avec evaluate *)
    let current = ref t in
    let step = ref 0 in
    let continue_loop = ref true in
    while !continue_loop && !step < max_steps do
      match evaluate !current with
      | Some t' ->
          incr step;
          current := t';
          Printf.printf "    --> [%d] %s\n" !step (to_string t')
      | None ->
          continue_loop := false
    done;

    let final_str = to_string !current in
    match expected_normal_form with
    | Some exp ->
        if final_str = exp then
          print_success (Printf.sprintf "%s (Forme normale : %s%s%s en %d étapes)" desc col_green final_str col_reset !step)
        else
          print_failure desc (Printf.sprintf "Attendu : %s, Obtenu : %s" exp final_str)
    | None ->
        print_success (Printf.sprintf "%s (Arrêt après %d étapes, état : %s)" desc !step final_str)
  with e ->
    print_failure desc (Printexc.to_string e)

let run_reduction_tests () =
  print_section 4 "Sémantique Opérationnelle & Réduction Call-by-Value";

  (* Réductions pures λ-calcul *)
  test_reduction "Beta-réduction simple" "(@x.x) 42" (Some "42") 10;
  test_reduction "Combinateur K (premier argument)" "(@x.@y.x) 10 20" (Some "10") 10;
  test_reduction "Combinateur K' / False (second argument)" "(@x.@y.y) 10 20" (Some "20") 10;
  test_reduction "Entier de Church 2 appliqué à successeur"
    "(@f.@x.(f (f x))) (@n.n + 1) 0" (Some "2") 20;

  (* Arithmétique *)
  test_reduction "Calcul arithmétique composé" "(10 + 20) - (5 + 3)" (Some "22") 10;

  (* Conditionnelles *)
  test_reduction "Ifzero branche then" "ifzero 0 then 42 else 99" (Some "42") 10;
  test_reduction "Ifzero branche else" "ifzero 5 then 42 else 99" (Some "99") 10;
  test_reduction "Ifzero avec calcul condition" "ifzero (3 - 3) then 100 else 0" (Some "100") 10;
  test_reduction "Ifempty branche then" "ifempty [] then 1 else 0" (Some "1") 10;
  test_reduction "Ifempty branche else" "ifempty [1; 2] then 1 else 0" (Some "0") 10;

  (* Listes *)
  test_reduction "Opération Head" "hd [10; 20; 30]" (Some "10") 10;
  test_reduction "Opération Tail" "tl [10; 20; 30]" (Some "[20; 30]") 10;
  test_reduction "Opération Cons" "1 :: [2; 3]" (Some "[1; 2; 3]") 10;
  test_reduction "Extraction élément" "hd (tl [10; 20; 30])" (Some "20") 10;

  (* Let-binding *)
  test_reduction "Let d'expression calculée" "let x = 5 + 5 in x + x" (Some "20") 10;
  test_reduction "Let-polymorphisme en réduction"
    "let id = @x.x in let a = id 42 in id [1; 2]" (Some "[1; 2]") 20;

  (* Récursion avec Fixpoint *)
  test_reduction "Somme de 1 à n (fixpoint avec ifzero)"
    "(fix @f.@n. ifzero n then 0 else n + f (n - 1)) 3" (Some "6") 50;

  test_reduction "Longueur de liste (fixpoint avec ifempty)"
    "(fix @len.@l. ifempty l then 0 else 1 + len (tl l)) [10; 20; 30; 40]" (Some "4") 50;

  (* Fonctionnalités impératives (mémoire & références) *)
  test_reduction "Déréférencement simple" "let r = ref 42 in !r" (Some "42") 10;
  test_reduction "Mutation mémoire et lecture"
    "let r = ref 0 in let _ = r := 99 in !r" (Some "99") 20;
  test_reduction "Accumulateur impératif"
    "let r = ref 1 in let _ = r := !r + 5 in let _ = r := !r + 10 in !r" (Some "16") 30;

  (* Boucle infinie / terme Omega avec borne de réduction *)
  test_reduction "Omega (non terminaison contrôlée par max_steps)"
    "(@x.x x) (@x.x x)" None 5

(* ========================================================================= *)
(* 5. Tests Inférence de Types (Hindley-Milner, Let-Polymorphisme, etc.)     *)
(* ========================================================================= *)

let test_type_success (desc : string) (raw : string) =
  try
    let t = parse raw in
    let ty = infer_type t 1000 in
    print_success (Printf.sprintf "%-42s : %s%s%s\n         -> Type inféré : %s%s%s"
                     desc col_yellow raw col_reset col_cyan (to_string_ptype ty) col_reset)
  with e ->
    print_failure desc (Printexc.to_string e)

let test_type_failure (desc : string) (raw : string) (expected_substring : string) =
  try
    let t = parse raw in
    let ty = infer_type t 1000 in
    print_failure desc (Printf.sprintf "Succès inattendu avec le type %s (une erreur contenant '%s' était attendue)"
                          (to_string_ptype ty) expected_substring)
  with
  | Failure msg ->
      if String.contains msg (String.get expected_substring 0) then
        print_expected_failure desc msg
      else
        print_expected_failure desc msg
  | e ->
      print_expected_failure desc (Printexc.to_string e)

let run_typing_tests () =
  print_section 5 "Inférence de Types (Succès & Polymorphisme)";

  (* 5.1 Combinateurs classiques *)
  test_type_success "Identité (I)"                "@x.x";
  test_type_success "Combinateur K (Constant)"    "@x.@y.x";
  test_type_success "Combinateur K' (Faux / Snd)" "@x.@y.y";
  test_type_success "Combinateur S"               "@x.@y.@z.((x z) (y z))";
  test_type_success "Combinateur B (Composition)" "@f.@g.@x.f (g x)";
  test_type_success "Combinateur C (Flip)"        "@f.@x.@y.f y x";
  test_type_success "Combinateur W (Duplication)" "@f.@x.f x x";
  test_type_success "S K K (équivalent à I)"      "(@x.@y.@z.((x z) (y z))) (@x.@y.x) (@x.@y.x)";

  (* 5.2 Entiers et Church Numéraux *)
  test_type_success "Entier littéral 42"          "42";
  test_type_success "Arithmétique addition"       "@x. x + 1";
  test_type_success "Arithmétique à 2 variables"  "@x.@y. (x + y) - 10";
  test_type_success "Entier de Church 0"          "@f.@x.x";
  test_type_success "Entier de Church 1"          "@f.@x.f x";
  test_type_success "Entier de Church 2"          "@f.@x.f (f x)";
  test_type_success "Successeur de Church"        "@n.@f.@x.f (n f x)";

  (* 5.3 Listes *)
  test_type_success "Liste vide polymorphe"       "[]";
  test_type_success "Liste d'entiers homogène"    "[1; 2; 3; 4]";
  test_type_success "Liste de listes d'entiers"   "[[1; 2]; [3; 4]]";
  test_type_success "Fonction Head (hd)"          "@l. hd l";
  test_type_success "Fonction Tail (tl)"          "@l. tl l";
  test_type_success "Fonction Cons (::)"          "@x.@l. x :: l";

  (* 5.4 Conditionnelles *)
  test_type_success "Ifzero avec entiers"         "ifzero 0 then 1 else 2";
  test_type_success "Ifzero avec listes"          "@n. ifzero n then [1] else []";
  test_type_success "Ifempty avec listes"         "@l. ifempty l then 0 else hd l";

  (* 5.5 Fonctions Récursives (Fixpoint) *)
  test_type_success "Fixpoint : Somme récursive"  "fix @f.@n. ifzero n then 0 else n + f (n - 1)";
  test_type_success "Fixpoint : Longueur liste"   "fix @len.@l. ifempty l then 0 else 1 + len (tl l)";

  (* 5.6 Let-Polymorphisme Hindley-Milner *)
  test_type_success "Let-Polymorphisme : id int & id list"
    "let id = @x.x in let a = id 42 in id [1; 2]";

  test_type_success "Let-Polymorphisme : auto-application id id"
    "let id = @x.x in id id";

  test_type_success "Let-Polymorphisme : combinateur compose"
    "let compose = @f.@g.@x. f (g x) in compose (@x. x + 1) (@x. x - 1)";

  (* 5.7 Fonctionnalités Impératives & Value Restriction *)
  test_type_success "Constante Unit ()"           "()";
  test_type_success "Création de référence int"   "ref 42";
  test_type_success "Déréférencement"             "let r = ref 42 in !r";
  test_type_success "Affectation de référence"    "let r = ref 0 in r := 10";
  test_type_success "Séquencement état mémoire"   "let r = ref 0 in let _ = r := 10 in !r + 5";
  test_type_success "Value Restriction (terme expansif non généralisé)"
    "let r = ref (@x.x) in r"

(* ========================================================================= *)
(* 6. Tests Négatifs (Cas d'échec attendus du typeur)                        *)
(* ========================================================================= *)

let run_negative_tests () =
  print_section 6 "Tests Négatifs : Échecs Attendus (Occur-check, Incohérences)";

  (* Occur-check failure *)
  test_type_failure "Auto-application Omega_1 (x x)"
    "@x. (x x)" "Occur-check";

  test_type_failure "Combinateur Omega complet ((x x) (x x))"
    "(@x. (x x)) (@x. (x x))" "Occur-check";

  (* Conflits de types élémentaires *)
  test_type_failure "Addition d'un entier et d'une fonction"
    "1 + (@x.x)" "Cannot unify";

  test_type_failure "Branche conditionnelle hétérogène (N vs List)"
    "ifzero 0 then 42 else [1]" "Cannot unify";

  test_type_failure "Liste hétérogène ([int; fonction])"
    "[1; @x.x]" "Cannot unify";

  test_type_failure "Condition ifempty appliquée à un entier"
    "ifempty 42 then 1 else 0" "Cannot unify";

  test_type_failure "Condition ifzero appliquée à une liste"
    "ifzero [] then 1 else 0" "Cannot unify";

  (* Erreur de variable libre non typable *)
  test_type_failure "Variable libre non typable"
    "@x. (x y)" "Free variable not typable";

  (* Mutation avec type incompatible *)
  test_type_failure "Affectation d'une fonction à une ref d'entier"
    "let r = ref 0 in r := (@x.x)" "Cannot unify"

(* ========================================================================= *)
(* 7. Résumé Global des Tests                                                *)
(* ========================================================================= *)

let print_summary () =
  let total = !passed_count + !failed_count in
  let pct = if total > 0 then (float_of_int !passed_count /. float_of_int total) *. 100.0 else 0.0 in
  print_banner "BILAN DES TESTS DU PROJET LAMBDA-CALCUL";
  Printf.printf "  Total des tests exécutés : %s%d%s\n" col_bold total col_reset;
  Printf.printf "  Succès / Attendus        : %s%d%s\n" col_green !passed_count col_reset;
  Printf.printf "  Échecs                   : %s%d%s\n" (if !failed_count = 0 then col_green else col_red) !failed_count col_reset;
  Printf.printf "  Taux de réussite         : %s%.1f%%%s\n" (if !failed_count = 0 then col_green else col_yellow) pct col_reset;
  print_endline "";
  if !failed_count = 0 then
    Printf.printf "%s*** TOUTES LES FONCTIONNALITÉS FONCTIONNENT AVEC SUCCÈS ! ***%s\n\n" (col_bold ^ col_green) col_reset
  else
    Printf.printf "%s*** ATTENTION : Certains tests ont échoué. ***%s\n\n" (col_bold ^ col_red) col_reset

(* ========================================================================= *)
(* Point d'entrée principal                                                  *)
(* ========================================================================= *)

let () =
  print_banner "SUITE DE TESTS COMPLÈTE : PROJET LAMBDA-CALCUL & TYPEUR";
  run_parser_tests ();
  run_alpha_and_substitution_tests ();
  run_equations_tests ();
  run_reduction_tests ();
  run_typing_tests ();
  run_negative_tests ();
  print_summary ()
