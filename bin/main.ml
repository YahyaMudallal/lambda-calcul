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
  print_equations_list eqs_term2
