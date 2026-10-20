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