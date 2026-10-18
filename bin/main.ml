open Lambda_calcul

let () = 
  let term = parse "@a.(a b)" in
  pretty_printer term;
  let term2 = parse "@x.@y.(x y)" in
  pretty_printer term2;

  let term3 = alpa_convertion term "a" in
  pretty_printer term3;

  let term4 = parse "((@x.x ((@x.x) y)) (@x.(x y)))" in
  pretty_printer term4;

  let term5 = alpa_convertion term4 "x" in
  pretty_printer term5;