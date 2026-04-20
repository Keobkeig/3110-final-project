let expr_to_dot _expr =
  "digraph Expr {\n" ^ "  label=\"TODO: expression graph export pending\";\n"
  ^ "  labelloc=t;\n" ^ "  n0 [label=\"TODO\"];\n" ^ "}\n"

let write_expr_dot path expr =
  let dot = expr_to_dot expr in
  try
    let channel = open_out path in
    output_string channel dot ;
    close_out channel ;
    Ok ()
  with Sys_error message -> Error message
