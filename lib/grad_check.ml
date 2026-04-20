let central_difference f x eps =
  if eps <= 0.0 then failwith "eps must be positive in central_difference"
  else
    let fx_plus = f (x +. eps) in
    let fx_minus = f (x -. eps) in
    (fx_plus -. fx_minus) /. (2.0 *. eps)

let gradient_error ~abs_tol ~expected ~actual =
  Float.abs (expected -. actual) <= abs_tol

let check_expr_gradient _expr _env _var_name _eps _abs_tol =
  Error "expression gradient checker is not implemented yet; see Sprint-B TODO"
