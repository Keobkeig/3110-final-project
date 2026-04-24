let central_difference f x eps =
  if eps <= 0.0 then failwith "eps must be positive in central_difference"
  else
    let fx_plus = f (x +. eps) in
    let fx_minus = f (x -. eps) in
    (fx_plus -. fx_minus) /. (2.0 *. eps)

let gradient_error ~abs_tol ~expected ~actual =
  Float.abs (expected -. actual) <= abs_tol

let check_expr_gradient expr env var_name eps abs_tol =
  (* Build a scalar function of var_name alone, holding all other vars fixed, so
     we can pass it to central_difference. *)
  let f x =
    let env' = (var_name, x) :: List.filter (fun (k, _) -> k <> var_name) env in
    match Reverse_ad.eval expr env' with Ok v -> v | Error msg -> failwith msg
  in
  (* Look up the point at which to evaluate the numerical derivative. *)
  match List.assoc_opt var_name env with
  | None -> Error ("variable not found in environment: " ^ var_name)
  | Some x0 -> (
      (* Compute the reverse-mode gradient. *)
      match Reverse_ad.gradient expr env with
      | Error msg -> Error msg
      | Ok (_value, grads) -> (
          (* Look up the analytic partial for var_name; 0 if absent (var
             unused). *)
          let analytic =
            match List.assoc_opt var_name grads with Some g -> g | None -> 0.0
          in
          (* Compute the numerical estimate via central differences. *)
          match
            try Ok (central_difference f x0 eps) with Failure msg -> Error msg
          with
          | Error msg -> Error ("numerical evaluation failed: " ^ msg)
          | Ok numerical ->
              Ok (gradient_error ~abs_tol ~expected:numerical ~actual:analytic))
      )
