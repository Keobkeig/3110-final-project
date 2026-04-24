open OUnit2
open Ocaml_autodiff.Grad_check

let eps = 1e-5
let abs_tol = 1e-6

let parse s =
  match Ocaml_autodiff.Expr.parse s with
  | Ok e -> e
  | Error m -> assert_failure ("Parse error in " ^ s ^ ": " ^ m)

let assert_pass result =
  match result with
  | Error msg -> assert_failure ("Expected Ok true, got Error: " ^ msg)
  | Ok false -> assert_failure "Expected Ok true, got Ok false"
  | Ok true -> ()

let test_cd_linear _ =
  let f x = (3.0 *. x) +. 7.0 in
  let g = central_difference f 0.0 eps in
  assert_bool "linear slope should be 3.0" (Float.abs (g -. 3.0) < abs_tol)

let test_cd_quadratic _ =
  let f x = x *. x in
  let g = central_difference f 4.0 eps in
  assert_bool "quadratic slope at x=4 should be 8.0"
    (Float.abs (g -. 8.0) < abs_tol)

let test_cd_sin _ =
  let g = central_difference sin 0.0 eps in
  assert_bool "sin slope at x=0 should be 1.0" (Float.abs (g -. 1.0) < abs_tol)

let test_cd_exp _ =
  let g = central_difference exp 1.0 eps in
  assert_bool "exp slope at x=1 should be e" (Float.abs (g -. exp 1.0) < abs_tol)

let test_cd_cubic_negative _ =
  let f x = x *. x *. x in
  let g = central_difference f (-2.0) eps in
  assert_bool "cubic slope at x=-2 should be 12.0"
    (Float.abs (g -. 12.0) < abs_tol)

let test_cd_constant _ =
  let g = central_difference (fun _ -> 5.0) 99.0 eps in
  assert_bool "constant slope should be 0.0" (Float.abs g < abs_tol)

let test_cd_negative_eps _ =
  assert_raises (Failure "eps must be positive in central_difference")
    (fun () -> central_difference Fun.id 1.0 (-1e-5))

let test_cd_zero_eps _ =
  assert_raises (Failure "eps must be positive in central_difference")
    (fun () -> central_difference Fun.id 1.0 0.0)

let suite_central_difference =
  "central_difference"
  >::: [
         "linear" >:: test_cd_linear;
         "quadratic" >:: test_cd_quadratic;
         "sin" >:: test_cd_sin;
         "exp" >:: test_cd_exp;
         "cubic_negative" >:: test_cd_cubic_negative;
         "constant" >:: test_cd_constant;
         "negative_eps" >:: test_cd_negative_eps;
         "zero_eps" >:: test_cd_zero_eps;
       ]

(*gradient error cases*)
let test_ge_exact _ =
  assert_bool "identical values"
    (gradient_error ~abs_tol:1e-6 ~expected:3.14 ~actual:3.14)

let test_ge_within_tol _ =
  assert_bool "difference 1e-7 within tol 1e-6"
    (gradient_error ~abs_tol:1e-6 ~expected:1.0 ~actual:(1.0 +. 1e-7))

let test_ge_at_boundary _ =
  assert_bool "difference exactly equal to tol should pass"
    (gradient_error ~abs_tol:1e-6 ~expected:0.0 ~actual:1e-6)

let test_ge_outside_tol _ =
  assert_bool "difference 1e-4 outside tol 1e-6 should fail"
    (not (gradient_error ~abs_tol:1e-6 ~expected:1.0 ~actual:(1.0 +. 1e-4)))

let test_ge_negative_diff _ =
  assert_bool "negative difference within tol should pass"
    (gradient_error ~abs_tol:1e-6 ~expected:1.0 ~actual:(1.0 -. 1e-7))

let test_ge_zero_tol_exact _ =
  assert_bool "zero tol, exact match"
    (gradient_error ~abs_tol:0.0 ~expected:2.0 ~actual:2.0)

let suite_gradient_error =
  "gradient_error"
  >::: [
         "exact" >:: test_ge_exact;
         "within_tol" >:: test_ge_within_tol;
         "at_boundary" >:: test_ge_at_boundary;
         "outside_tol" >:: test_ge_outside_tol;
         "negative_diff" >:: test_ge_negative_diff;
         "zero_tol_exact" >:: test_ge_zero_tol_exact;
       ]

let test_ceg_identity _ =
  assert_pass (check_expr_gradient (parse "x") [ ("x", 3.0) ] "x" eps abs_tol)

let test_ceg_constant _ =
  assert_pass (check_expr_gradient (parse "42") [ ("x", 1.0) ] "x" eps abs_tol)

let test_ceg_linear _ =
  assert_pass
    (check_expr_gradient (parse "3 * x + 7") [ ("x", 5.0) ] "x" eps abs_tol)

let test_ceg_quadratic _ =
  assert_pass
    (check_expr_gradient (parse "x * x") [ ("x", 4.0) ] "x" eps abs_tol)

let test_ceg_cubic _ =
  assert_pass
    (check_expr_gradient (parse "x * x * x") [ ("x", 2.0) ] "x" eps abs_tol)

let test_ceg_quadratic_at_zero _ =
  assert_pass
    (check_expr_gradient (parse "x * x") [ ("x", 0.0) ] "x" eps abs_tol)

let test_ceg_quadratic_negative _ =
  assert_pass
    (check_expr_gradient (parse "x * x") [ ("x", -3.0) ] "x" eps abs_tol)

let test_ceg_sin _ =
  assert_pass
    (check_expr_gradient (parse "sin(x)") [ ("x", 0.0) ] "x" eps abs_tol)

let test_ceg_cos _ =
  assert_pass
    (check_expr_gradient (parse "cos(x)")
       [ ("x", Float.pi /. 2.0) ]
       "x" eps abs_tol)

let test_ceg_exp _ =
  assert_pass
    (check_expr_gradient (parse "exp(x)") [ ("x", 1.0) ] "x" eps abs_tol)

let test_ceg_log _ =
  assert_pass
    (check_expr_gradient (parse "log(x)") [ ("x", 2.0) ] "x" eps abs_tol)

let test_ceg_sin_squared _ =
  assert_pass
    (check_expr_gradient (parse "sin(x) * sin(x)")
       [ ("x", 1.0) ]
       "x" eps abs_tol)

let test_ceg_log_of_exp _ =
  assert_pass
    (check_expr_gradient (parse "log(exp(x))") [ ("x", 2.0) ] "x" eps abs_tol)

let test_ceg_nested_sin _ =
  assert_pass
    (check_expr_gradient (parse "sin(sin(x))") [ ("x", 0.5) ] "x" eps abs_tol)

let test_ceg_exp_neg_x_sq _ =
  assert_pass
    (check_expr_gradient (parse "exp(0 - x * x)")
       [ ("x", 1.0) ]
       "x" eps abs_tol)

let test_ceg_product_wrt_x _ =
  assert_pass
    (check_expr_gradient (parse "x * y")
       [ ("x", 2.0); ("y", 3.0) ]
       "x" eps abs_tol)

let test_ceg_product_wrt_y _ =
  assert_pass
    (check_expr_gradient (parse "x * y")
       [ ("x", 2.0); ("y", 3.0) ]
       "y" eps abs_tol)

let test_ceg_sum_wrt_x _ =
  assert_pass
    (check_expr_gradient (parse "x + y + z")
       [ ("x", 1.0); ("y", 2.0); ("z", 3.0) ]
       "x" eps abs_tol)

let test_ceg_independent_var _ =
  (* x does not appear; grad defaults to 0.0 *)
  assert_pass
    (check_expr_gradient (parse "y * y")
       [ ("x", 5.0); ("y", 3.0) ]
       "x" eps abs_tol)

let test_ceg_small_x _ =
  assert_pass
    (check_expr_gradient (parse "x * x") [ ("x", 1e-4) ] "x" eps abs_tol)

let test_ceg_large_x _ =
  assert_pass (check_expr_gradient (parse "x * x") [ ("x", 1e6) ] "x" eps 20.0)

let suite_check_expr_gradient =
  "check_expr_gradient"
  >::: [
         "identity" >:: test_ceg_identity;
         "constant" >:: test_ceg_constant;
         "linear" >:: test_ceg_linear;
         "quadratic" >:: test_ceg_quadratic;
         "cubic" >:: test_ceg_cubic;
         "quadratic_at_zero" >:: test_ceg_quadratic_at_zero;
         "quadratic_negative" >:: test_ceg_quadratic_negative;
         "sin" >:: test_ceg_sin;
         "cos" >:: test_ceg_cos;
         "exp" >:: test_ceg_exp;
         "log" >:: test_ceg_log;
         "sin_squared" >:: test_ceg_sin_squared;
         "log_of_exp" >:: test_ceg_log_of_exp;
         "nested_sin" >:: test_ceg_nested_sin;
         "exp_neg_x_sq" >:: test_ceg_exp_neg_x_sq;
         "product_wrt_x" >:: test_ceg_product_wrt_x;
         "product_wrt_y" >:: test_ceg_product_wrt_y;
         "sum_wrt_x" >:: test_ceg_sum_wrt_x;
         "independent_var" >:: test_ceg_independent_var;
         "small_x" >:: test_ceg_small_x;
         "large_x" >:: test_ceg_large_x;
       ]

let () =
  run_test_tt_main
    ("grad_check"
    >::: [
           suite_central_difference;
           suite_gradient_error;
           suite_check_expr_gradient;
         ])

