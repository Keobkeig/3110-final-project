open OUnit2
open Ocaml_autodiff

let assert_float_equal ?(eps = 1e-6) expected actual =
  let float_close left right = Float.abs (left -. right) <= eps in
  assert_equal expected actual ~printer:string_of_float ~cmp:float_close

let assert_result_ok_float result expected =
  match result with
  | Error message -> assert_failure ("expected Ok but got Error: " ^ message)
  | Ok value -> assert_float_equal expected value

let assert_result_ok_gradient result expected_value expected_grads =
  match result with
  | Error message -> assert_failure ("expected Ok but got Error: " ^ message)
  | Ok (value, grads) ->
      assert_float_equal expected_value value ;
      let rec check_expected items =
        match items with
        | [] -> ()
        | (name, expected_grad) :: rest ->
            let actual =
              match List.assoc_opt name grads with
              | None -> assert_failure ("missing gradient for variable " ^ name)
              | Some g -> g
            in
            assert_float_equal expected_grad actual ;
            check_expected rest
      in
      check_expected expected_grads

let assert_result_error result expected_prefix =
  match result with
  | Ok _ -> assert_failure "expected Error but got Ok"
  | Error message ->
      let starts_with =
        String.length message >= String.length expected_prefix
        && String.sub message 0 (String.length expected_prefix)
           = expected_prefix
      in
      assert_bool
        ("expected error prefix " ^ expected_prefix ^ ", got: " ^ message)
        starts_with

let parse_test_valid _ =
  match Expr.parse "x * x + 3 * x + 1" with
  | Error message -> assert_failure ("parse unexpectedly failed: " ^ message)
  | Ok expr ->
      assert_equal [ "x" ] (Expr.vars expr) ~printer:(String.concat ",")

let parse_test_invalid _ =
  let result = Expr.parse "x + * 2" in
  assert_result_error result "unexpected token"

let eval_test_simple _ =
  let expr = Expr.Add (Expr.Mul (Expr.Var "x", Expr.Var "x"), Expr.Const 1.0) in
  let result = Reverse_ad.eval expr [ ("x", 3.0) ] in
  assert_result_ok_float result 10.0

let gradient_test_polynomial _ =
  let expr = Expr.Add (Expr.Mul (Expr.Var "x", Expr.Var "x"), Expr.Var "x") in
  let result = Reverse_ad.gradient expr [ ("x", 2.0) ] in
  assert_result_ok_gradient result 6.0 [ ("x", 5.0) ]

let gradient_test_multivar _ =
  let expr =
    Expr.Add (Expr.Mul (Expr.Var "x", Expr.Var "y"), Expr.Sin (Expr.Var "x"))
  in
  let x = 1.2 in
  let y = 0.5 in
  let expected_value = (x *. y) +. sin x in
  let expected_dx = y +. cos x in
  let expected_dy = x in
  let result = Reverse_ad.gradient expr [ ("x", x); ("y", y) ] in
  assert_result_ok_gradient result expected_value
    [ ("x", expected_dx); ("y", expected_dy) ]

let gradient_test_log_domain_error _ =
  let expr = Expr.Log (Expr.Var "x") in
  let result = Reverse_ad.gradient expr [ ("x", 0.0) ] in
  assert_result_error result "log undefined"

let gradient_test_missing_var_error _ =
  let expr = Expr.Add (Expr.Var "x", Expr.Var "y") in
  let result = Reverse_ad.gradient expr [ ("x", 1.0) ] in
  assert_result_error result "missing variable assignment"

let trainer_test_loss_decreases _ =
  let initial_loss =
    Trainer.mse_loss Trainer.default_model Trainer.demo_samples
  in
  match
    Trainer.train_linear ~epochs:300 ~learning_rate:0.05 Trainer.default_model
      Trainer.demo_samples
  with
  | Error message -> assert_failure ("training failed: " ^ message)
  | Ok (model, history) ->
      let final_loss =
        match List.rev history with [] -> initial_loss | head :: _ -> head
      in
      assert_bool
        ("expected final loss < initial loss, got initial="
        ^ string_of_float initial_loss
        ^ " final=" ^ string_of_float final_loss)
        (final_loss < initial_loss) ;
      assert_float_equal ~eps:0.2 2.0 model.w ;
      assert_float_equal ~eps:0.2 1.0 model.b

let trainer_test_bad_inputs _ =
  let bad_epochs =
    Trainer.train_linear ~epochs:(-1) ~learning_rate:0.05 Trainer.default_model
      Trainer.demo_samples
  in
  let bad_lr =
    Trainer.train_linear ~epochs:10 ~learning_rate:0.0 Trainer.default_model
      Trainer.demo_samples
  in
  assert_result_error bad_epochs "epochs must be non-negative" ;
  assert_result_error bad_lr "learning_rate must be positive"

let suite =
  "ocaml_autodiff sprint-a tests"
  >::: [
         "parse valid" >:: parse_test_valid;
         "parse invalid" >:: parse_test_invalid;
         "eval simple" >:: eval_test_simple;
         "gradient polynomial" >:: gradient_test_polynomial;
         "gradient multivar" >:: gradient_test_multivar;
         "gradient log domain error" >:: gradient_test_log_domain_error;
         "gradient missing var error" >:: gradient_test_missing_var_error;
         "trainer loss decreases" >:: trainer_test_loss_decreases;
         "trainer bad inputs" >:: trainer_test_bad_inputs;
       ]

let () = run_test_tt_main suite
