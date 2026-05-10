let usage_lines =
  [
    "Usage:";
    "  ocaml-autodiff diff \"<expr>\" \"x=1,y=2\"";
    "  ocaml-autodiff forward-diff \"<expr>\" \"x=1,y=2\"";
    "  ocaml-autodiff train [epochs] [learning_rate] [csv_path]";
    "  ocaml-autodiff check-grad \"<expr>\" \"x=1\" x [eps] [abs_tol]";
    "  ocaml-autodiff export-dot \"<expr>\" [path.dot]";
    "  ocaml-autodiff train-adam [epochs] [csv_path]";
    "";
    "Examples:";
    "  ocaml-autodiff diff \"x*x + 3*x + 1\" \"x=2\"";
    "  ocaml-autodiff forward-diff \"x*x + 3*x + 1\" \"x=2\"";
    "  ocaml-autodiff train 300 0.05";
    "  ocaml-autodiff train 300 0.05 data.csv";
    "  ocaml-autodiff train-adam 1000";
    "  ocaml-autodiff train-adam 1000 data.csv";
    "  ocaml-autodiff check-grad \"x*x\" \"x=2\" x 1e-6 1e-4";
    "  ocaml-autodiff export-dot \"x*x + 1\" expr.dot";
  ]

let rec print_lines lines =
  match lines with
  | [] -> ()
  | line :: rest ->
      print_endline line ;
      print_lines rest

let print_usage () = print_lines usage_lines
let split_assignments text = String.split_on_char ',' text

let parse_float_value text =
  try Ok (float_of_string text)
  with Failure _ -> Error ("invalid float literal: " ^ text)

let parse_assignment assignment_text =
  let trimmed = String.trim assignment_text in
  if trimmed = "" then Error "empty assignment in environment"
  else
    match String.split_on_char '=' trimmed with
    | [ name; value_text ] -> (
        let var_name = String.trim name in
        let var_value_text = String.trim value_text in
        if var_name = "" then Error "empty variable name in assignment"
        else
          match parse_float_value var_value_text with
          | Error message -> Error message
          | Ok value -> Ok (var_name, value))
    | _ ->
        Error
          ("invalid assignment format: " ^ assignment_text
         ^ " (expected name=value)")

let rec parse_assignments assignment_texts acc =
  match assignment_texts with
  | [] -> Ok (List.rev acc)
  | item :: rest -> (
      match parse_assignment item with
      | Error message -> Error message
      | Ok pair -> parse_assignments rest (pair :: acc))

let parse_env text =
  if String.trim text = "" then Ok []
  else
    let assignment_texts = split_assignments text in
    parse_assignments assignment_texts []

let print_gradients gradients =
  let rec loop pairs =
    match pairs with
    | [] -> ()
    | (name, value) :: rest ->
        Printf.printf "  d/d%s = %.8f\n" name value ;
        loop rest
  in
  loop gradients

let run_diff expr_text env_text =
  match Ocaml_autodiff.Expr.parse expr_text with
  | Error message ->
      prerr_endline ("Parse error: " ^ message) ;
      1
  | Ok expr -> (
      match parse_env env_text with
      | Error message ->
          prerr_endline ("Environment error: " ^ message) ;
          1
      | Ok env -> (
          match Ocaml_autodiff.Reverse_ad.gradient expr env with
          | Error message ->
              prerr_endline ("Autodiff error: " ^ message) ;
              1
          | Ok (value, gradients) ->
              Printf.printf "Expression: %s\n"
                (Ocaml_autodiff.Expr.to_string expr) ;
              Printf.printf "Value: %.8f\n" value ;
              print_endline "Gradients:" ;
              print_gradients gradients ;
              0))

let run_forward_diff expr_text env_text =
  match Ocaml_autodiff.Expr.parse expr_text with
  | Error message ->
      prerr_endline ("Parse error: " ^ message) ;
      1
  | Ok expr -> (
      match parse_env env_text with
      | Error message ->
          prerr_endline ("Environment error: " ^ message) ;
          1
      | Ok env -> (
          match Ocaml_autodiff.Forward_ad.gradient_expr expr env with
          | Error message ->
              prerr_endline ("Autodiff error: " ^ message) ;
              1
          | Ok (value, gradients) ->
              Printf.printf "Expression: %s\n"
                (Ocaml_autodiff.Expr.to_string expr) ;
              Printf.printf "Value: %.8f\n" value ;
              print_endline "Gradients (Forward-Mode):" ;
              print_gradients gradients ;
              0))

let parse_int text =
  try Ok (int_of_string text)
  with Failure _ -> Error ("invalid integer: " ^ text)

let maybe_head list default =
  match list with [] -> default | head :: _ -> head

let second_or_default list default =
  match list with _first :: second :: _ -> second | _ -> default

let third_or_default list default =
  match list with _ :: _ :: third :: _ -> third | _ -> default

let load_samples csv_path =
  if csv_path = "" then Ok Ocaml_autodiff.Trainer.demo_samples
  else Ocaml_autodiff.Trainer.load_csv_samples csv_path

let last_or_default list default =
  let rec loop items current =
    match items with [] -> current | value :: rest -> loop rest value
  in
  loop list default

let run_train args =
  let epochs_text = maybe_head args "200" in
  let lr_text = second_or_default args "0.05" in
  let csv_path = third_or_default args "" in
  match parse_int epochs_text with
  | Error message ->
      prerr_endline ("Train argument error: invalid epochs " ^ message) ;
      prerr_endline "Usage: train [epochs] [learning_rate] [csv_path]" ;
      1
  | Ok epochs -> (
      match parse_float_value lr_text with
      | Error _ ->
          Printf.eprintf
            "Train argument error: invalid learning_rate %S (expected a \
             positive float)\n"
            lr_text ;
          prerr_endline "Usage: train [epochs] [learning_rate] [csv_path]" ;
          1
      | Ok learning_rate -> (
          match load_samples csv_path with
          | Error message ->
              prerr_endline ("CSV error: " ^ message) ;
              1
          | Ok samples -> (
              let initial_loss =
                Ocaml_autodiff.Trainer.mse_loss
                  Ocaml_autodiff.Trainer.default_model samples
              in
              match
                Ocaml_autodiff.Trainer.train_linear ~epochs ~learning_rate
                  Ocaml_autodiff.Trainer.default_model samples
              with
              | Error message ->
                  prerr_endline ("Training error: " ^ message) ;
                  1
              | Ok (model, history) ->
                  let final_loss = last_or_default history initial_loss in
                  Printf.printf "Training completed in %d epochs\n" epochs ;
                  Printf.printf "Initial loss: %.8f\n" initial_loss ;
                  Printf.printf "Final loss:   %.8f\n" final_loss ;
                  Printf.printf "Learned model: y = %.8f * x + %.8f\n" model.w
                    model.b ;
                  0)))

let run_check_grad args =
  match args with
  | [] | [ _ ] | [ _; _ ] ->
      prerr_endline
        "check-grad expects at least: <expr> <env> <var_name> [eps] [abs_tol]" ;
      prerr_endline
        "  expr     : arithmetic expression, e.g. \"x * x + sin(x)\"" ;
      prerr_endline
        "  env      : comma-separated variable bindings, e.g. \"x=1.0,y=2.0\"" ;
      prerr_endline "  var_name : variable to differentiate with respect to" ;
      prerr_endline "  eps      : finite-difference step size (default: 1e-5)" ;
      prerr_endline "  abs_tol  : absolute tolerance (default: 1e-6)" ;
      1
  | expr_text :: env_text :: var_name :: rest -> (
      let eps_text = maybe_head rest "1e-5" in
      let abs_tol_text = second_or_default rest "1e-6" in
      match parse_env env_text with
      | Error message ->
          prerr_endline ("check-grad argument error: " ^ message) ;
          prerr_endline
            "  env must be comma-separated name=value pairs, e.g. \
             \"x=1.0,y=2.0\"" ;
          1
      | Ok env -> (
          match parse_float_value eps_text with
          | Error _ ->
              Printf.eprintf
                "check-grad argument error: invalid eps %S (expected a \
                 positive float)\n"
                eps_text ;
              1
          | Ok eps -> (
              match parse_float_value abs_tol_text with
              | Error _ ->
                  Printf.eprintf
                    "check-grad argument error: invalid abs_tol %S (expected a \
                     positive float)\n"
                    abs_tol_text ;
                  1
              | Ok abs_tol -> (
                  match Ocaml_autodiff.Expr.parse expr_text with
                  | Error message ->
                      prerr_endline ("check-grad parse error: " ^ message) ;
                      1
                  | Ok expr -> (
                      match
                        Ocaml_autodiff.Grad_check.check_expr_gradient expr env
                          var_name eps abs_tol
                      with
                      | Error message ->
                          prerr_endline ("check-grad error: " ^ message) ;
                          1
                      | Ok passed ->
                          Printf.printf "Expression : %s\n" expr_text ;
                          Printf.printf "Variable   : %s\n" var_name ;
                          Printf.printf "Point      : %.8g\n"
                            (match List.assoc_opt var_name env with
                            | Some v -> v
                            | None -> Float.nan) ;
                          Printf.printf "Eps        : %.2e\n" eps ;
                          Printf.printf "Abs tol    : %.2e\n" abs_tol ;
                          Printf.printf "Result     : %s\n"
                            (if passed then "PASS" else "FAIL") ;
                          if passed then 0 else 1)))))

let run_export_dot args =
  match args with
  | expr_text :: rest -> (
      let output_path =
        match rest with [] -> "expression.dot" | first :: _ -> first
      in
      match Ocaml_autodiff.Expr.parse expr_text with
      | Error message ->
          prerr_endline ("Parse error: " ^ message) ;
          1
      | Ok expr -> (
          match
            Ocaml_autodiff.Graphviz_export.write_expr_dot output_path expr
          with
          | Error message ->
              prerr_endline ("DOT export error: " ^ message) ;
              1
          | Ok () ->
              Printf.printf "Wrote DOT scaffold to %s\n" output_path ;
              0))
  | _ ->
      prerr_endline "export-dot expects: <expr> [path.dot]" ;
      1

let run_train_adam args =
  let epochs_text = maybe_head args "200" in
  let csv_path = second_or_default args "" in
  match parse_int epochs_text with
  | Error message ->
      prerr_endline ("Train-adam argument error: " ^ message) ;
      1
  | Ok epochs -> (
      match load_samples csv_path with
      | Error message ->
          prerr_endline ("CSV error: " ^ message) ;
          1
      | Ok samples -> (
          let initial_loss =
            Ocaml_autodiff.Trainer.mse_loss Ocaml_autodiff.Trainer.default_model
              samples
          in
          match
            Ocaml_autodiff.Trainer.train_linear_adam ~epochs
              ~config:Ocaml_autodiff.Trainer.default_adam_config
              Ocaml_autodiff.Trainer.default_model samples
          with
          | Error message ->
              prerr_endline ("Training error: " ^ message) ;
              1
          | Ok (model, history) ->
              let final_loss = last_or_default history initial_loss in
              Printf.printf "Training completed in %d epochs (Adam)\n" epochs ;
              Printf.printf "Initial loss: %.8f\n" initial_loss ;
              Printf.printf "Final loss:   %.8f\n" final_loss ;
              Printf.printf "Learned model: y = %.8f * x + %.8f\n" model.w
                model.b ;
              0))

let run_command argv =
  if Array.length argv < 2 then (
    print_usage () ;
    0)
  else
    let command = argv.(1) in
    if command = "diff" then
      if Array.length argv <> 4 then (
        prerr_endline "diff command expects exactly 2 arguments" ;
        print_usage () ;
        1)
      else run_diff argv.(2) argv.(3)
    else if command = "forward-diff" then
      if Array.length argv <> 4 then (
        prerr_endline "forward-diff command expects exactly 2 arguments" ;
        print_usage () ;
        1)
      else run_forward_diff argv.(2) argv.(3)
    else if command = "train" then
      let args =
        if Array.length argv <= 2 then []
        else Array.to_list (Array.sub argv 2 (Array.length argv - 2))
      in
      run_train args
    else if command = "check-grad" then
      let args =
        if Array.length argv <= 2 then []
        else Array.to_list (Array.sub argv 2 (Array.length argv - 2))
      in
      run_check_grad args
    else if command = "export-dot" then
      let args =
        if Array.length argv <= 2 then []
        else Array.to_list (Array.sub argv 2 (Array.length argv - 2))
      in
      run_export_dot args
    else if command = "train-adam" then
      let args =
        if Array.length argv <= 2 then []
        else Array.to_list (Array.sub argv 2 (Array.length argv - 2))
      in
      run_train_adam args
    else if command = "help" then (
      print_usage () ;
      0)
    else (
      prerr_endline ("unknown command: " ^ command) ;
      print_usage () ;
      1)

let () = exit (run_command Sys.argv)
