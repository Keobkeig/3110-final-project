let usage_lines =
  [
    "Usage:";
    "  ocaml-autodiff diff \"<expr>\" \"x=1,y=2\"";
    "  ocaml-autodiff train [epochs] [learning_rate]";
    "";
    "Examples:";
    "  ocaml-autodiff diff \"x*x + 3*x + 1\" \"x=2\"";
    "  ocaml-autodiff train 300 0.05";
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

let parse_int text =
  try Ok (int_of_string text)
  with Failure _ -> Error ("invalid integer: " ^ text)

let maybe_head list default =
  match list with [] -> default | head :: _ -> head

let second_or_default list default =
  match list with _first :: second :: _ -> second | _ -> default

let last_or_default list default =
  let rec loop items current =
    match items with [] -> current | value :: rest -> loop rest value
  in
  loop list default

let run_train args =
  let epochs_text = maybe_head args "200" in
  let lr_text = second_or_default args "0.05" in
  match parse_int epochs_text with
  | Error message ->
      prerr_endline ("Train argument error: " ^ message) ;
      1
  | Ok epochs -> (
      match parse_float_value lr_text with
      | Error message ->
          prerr_endline ("Train argument error: " ^ message) ;
          1
      | Ok learning_rate -> (
          let initial_loss =
            Ocaml_autodiff.Trainer.mse_loss Ocaml_autodiff.Trainer.default_model
              Ocaml_autodiff.Trainer.demo_samples
          in
          match
            Ocaml_autodiff.Trainer.train_linear ~epochs ~learning_rate
              Ocaml_autodiff.Trainer.default_model
              Ocaml_autodiff.Trainer.demo_samples
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
    else if command = "train" then
      let args =
        if Array.length argv <= 2 then []
        else Array.to_list (Array.sub argv 2 (Array.length argv - 2))
      in
      run_train args
    else if command = "help" then (
      print_usage () ;
      0)
    else (
      prerr_endline ("unknown command: " ^ command) ;
      print_usage () ;
      1)

let () = exit (run_command Sys.argv)
