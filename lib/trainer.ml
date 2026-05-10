type sample = { x : float; y : float }
type model = { w : float; b : float }

type adam_config = {
  learning_rate : float;
  beta1 : float;
  beta2 : float;
  epsilon : float;
}

let default_adam_config =
  { learning_rate = 0.01; beta1 = 0.9; beta2 = 0.999; epsilon = 1e-8 }

let predict model x = (model.w *. x) +. model.b
let sqr x = x *. x

let rec sum_losses model samples total count =
  match samples with
  | [] -> if count = 0 then 0.0 else total /. float_of_int count
  | sample :: rest ->
      let prediction = predict model sample.x in
      let residual = prediction -. sample.y in
      let loss = sqr residual in
      sum_losses model rest (total +. loss) (count + 1)

let mse_loss model samples = sum_losses model samples 0.0 0

let loss_expr =
  Expr.Mul
    ( Expr.Sub
        ( Expr.Add (Expr.Mul (Expr.Var "w", Expr.Var "x"), Expr.Var "b"),
          Expr.Var "y" ),
      Expr.Sub
        ( Expr.Add (Expr.Mul (Expr.Var "w", Expr.Var "x"), Expr.Var "b"),
          Expr.Var "y" ) )

let sample_env model sample =
  [ ("w", model.w); ("b", model.b); ("x", sample.x); ("y", sample.y) ]

let extract_grad grads name =
  match List.assoc_opt name grads with None -> 0.0 | Some value -> value

let rec accumulate_sample_grads model samples sum_w sum_b sample_count =
  match samples with
  | [] ->
      if sample_count = 0 then Error "cannot train on empty dataset"
      else
        let denom = float_of_int sample_count in
        Ok (sum_w /. denom, sum_b /. denom)
  | sample :: rest -> (
      let env = sample_env model sample in
      match Reverse_ad.gradient loss_expr env with
      | Error message -> Error message
      | Ok (_, grads) ->
          let grad_w = extract_grad grads "w" in
          let grad_b = extract_grad grads "b" in
          accumulate_sample_grads model rest (sum_w +. grad_w) (sum_b +. grad_b)
            (sample_count + 1))

let step_model model learning_rate avg_grad_w avg_grad_b =
  {
    w = model.w -. (learning_rate *. avg_grad_w);
    b = model.b -. (learning_rate *. avg_grad_b);
  }

let rec train_epochs epochs learning_rate samples current_model history =
  if epochs <= 0 then Ok (current_model, List.rev history)
  else
    match accumulate_sample_grads current_model samples 0.0 0.0 0 with
    | Error message -> Error message
    | Ok (avg_grad_w, avg_grad_b) ->
        let updated_model =
          step_model current_model learning_rate avg_grad_w avg_grad_b
        in
        let loss = mse_loss updated_model samples in
        train_epochs (epochs - 1) learning_rate samples updated_model
          (loss :: history)

let train_linear ~epochs ~learning_rate init_model samples =
  if epochs < 0 then Error "epochs must be non-negative"
  else if learning_rate <= 0.0 then Error "learning_rate must be positive"
  else train_epochs epochs learning_rate samples init_model []

let demo_samples =
  [
    { x = 0.0; y = 1.0 };
    { x = 1.0; y = 3.0 };
    { x = 2.0; y = 5.0 };
    { x = 3.0; y = 7.0 };
  ]

let default_model = { w = 0.0; b = 0.0 }

let adam_step ~config ~t ~grad ~m ~v ~param =
  let m' = (config.beta1 *. m) +. ((1.0 -. config.beta1) *. grad) in
  let v' = (config.beta2 *. v) +. ((1.0 -. config.beta2) *. grad *. grad) in
  let bc1 = 1.0 -. (config.beta1 ** float_of_int t) in
  let bc2 = 1.0 -. (config.beta2 ** float_of_int t) in
  let m_hat = m' /. bc1 in
  let v_hat = v' /. bc2 in
  let param' =
    param -. (config.learning_rate *. m_hat /. (sqrt v_hat +. config.epsilon))
  in
  (param', m', v')

let rec train_adam_epochs epochs config samples model m_w v_w m_b v_b t history
    =
  if epochs <= 0 then Ok (model, List.rev history)
  else
    match accumulate_sample_grads model samples 0.0 0.0 0 with
    | Error message -> Error message
    | Ok (avg_grad_w, avg_grad_b) ->
        let new_w, m_w', v_w' =
          adam_step ~config ~t ~grad:avg_grad_w ~m:m_w ~v:v_w ~param:model.w
        in
        let new_b, m_b', v_b' =
          adam_step ~config ~t ~grad:avg_grad_b ~m:m_b ~v:v_b ~param:model.b
        in
        let updated_model = { w = new_w; b = new_b } in
        let loss = mse_loss updated_model samples in
        train_adam_epochs (epochs - 1) config samples updated_model m_w' v_w'
          m_b' v_b' (t + 1) (loss :: history)

let train_linear_adam ~epochs ~config init_model samples =
  if epochs < 0 then Error "epochs must be non-negative"
  else if config.learning_rate <= 0.0 then
    Error "learning_rate must be positive"
  else if config.beta1 <= 0.0 || config.beta1 >= 1.0 then
    Error "beta1 must be in (0, 1)"
  else if config.beta2 <= 0.0 || config.beta2 >= 1.0 then
    Error "beta2 must be in (0, 1)"
  else if config.epsilon <= 0.0 then Error "epsilon must be positive"
  else train_adam_epochs epochs config samples init_model 0.0 0.0 0.0 0.0 1 []

let parse_csv_line line =
  match String.split_on_char ',' line with
  | [ x_text; y_text ] -> (
      let x_trim = String.trim x_text in
      let y_trim = String.trim y_text in
      try Ok { x = float_of_string x_trim; y = float_of_string y_trim }
      with Failure _ -> Error ("invalid number in line: " ^ line))
  | _ -> Error ("expected two columns (x,y) in line: " ^ line)

let rec read_csv_lines channel acc =
  match try Some (input_line channel) with End_of_file -> None with
  | None -> Ok (List.rev acc)
  | Some line -> (
      let trimmed = String.trim line in
      if trimmed = "" then read_csv_lines channel acc
      else
        match parse_csv_line trimmed with
        | Error message -> Error message
        | Ok sample -> read_csv_lines channel (sample :: acc))

let load_csv_samples path =
  match try Ok (open_in path) with Sys_error message -> Error message with
  | Error message -> Error message
  | Ok channel -> (
      let result = read_csv_lines channel [] in
      close_in channel ;
      match result with
      | Error message -> Error message
      | Ok [] -> Error "csv file contains no samples"
      | Ok samples -> Ok samples)
