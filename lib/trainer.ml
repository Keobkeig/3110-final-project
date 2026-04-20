type sample = { x : float; y : float }
type model = { w : float; b : float }

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
