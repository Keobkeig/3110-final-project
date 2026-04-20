(** Small gradient-descent training utilities built on reverse-mode AD. *)

type sample = { x : float; y : float }
(** AF: A [sample] stores one supervised point for one-dimensional regression.
    RI: values are finite IEEE-754 floats. *)

type model = { w : float; b : float }
(** AF: A [model] represents [y = w * x + b]. RI: parameters are finite IEEE-754
    floats. *)

(** Linear model [y = w * x + b]. *)
val predict : model -> float -> float
(** [predict model x] computes [w * x + b]. *)

type adam_config = {
  learning_rate : float;
  beta1 : float;
  beta2 : float;
  epsilon : float;
}
(** Hyperparameters for Adam optimization. *)

val default_adam_config : adam_config
(** [default_adam_config] contains baseline Adam hyperparameters. *)

val mse_loss : model -> sample list -> float
(** [mse_loss model samples] computes mean squared error on [samples]. Returns
    [0.] for an empty sample list. *)

val train_linear :
  epochs:int ->
  learning_rate:float ->
  model ->
  sample list ->
  (model * float list, string) result
(** [train_linear ~epochs ~learning_rate init samples] trains a linear model
    with batch gradient descent. Returns [model, history], where [history] is
    one loss per epoch.

    Preconditions: [epochs >= 0] and [learning_rate > 0.0]. *)

val demo_samples : sample list
(** [demo_samples] is a small built-in dataset for live demos. *)

val default_model : model
(** [default_model] is the default initial model for demos. *)

val train_linear_adam :
  epochs:int ->
  config:adam_config ->
  model ->
  sample list ->
  (model * float list, string) result
(** [train_linear_adam ~epochs ~config init samples] is a Sprint-B placeholder
    for Adam-based training. Returns an error until implemented. *)

val load_csv_samples : string -> (sample list, string) result
(** [load_csv_samples path] is a Sprint-B placeholder for loading regression
    datasets from CSV files. Returns an error until implemented. *)
