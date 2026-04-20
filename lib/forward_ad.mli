(** Forward-mode automatic differentiation scaffolding. *)

type dual = { primal : float; tangent : float }
(** AF: A [dual] value stores a primal value and one tangent value. RI: [primal]
    and [tangent] are finite IEEE-754 floats in expected usage. *)

val make : float -> float -> dual
(** [make primal tangent] builds a dual number. *)

val constant : float -> dual
(** [constant x] is the dual number for a constant [x]. *)

val variable : float -> dual
(** [variable x] is the dual number for the active variable initialized at [x].
    Its tangent is [1.0]. *)

val value : dual -> float
(** [value d] returns the primal value of [d]. *)

val derivative : dual -> float
(** [derivative d] returns the tangent value of [d]. *)

val add : dual -> dual -> dual
(** Basic forward-mode operations. *)

val sub : dual -> dual -> dual
val mul : dual -> dual -> dual
val div : dual -> dual -> dual
val neg : dual -> dual
val exp : dual -> dual
val log : dual -> dual
val sin : dual -> dual
val cos : dual -> dual
val tanh : dual -> dual

val derivative_univariate : (dual -> dual) -> float -> float
(** [derivative_univariate f x] computes [d/dx f(x)] with forward mode. *)

val gradient_expr :
  Expr.t ->
  (string * float) list ->
  (float * (string * float) list, string) result
(** [gradient_expr expr env] is a Sprint-B placeholder for a forward-mode
    expression gradient API. Returns an error until implemented by the team. *)
