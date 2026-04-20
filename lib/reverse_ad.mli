(** Reverse-mode automatic differentiation over scalar expressions. *)

val eval : Expr.t -> (string * float) list -> (float, string) result
(** [eval expr env] evaluates [expr] in environment [env]. Returns [Ok value] on
    success and [Error msg] on failure. *)

(** AF: A [gradient] result [(value, grads)] denotes the value of the expression
    and partial derivatives with respect to all variables in [Expr.vars expr].

    RI: [grads] contains at most one pair per variable name and uses the
    first-occurrence ordering from [Expr.vars]. *)
val gradient :
  Expr.t ->
  (string * float) list ->
  (float * (string * float) list, string) result
(** [gradient expr env] computes gradients of [expr] with respect to all
    variables appearing in [expr]. Returns [(value, grads)] where [value] is the
    expression value and [grads] maps variable names to partial derivatives.
    Returns [Error msg] on failure. *)
