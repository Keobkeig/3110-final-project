(** Graph export scaffolding for computation/expression visualization. *)

val expr_to_dot : Expr.t -> string
(** [expr_to_dot expr] returns a DOT graph string for the expression AST. This
    is a minimal scaffold output for Sprint-B and beyond. *)

val write_expr_dot : string -> Expr.t -> (unit, string) result
(** [write_expr_dot path expr] writes a DOT graph file for [expr] at [path]. *)
