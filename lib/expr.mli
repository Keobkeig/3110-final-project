(** A mathematical expression syntax tree for scalar formulas. *)

(** AF: A value of type [t] is a syntax tree for a scalar mathematical
    expression. Constructors map directly to arithmetic or transcendental
    operations.

    RI: Expression trees are finite and well-formed by construction. *)
type t =
  | Const of float
  | Var of string
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Neg of t
  | Exp of t
  | Log of t
  | Sin of t
  | Cos of t
  | Tanh of t

val parse : string -> (t, string) result
(** [parse input] parses [input] as an expression. Returns [Ok expr] on success
    and [Error msg] on failure. *)

val to_string : t -> string
(** [to_string expr] returns a parenthesized string representation of [expr]. *)

val vars : t -> string list
(** [vars expr] returns variable names in [expr] in first-occurrence order. *)
