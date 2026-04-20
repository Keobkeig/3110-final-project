(** Core metadata for the Ocaml_autodiff library. *)

val greet : string -> string
(** [greet name] returns a greeting for [name]. *)

val version : string
(** [version] is the current project version string. *)

module Expr : module type of Expr
(** Expression parsing and utilities. *)

module Reverse_ad : module type of Reverse_ad
(** Reverse-mode automatic differentiation. *)

module Trainer : module type of Trainer
(** Gradient-based training utilities. *)
