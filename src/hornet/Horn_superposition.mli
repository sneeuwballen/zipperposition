
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(* {1 Superposition on Horn Clauses} *)

(** This module contains the main first-order reasoning engine.
    It works incrementally, by saturating a growing set of Horn Clauses
    up to some pre-defined limit. *)

module Make : State.THEORY_FUN

val theory : State.theory_fun

