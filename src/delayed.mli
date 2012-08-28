(*
zipperposition: a functional superposition prover for prototyping
copyright (c) 2012 simon cruanes

this is free software; you can redistribute it and/or
modify it under the terms of the gnu general public license
as published by the free software foundation; either version 2
of the license, or (at your option) any later version.

this is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  see the
gnu general public license for more details.

you should have received a copy of the gnu general public license
along with this program; if not, write to the free software
foundation, inc., 51 franklin street, fifth floor, boston, ma
02110-1301 usa.
*)

(** module for superposition with equivalence reasoning and delayed clausal form *)

open Types

(* connectives *)
val eq_symbol : symbol
val false_symbol : symbol
val true_symbol : symbol
val exists_symbol : symbol
val forall_symbol : symbol
val lambda_symbol : symbol
val not_symbol : symbol
val imply_symbol : symbol
val and_symbol : symbol
val or_symbol : symbol

(* De Bruijn *)
val db_symbol : symbol
val succ_db_symbol : symbol
val subst_symbol : symbol

(** check whether s is a binding symbol *)
val is_binder_symbol : symbol -> bool
(** symbols that are symmetric (that is, order of arguments does not matter) *)
val is_symmetric_symbol : symbol -> bool
(** infix symbols *)
val is_infix_symbol : symbol -> bool

(** nicer pretty printing *)
val pp_foterm : Format.formatter -> foterm -> unit
val pp_clause : Format.formatter -> clause -> unit

(** Substitution of De Bruijn symbol by a term. [db_replace t s]
    replaces the De Bruijn symbol 0 by s in t *)
val db_replace : foterm -> foterm -> foterm

(** Precedence constraint *)
val symbol_constraint : ordering_constraint

(** Creation of a new skolem symbol, applied to the given arguments.
    it also refreshes the ordering (the signature has changed) *)
val skolem : ordering -> foterm list -> sort -> foterm

(** new inference rules *)
val inference_rules : Superposition.inference_rule list
(** new superposition axioms, to add to the set of support *)
val axioms : ordering -> clause list

