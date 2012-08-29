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

(** check whether s is a binding symbol *)
val is_binder_symbol : symbol -> bool

(** nicer pretty printing *)
val pp_foterm : Format.formatter -> foterm -> unit
val pp_clause : Format.formatter -> clause -> unit

(** Precedence constraint *)
val symbol_constraint : ordering_constraint

(** The Delayed superposition calculus itself *)
val delayed : Calculus.calculus

