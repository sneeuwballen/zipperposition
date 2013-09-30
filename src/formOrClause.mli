
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Formula Or Clause} *)

open Logtk

(** This module defines a type that can be either a clause or any formula,
    operations on the type, and a set of such formulas. *)

type t = private
  | F of PFormula.t * forward_cell
  | C of Clause.t
and forward_cell = t option ref

type form_or_clause = t

(* TODO : construction from F and C, simplification DAG, simplification
  fixpoint, set of FormOrClause, printing/bij.
  This is vital for preprocessing!! *)

val eq : t -> t -> bool         (** equality of clauses *)
val hash : t -> int             (** hash a clause *)
val compare : t -> t -> int     (** simple order on clauses (by ID) *)

val of_form : PFormula.t -> t
val of_clause : Clause.t -> t

val get_proof : t -> Proof.t
  (** Get proof of the formula *)

val follow_simpl : t -> t
  (** Follow the "simplify to" links until the clause has None *)

val simpl_to : from:t -> into:t -> unit
  (** [simpl_to ~from ~into] sets the link of [from] to [into], so that
      the simplification of [from] into [into] is cached. *)

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : ctx:Ctx.t -> t Bij.t

(** {2 Persistent Set} *)

module Set : Set.S with type elt = t
