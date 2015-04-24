
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

(** {1 Basic Splitting à la Avatar}

We don't implement all the stuff from Avatar, in particular all clauses are
active whether or not their trail is satisfied in the current model.
Trails are only used to make splits easier {b currently}.

Future work may include locking clauses whose trails are unsatisfied.

Depends on the "meta" extension.
*)

type 'a printer = Format.formatter -> 'a -> unit

type formula = Logtk.Formula.FO.t

(** {2 Avatar: splitting+sat} *)

module type S = sig
  module E : Env.S
  module Solver : BoolSolver.SAT

  val split : E.multi_simpl_rule
  (** Split a clause into components *)

  val check_empty : E.unary_inf_rule
  (** Forbid empty clauses with trails, i.e. adds the negation of their
      trails to the SAT-solver *)

  val before_check_sat : unit Signal.t
  val after_check_sat : unit Signal.t

  val filter_absurd_trails : (E.C.Trail.t -> bool) -> unit
  (** [filter_trails f] calls [f] on every trail associated with the empty
      clause. If [f] returns [false], the trail is ignored, otherwise
      it's negated and sent to the SAT solver *)

  val check_satisfiability : E.generate_rule
  (** Checks  that the SAT context is still valid *)

  val save_clause : tag:int -> E.C.t -> unit
  (** Map the tag to the clause *)

  val get_clause : tag:int -> E.C.t option
  (** Recover clause from the tag, if any *)

  val introduce_cut : formula -> (CompactClause.t -> Proof.t) ->
                      E.C.t list * E.Ctx.BoolLit.t
  (** Introduce a cut on the given formula *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

module Make(E : Env.S)(Sat : BoolSolver.SAT) : S
  with module E = E and module Solver = Sat

val extension : Extensions.t
(** Extension that enables Avatar splitting and create a new SAT-solver. *)
