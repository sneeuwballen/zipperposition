(*
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

(** {1 Meta-prover, working on axioms, theories and lemmas} *)

open Logtk

type t

(** {2 Basic operations and accessors} *)

val create : ?kb:MetaKB.t -> unit -> t
  (** New prover *)

val patterns : t -> MetaPattern.Set.t
  (** Set of active patterns *)

val kb : t -> MetaKB.t
  (** Knowledge base of the prover *)

val reasoner : t -> MetaReasoner.t
  (** Reasoner of the prover *)

val add_pattern : t -> MetaPattern.t -> unit
  (** New pattern to match against formulas *)

val add_kb : t -> MetaKB.t -> unit
  (** Update the KB *)

val match_formula : t -> FOFormula.t -> MetaReasoner.Logic.literal list
  (** List of literals representing patterns matching this formula.
      Literals can then be added with {! add_literals} *)

val match_clause : t -> FOFormula.t list -> MetaReasoner.Logic.literal list
  (** See {! match_formula} *)

val add_literals : t -> MetaReasoner.Logic.literal Sequence.t -> unit
  (** Add the literals to the reasoner *)

val add_clauses : t -> MetaReasoner.Logic.clause Sequence.t -> unit
  (** Add the given clauses to the reasoner *)

val add_goal : t -> MetaReasoner.Logic.literal -> unit

(** {2 Basic events} *)

(** Those events should be used by the user's code when it wishes
    to know which lemmas, theories and axioms have been detected *)

val on_lemma : t -> MetaKB.found_lemma Signal.t
val on_theory : t -> MetaKB.found_theory Signal.t
val on_axiom : t -> MetaKB.found_axiom Signal.t

val on_goal_pattern : t -> MetaPattern.t Signal.t

(** {2 IO} *)

val parse_theory_file : t -> string -> unit
  (** Parse a theory file and update the KB *)

val save_kb : t -> string -> unit
val restore_kb : t -> string -> unit
