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

(** {1 Knowledge base} *)

open Logtk

(** {2 Basic knowledge} *)

type lemma =
  | Lemma of MetaPattern.t * HOTerm.t list * premise list
and axiom =
  | Axiom of string * HOTerm.t list * MetaPattern.t * HOTerm.t list
and theory =
  | Theory of string * HOTerm.t list * premise list
and premise =
  | IfAxiom of string * HOTerm.t list
  | IfTheory of string * HOTerm.t list
  | IfPattern of MetaPattern.t * HOTerm.t list
and clause =
  | Clause of raw_lit * raw_lit list
and raw_lit = string * string list

(** {2 Knowledge base} *)

type t   (* Knowledge base *)

val eq : t -> t -> bool

val empty : t

val compare_lemma : lemma -> lemma -> int
val compare_axiom : axiom -> axiom -> int
val compare_theory : theory -> theory -> int
val compare_premise : premise -> premise -> int
val compare_premises : premise list -> premise list -> int

val add_lemma : t -> lemma -> t
val add_axiom : t -> axiom -> t
val add_theory : t -> theory -> t
val add_clause : t -> clause -> t

val get_axiom : t -> string -> axiom list
val get_theory : t -> string -> theory list

val all_patterns : t -> MetaPattern.t list
  (** All patterns used in some premise *)

val union : t -> t -> t
  (** Join two bases together *)

val diff : t -> t -> t
  (** [diff kb1 kb2] is the set of definitions of [kb1] that
      do not belong to [kb2] *)

val to_seq : t -> (lemma Sequence.t * axiom Sequence.t *
                   theory Sequence.t * clause Sequence.t)
val of_seq : (lemma Sequence.t * axiom Sequence.t *
              theory Sequence.t * clause Sequence.t) -> t

val pp_lemma : Buffer.t -> lemma -> unit
val pp_theory : Buffer.t -> theory -> unit
val pp_axiom : Buffer.t -> axiom -> unit
val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit

val bij_lemma : lemma Bij.t
val bij_axiom : axiom Bij.t
val bij_theory : theory Bij.t
val bij_clause : clause Bij.t
val bij_premise : premise Bij.t
val bij : t Bij.t

(** {2 MetaReasoner} *)

type found_lemma =
  | NewLemma of FOFormula.t * MetaReasoner.Logic.literal
    (** formula + explanation *)

and found_theory =
  | NewTheory of string * HOTerm.t list * MetaReasoner.Logic.literal
    (** Theory + explanation *)

and found_axiom =
  | NewAxiom of string * HOTerm.t list

val add_reasoner : MetaReasoner.t -> t -> unit
  (** Add definitions to the reasoner *)

val on_lemma : MetaReasoner.t -> found_lemma Signal.t
val on_axiom : MetaReasoner.t -> found_axiom Signal.t
val on_theory : MetaReasoner.t -> found_theory Signal.t

val cur_lemmas : MetaReasoner.t -> found_lemma Sequence.t  (* lemmas discovered *)
val cur_theories : MetaReasoner.t -> found_theory Sequence.t (* theories discovered *)

(** {2 Backward chaining} *)

val match_lemmas : t -> MetaPattern.EncodedForm.t -> (lemma * HOTerm.t list) list
  (** Given a KB and a goal formula [g], find lemmas whose
      conclusions imply [g]. For each such lemma [l], return:

      - the lemma
      - a list of arguments that serve to instantiate the lemma's conclusion
  *)

type lemma_back_chain =
  | LBC_add_goal of MetaPattern.EncodedForm.t
  | LBC_add_datalog_goal of MetaReasoner.Logic.literal
  | LBC_add_datalog_clause of MetaReasoner.Logic.clause

val backward_chain : t -> MetaPattern.EncodedForm.t -> lemma_back_chain list
  (** uses {!match_lemmas} to try to solve the given goal
      using lemmas. It returns a list of actions that may help solving
      the input goal. *)

(** {2 IO} *)

val parse_theory_file : string -> t
  (** Parse the given file (blocking). On failure, logs an error
      and return the empty KB. *)

val save : string -> t -> unit
  (** Save to the file (blocking) *)

val restore : string -> t option
  (** Restore from a file (blocking) *)

