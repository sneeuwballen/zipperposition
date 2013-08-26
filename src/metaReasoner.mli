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

(** {1 Forward and backward Logic Reasoner} *)

type datalog_symbol

module Logic : Datalog.S with type symbol = datalog_symbol

(** {2 Encoding into Datalog literals/clauses} *)

(** This encoding allows to map complex structures into Datalog constants,
    and back. Its design aims at being type-safe and modular.
*)

module Translate : sig
  type 'a mapping

  val none : unit mapping
  val str : string mapping
  val abstract : (Term.t * Term.t list) mapping
  val parametrize : 'a mapping -> ('a * Term.t list) mapping
  val term : Term.t mapping
  val list_ : 'a mapping -> 'a list mapping
  val map : inject:('a -> 'b) -> extract:('b -> 'a) -> 'b mapping -> 'a mapping
  val pair : 'a mapping -> 'b mapping -> ('a * 'b) mapping
  val triple : 'a mapping -> 'b mapping -> 'c mapping -> ('a * 'b * 'c) mapping 
  val quad : 'a mapping -> 'b mapping -> 'c mapping -> 'd mapping -> ('a * 'b * 'c * 'd) mapping 

  val (|||) : 'a mapping -> 'b mapping -> ('a * 'b) mapping

  exception CannotDecode
    (** Raised when the literal does not correspond to the given mapping *)

  val encode : 'a mapping -> string -> 'a -> Logic.literal
  val decode : 'a mapping -> Logic.literal -> string * 'a

  val decode_head : 'a mapping -> string -> Logic.literal -> 'a
    (** Expect the given head string, fails otherwise *)
end

val pp_lit : Buffer.t -> Logic.literal -> unit
val pp_clause : Buffer.t -> Logic.clause -> unit

(** {2 Reasoner} *)

type t
  (** An instance of the reasoner *)

val create : unit -> t

val is_empty : t -> bool

val size : t -> int
  (** Number of clauses *)

val all_facts : t -> Logic.literal Sequence.t

val all_facts_matching : t -> Logic.literal -> Logic.literal Sequence.t
  (** All facts for the given predicate *)

val explanations : t -> Logic.clause -> Logic.explanation list

val explanations_lit : t -> Logic.literal -> Logic.explanation list

val is_fact : t -> Logic.literal -> bool

(** {2 Forward chaining} *)

val add : ?expl:Logic.explanation -> t -> Logic.clause -> unit
  (** Add a clause to the engine *)

val add_seq : t -> Logic.clause Sequence.t -> unit
  (** Add several clauses at once *)

val add_fact : ?expl:Logic.explanation -> t -> Logic.literal -> unit
  (** Add a fact to the engine *)

(** {2 Backward chaining (goals)} *)

val add_goal : t -> Logic.literal -> unit

(** {2 Explanations} *)

val explain : t -> Logic.literal -> Logic.literal list
  (** List of facts that explain the given fact (or Not_found) *)

val premises : t -> Logic.clause -> Logic.clause * Logic.literal list
  (** Hyperresolution atom+electrons that explain the given clause, ie,
      which directly imply it (or Not_found). *)

(** {2 Events} *)

val on_new_fact : t -> Logic.literal Signal.t
  (** Any new fact *)

val on_new_fact_by : t -> string -> Logic.literal Signal.t
  (** Obtain an event handler for facts that have the given predicate *)

val on_new_goal : t -> Logic.literal Signal.t
  (** All new goals *)

val on_new_goal_by : t -> string -> Logic.literal Signal.t
  (** Goal with the given predicate *)
