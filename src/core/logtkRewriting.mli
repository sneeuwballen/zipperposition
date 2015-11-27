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

(** {1 LogtkFOTerm rewriting} *)

(* FIXME: allow one to specify depth of rewritten term EVERYWHERE *)

(** {2 Ordered rewriting} *)

(** Although this module is parametrized by an EQUATION
    module, it only deals with positive equations. Negative
    equations will be discarded. *)

module type ORDERED = sig
  type t

  module E : LogtkIndex.EQUATION

  val empty : ord:LogtkOrdering.t -> t
  
  val add : t -> E.t -> t
  val add_seq : t -> E.t Sequence.t -> t
  val add_list : t -> E.t list -> t
  
  val to_seq : t -> E.t Sequence.t

  val size : t -> int
  
  val mk_rewrite : t -> size:int -> (LogtkFOTerm.t -> LogtkFOTerm.t)
    (** Given a TRS and a cache size, build a memoized function that
        performs term rewriting *)
end

module MakeOrdered(E : LogtkIndex.EQUATION with type rhs = LogtkFOTerm.t)
  : ORDERED with module E = E

(** {2 Regular rewriting} *)

module type SIG_TRS = sig
  type t

  type rule = LogtkFOTerm.t * LogtkFOTerm.t
    (** rewrite rule, from left to right *)

  val empty : unit -> t 

  val add : t -> rule -> t
  val add_seq : t -> rule Sequence.t -> t
  val add_list : t -> rule list -> t

  val to_seq : t -> rule Sequence.t
  val of_seq : rule Sequence.t -> t
  val of_list : rule list -> t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit
  
  val rule_to_form : rule -> LogtkFormula.FO.t
    (** Make a formula out of a rule (an equality) *)

  val rewrite_collect : t -> LogtkFOTerm.t -> LogtkFOTerm.t * rule list
    (** Compute normal form of the term, and also return the list of
        rules that were used. *)

  val rewrite : t -> LogtkFOTerm.t -> LogtkFOTerm.t
    (** Compute normal form of the term. See {!rewrite_collect}. *)
end

module MakeTRS(I : functor(E : LogtkIndex.EQUATION) -> LogtkIndex.UNIT_IDX with module E = E)
  : SIG_TRS

module TRS : SIG_TRS

(** {2 FOLogtkFormula rewriting} *)

module FormRW : sig
  type t
  type form = LogtkFormula.FO.t

  type rule = LogtkFOTerm.t * form
    (** rewrite rule, from left to right *)

  val empty : unit -> t 

  val add : t -> rule -> t
  val add_seq : t -> rule Sequence.t -> t
  val add_list : t -> rule list -> t

  val add_term_rule : t -> (LogtkFOTerm.t * LogtkFOTerm.t) -> t
  val add_term_rules : t -> (LogtkFOTerm.t * LogtkFOTerm.t) list -> t

  val to_seq : t -> rule Sequence.t
  val of_seq : rule Sequence.t -> t
  val of_list : rule list -> t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit

  val rule_to_form : rule -> form
    (** Convert the rule back to a term *)

  val rewrite_collect : t -> form -> form * rule list
    (** Compute normal form of the formula, and return it together with
        the list of rules that were used to rewrite. *)

  val rewrite : t -> form -> form
    (** see {!rewrite_collect} *)
end
