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

(** {1 FOTerm rewriting} *)

(* FIXME: allow one to specify depth of rewritten term EVERYWHERE *)

(** {2 Ordered rewriting} *)

(** Although this module is parametrized by an EQUATION
    module, it only deals with positive equations. Negative
    equations will be discarded. *)

module type ORDERED = sig
  type t

  module E : Index.EQUATION

  val empty : ord:Ordering.t -> t
  
  val add : t -> E.t -> t
  val add_seq : t -> E.t Sequence.t -> t
  val add_list : t -> E.t list -> t
  
  val to_seq : t -> E.t Sequence.t

  val size : t -> int
  
  val mk_rewrite : t -> size:int -> (FOTerm.t -> FOTerm.t)
    (** Given a TRS and a cache size, build a memoized function that
        performs term rewriting *)
end

module MakeOrdered(E : Index.EQUATION with type rhs = FOTerm.t) : ORDERED with module E = E

(** {2 Regular rewriting} *)

module type SIG_TRS = sig
  type t

  type rule = FOTerm.t * FOTerm.t
    (** rewrite rule, from left to right *)

  val empty : t 

  val add : t -> rule -> t
  val add_seq : t -> rule Sequence.t -> t
  val add_list : t -> rule list -> t

  val to_seq : t -> rule Sequence.t
  val of_seq : rule Sequence.t -> t
  val of_list : rule list -> t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit
  
  val rule_to_form : rule -> FOFormula.t
    (** Make a formula out of a rule (an equality) *)

  val rewrite_collect : ?depth:int -> t -> FOTerm.t -> FOTerm.t * rule list
    (** Compute normal form of the term, and also return the list of
        rules that were used.
        @param depth the number of surrounding binders (default 0) *)

  val rewrite : ?depth:int -> t -> FOTerm.t -> FOTerm.t
    (** Compute normal form of the term.
        see {!rewrite_collect}. *)
end

module MakeTRS(I : functor(E : Index.EQUATION) -> Index.UNIT_IDX with module E = E)
  : SIG_TRS

module TRS : SIG_TRS

(** {2 FOFormula rewriting} *)

module FormRW : sig
  type t

  type rule = FOTerm.t * FOFormula.t
    (** rewrite rule, from left to right *)

  val empty : t 

  val add : t -> rule -> t
  val add_seq : t -> rule Sequence.t -> t
  val add_list : t -> rule list -> t

  val add_term_rule : t -> (FOTerm.t * FOTerm.t) -> t
  val add_term_rules : t -> (FOTerm.t * FOTerm.t) list -> t

  val to_seq : t -> rule Sequence.t
  val of_seq : rule Sequence.t -> t
  val of_list : rule list -> t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit

  val rule_to_form : rule -> FOFormula.t
    (** Convert the rule back to a term *)

  val rewrite_collect : ?depth:int -> t -> FOFormula.t -> FOFormula.t * rule list
    (** Compute normal form of the formula, and return it together with
        the list of rules that were used to rewrite.
        @param depth the number of surrounding binders *)

  val rewrite : ?depth:int -> t -> FOFormula.t -> FOFormula.t
    (** see {!rewrite_collect} *)
end
