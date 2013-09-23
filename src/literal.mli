
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

(** {1 Equational literals} *)

open Logtk

type t = private
  | Equation of Term.t * Term.t * bool * Comparison.t
  | Prop of Term.t * bool
  | True
  | False
  (** a literal, that is, a signed equation or a proposition *)

val eq : t -> t -> bool         (** equality of literals *)
val eq_com : t -> t -> bool     (** commutative equality of lits *)
val compare : t -> t -> int     (** lexicographic comparison of literals *)

val hash : t -> int
val hash_novar : t -> int

val variant : ?subst:Substs.t -> t -> Substs.scope -> t -> Substs.scope -> Substs.t
val are_variant : t -> t -> bool

val compare_partial : ord:Ordering.t -> t -> t -> Comparison.t
  (** partial comparison of literals *)

val to_multiset : t -> Term.t list    (** literal to multiset of terms *)

val hash : t -> int                   (** hashing of literal *)
val weight : t -> int                 (** weight of the lit *)
val depth : t -> int                  (** depth of literal *)

val is_pos : t -> bool                (** is the literal positive? *)
val is_neg : t -> bool                (** is the literal negative? *)
val equational : t -> bool            (** is the literal a proper equation? *)
val orientation_of : t -> Comparison.t  (** get the orientation of the literal *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : ord:Ordering.t -> Term.t -> Term.t -> t
val mk_neq : ord:Ordering.t -> Term.t -> Term.t -> t
val mk_lit : ord:Ordering.t -> Term.t -> Term.t -> bool -> t
val mk_prop : ord:Ordering.t -> Term.t -> bool -> t   (* proposition *)
val mk_true : ord:Ordering.t -> Term.t -> t     (* true proposition *)
val mk_false : ord:Ordering.t -> Term.t -> t    (* false proposition *)

val reord : ord:Ordering.t -> t -> t      (** recompute order *)
val lit_of_form : ord:Ordering.t -> Formula.t -> t (** translate eq/not to literal *)
val to_tuple : t -> (Term.t * Term.t * bool)
val form_of_lit : t -> Formula.t
val term_of_lit : t -> Term.t                   (** translate lit to term *)

val apply_subst : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                  ord:Ordering.t -> Substs.t -> t -> Substs.scope -> t

val negate : t -> t                     (** negate literal *)
val fmap : ord:Ordering.t -> (Term.t -> Term.t) -> t -> t (** fmap in literal *)
val add_vars : Term.THashSet.t -> t -> unit  (** Add variables to the set *)
val vars : t -> Term.varlist (** gather variables *)
val var_occurs : Term.t -> t -> bool
val is_ground : t -> bool

val infer_type : TypeInference.Ctx.t -> t -> unit
val signature : ?signature:Signature.t -> t -> Signature.t

val apply_subst_list : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                        ord:Ordering.t -> Substs.t ->
                        t list -> Substs.scope -> t list

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit
val bij : ord:Ordering.t -> t Bij.t

(** {2 Arrays of literals} *)

module Arr : sig
  val eq : t array -> t array -> bool
  val compare : t array -> t array -> int
  val hash : t array -> int
  val hash_novar : t array -> int

  val sort_by_hash : t array -> unit
    (** Sort literals by increasing [hash_novar] *)

  val variant : ?subst:Substs.t ->
                t array -> Substs.scope -> t array -> Substs.scope ->
                Substs.t
  val are_variant : t array -> t array -> bool

  val weight : t array -> int
  val depth : t array -> int
  val vars : t array -> Term.varlist
  val is_ground : t array -> bool             (** all the literals are ground? *)

  val to_form : t array -> Formula.t

  val apply_subst : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                         ord:Ordering.t -> Substs.t ->
                         t array -> Substs.scope -> t array

  val pos : t array -> Bitvector.t
  val neg : t array -> Bitvector.t
  val maxlits : ord:Ordering.t -> t array -> Bitvector.t

  val to_seq : t array -> (Term.t * Term.t * bool) Sequence.t
    (** Convert the lits into a sequence of equations *)

  val of_forms : ord:Ordering.t -> Formula.t list -> t array
    (** Convert a list of atoms into literals *)

  val to_forms : t array -> Formula.t list
    (** To list of formulas *)

  val infer_type : TypeInference.Ctx.t -> t array -> unit
  val signature : ?signature:Signature.t -> t array -> Signature.t

  (** {3 High order combinators} *)

  val fold_eqn : ?both:bool ->
                  eligible:(int -> t -> bool) ->
                  t array -> 'a ->
                  ('a -> Term.t -> Term.t -> bool -> Position.t -> 'a) ->
                  'a
    (** fold f over all literals sides, with their positions.
        f is given (acc, left side, right side, sign, position of left side)
        if both=true, then both sides of a non-oriented equation
          will be visited *)

  val get_eqn : t array -> Position.t -> Term.t * Term.t * bool
    (** get the term l at given position in clause, and r such that l ?= r
        is the Literal.t at the given position *)

  (** {3 IO} *)

  val pp : Buffer.t -> t array -> unit
  val pp_tstp : Buffer.t -> t array -> unit
  val to_string : t array -> string
  val fmt : Format.formatter -> t array -> unit
  val bij : ord:Ordering.t -> t array Bij.t
end

(** {2 Special kinds of literal arrays} *)

val is_RR_horn_clause : t array -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : t array -> bool
  (** Recognizes Horn clauses (at most one positive literal) *)

val is_pos_eq : t array -> (Term.t * Term.t) option
  (** Recognize whether the clause is a positive unit equality. *)
