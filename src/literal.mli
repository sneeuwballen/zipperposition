
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
 | Equation of    Term.t  (** lhs *)
                * Term.t  (** rhs *)
                * bool    (** sign (equality, ie true, or difference) *)
                * Comparison.t   (* TODO remove? or just orient equations? *)
  (** a literal, that is, a signed equation *)

val eq : t -> t -> bool         (** equality of literals *)
val eq_com : t -> t -> bool     (** commutative equality of lits *)
val compare : t -> t -> int     (** lexicographic comparison of literals *)

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
val reord : ord:Ordering.t -> t -> t      (** recompute order *)
val lit_of_fof : ord:Ordering.t -> t -> t (** translate eq/not to literal *)
val term_of_lit : t -> Term.t                   (** translate lit to term *)

val apply_subst : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                  ord:Ordering.t -> Substs.t -> t -> Substs.scope -> t

val negate : t -> t                     (** negate literal *)
val fmap : ord:Ordering.t -> (Term.t -> Term.t) -> t -> t (** fmap in literal *)
val add_vars : Term.THashSet.t -> t -> unit  (** Add variables to the set *)
val vars : t -> Term.varlist (** gather variables *)

val infer_type : TypeInference.Ctx.t -> t -> TypeInference.Ctx.t
val signature : ?signature:Signature.t -> t -> Signature.t

(** {2 Arrays of literals} *)

val eq_lits : t array -> t array -> bool
val compare_lits : t array -> t array -> int
val hash_lits : t array -> int
val weight_lits : t array -> int
val depth_lits : t array -> int
val vars_lits : t array -> Term.varlist
val ground_lits : t array -> bool             (** all the literals are ground? *)
val term_of_lits : t array -> Term.t

val apply_subst_lits : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                       ord:Ordering.t -> Substs.t ->
                       t array -> Substs.scope -> t array
val apply_subst_list : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                        ord:Ordering.t -> Substs.t ->
                        t list -> Substs.scope -> t list

val pos_lits : t array -> Bitvector.t
val neg_lits : t array -> Bitvector.t
val maxlits : ord:Ordering.t -> t array -> Bitvector.t

val lits_to_seq : t array -> (Term.t * Term.t * bool) Sequence.t
  (** Convert the lits into a sequence of equations *)

val lits_of_terms : ord:Ordering.t -> Term.t list -> t array
  (** Convert a list of atoms into a list of literals *)

val lits_infer_type : TypeInference.Ctx.t -> t array -> TypeInference.Ctx.t
val lits_signature : ?signature:Signature.t -> t array -> Signature.t

(** {2 Special kinds of literal arrays} *)

val is_RR_horn_clause : t array -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : t array -> bool
  (** Recognizes Horn clauses (at most one positive literal) *)

val is_definition : t array -> (Term.t * Term.t) option
  (** Check whether the clause defines a symbol, e.g.
      subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
      is a flat symbol with variables, and all variables in RHS
      are also in LHS *)

val is_rewrite_rule : t array -> (Term.t * Term.t) list
  (** More general than definition. It means the clause is an
      equality where all variables in RHS are also in LHS. It
      can return two rewrite rules if the clause can be oriented
      in both ways, e.g. associativity axiom. *)

val is_const_definition : t array -> (Term.t * Term.t) option
  (** Checks whether the clause is "const = ground composite term", e.g.
      a clause "aIbUc = inter(a, union(b, c))". In this case it returns
      Some(constant, definition of constant) *)

val is_pos_eq : t array -> (Term.t * Term.t) option
  (** Recognize whether the clause is a positive unit equality. *)

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_lits : Buffer.t -> t array -> unit
val to_string : t -> string
val lits_to_string : t array -> string
val fmt : Format.formatter -> t -> unit
val fmt_lits : Format.formatter -> t array -> unit

val bij : ord:Ordering.t -> t Bij.t
val bij_lits : ord:Ordering.t -> t array Bij.t
