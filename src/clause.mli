
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


(** {1 Clauses} *)

(** The clauses are hashconsed within a context. The maximum number of literals
    that a clause can have is 63 (on a 64-bits machine), because of the way
    bitvectors are represented. A clause of more than 63 literals
    is replaced by "true" (tautology) and the context's incompleteness
    flag is set.
*)

open Logtk

val stat_fresh : Util.stat
val stat_mk_hclause : Util.stat
val stat_new_clause : Util.stat

type t = private {
  hclits : Literal.t array;               (** the literals *)
  hcctx : Ctx.t;                          (** context of the clause *)
  mutable hctag : int;                    (** unique ID of the clause *)
  mutable hcflags : int;                  (** boolean flags for the clause *)
  mutable hcweight : int;                 (** weight of clause *)
  mutable hcselected : Bitvector.t;       (** bitvector for selected literals *)
  mutable hcvars : Term.t list;           (** the free variables *)
  mutable hcproof : Proof.t;             (** Proof of the clause *)
  mutable hcparents : t list;             (** parents of the clause *)
  mutable hcdescendants : int SmallSet.t ;(** the set of IDs of descendants of the clause *)
} 

type clause = t

val compact : t -> CompactClause.t
  (** Turn into a compact clause *)

val to_seq : t -> (Term.t * Term.t * bool) Sequence.t
  (** Easy iteration on literals *)

val to_prec_clause : t -> Precedence.clause

(** {2 Flags} *)

val flag_ground : int                             (** clause is ground *)
val flag_lemma : int                              (** clause is a lemma *)
val flag_persistent : int                         (** clause cannot be redundant *)

val set_flag : int -> t -> bool -> unit     (** set boolean flag *)
val get_flag : int -> t -> bool             (** get value of boolean flag *)

(** {2 Basics} *)

val eq : t -> t -> bool         (** equality of clauses *)
val hash : t -> int             (** hash a clause *)
val compare : t -> t -> int     (** simple order on clauses (by ID) *)

module CHashtbl : Hashtbl.S with type key = t

module CHashSet : sig
  type t
  val create : unit -> t
  val is_empty : t -> bool
  val member : t -> clause -> bool
  val iter : t -> (clause -> unit) -> unit
  val add : t -> clause -> unit
  val to_list : t -> clause list
end

val is_child_of : child:t -> t -> unit
  (** [is_child_of ~child c] is to be called to remember that [child] is a child
      of [c], is has been infered/simplified from [c] *)

module CHashcons : Hashcons.S with type elt = clause

val create : ?parents:t list -> ?selected:Bitvector.t ->
             ctx:Ctx.t -> Literal.t list ->
             (CompactClause.t -> Proof.t) -> t
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. *)

val create_a : ?parents:t list -> ?selected:Bitvector.t ->
                ctx:Ctx.t -> Literal.t array ->
                (CompactClause.t -> Proof.t) -> t
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. This function takes
      ownership of the input array. *)

val create_forms : ?parents:t list -> ?selected:Bitvector.t ->
                    ctx:Ctx.t -> Formula.t list ->
                    (CompactClause.t -> Proof.t) -> t
  (** Directly from list of formulas *)

val get_proof : t -> Proof.t

val adapt_proof : Proof.t -> CompactClause.t -> Proof.t
  (** Adapt an old Proof.t to the new CompactClause.t *)

val stats : unit -> (int*int*int*int*int*int)
  (** hashconsing stats *)

val is_empty : t -> bool
  (** Is the clause an empty clause? *)

val descendants : t -> int SmallSet.t
  (** set of ID of descendants of the clause *)

val update_ctx : ctx:Ctx.t -> t -> t
  (** Change the context of the clause *)

val check_ord : ord:Ordering.t -> t -> unit
  (** checks that the clause is up-to-date w.r.t. the ordering *)

val apply_subst : ?recursive:bool -> ?renaming:Substs.Renaming.t ->
                  Substs.t -> t -> Substs.scope -> t
  (** apply the substitution to the clause *)

val maxlits : t Substs.scoped -> Substs.t -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are maximal under [ord] *)

val is_maxlit : t Substs.scoped -> Substs.t -> int -> bool
  (** Is the i-th literal maximal in subst(clause)? Equivalent to
      Bitvector.get (maxlits ~ord c subst) i *)

val eligible_res : t Substs.scoped -> Substs.t -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. *)

val eligible_param : t Substs.scoped -> Substs.t -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. *)

val has_selected_lits : t -> bool
  (** does the clause have some selected literals? *)

val is_selected : t -> int -> bool
  (** check whether a literal is selected *)

val selected_lits : t -> (Literal.t * int) list
  (** get the list of selected literals *)

val is_unit_clause : t -> bool
  (** is the clause a unit clause? *)

val is_oriented_rule : t -> bool
  (** Is the clause a positive oriented clause? *)

val infer_type : TypeInference.Ctx.t -> t Sequence.t -> unit
val signature : ?signature:Signature.t -> t Sequence.t -> Signature.t

val from_forms : file:string -> name:string -> ctx:Ctx.t -> Formula.t list -> t
  (** Conversion of a formula list to a clause *)

(** {2 Set of clauses} *)

(** Simple set *)
module ClauseSet : Set.S with type elt = t

(** Set with access by ID, bookeeping of maximal var... *)
module CSet : sig
  (** Set of hashconsed clauses. *)
  type t

  val empty : t
    (** the empty set *)

  val is_empty : t -> bool
    (** is the set empty? *)

  val size : t -> int
    (** number of clauses in the set *)

  val add : t -> clause -> t
    (** add the clause to the set *)

  val add_list : t -> clause list -> t
    (** add several clauses to the set *)

  val remove_id : t -> int -> t
    (** remove clause by ID *)

  val remove : t -> clause -> t
    (** remove hclause *)

  val remove_list : t -> clause list -> t
    (** remove hclauses *)

  val get : t -> int -> clause
    (** get a clause by its ID *)

  val mem : t -> clause -> bool
    (** membership test *)

  val mem_id : t -> int -> bool
    (** membership test by t ID *)

  val choose : t -> clause option
    (** Choose a clause in the set *)

  val iter : t -> (clause -> unit) -> unit
    (** iterate on clauses in the set *)

  val iteri : t -> (int -> clause -> unit) -> unit
    (** iterate on clauses in the set with their ID *)

  val fold : t -> 'b -> ('b -> int -> clause -> 'b) -> 'b
    (** fold on clauses *)

  val to_list : t -> clause list
  val of_list : clause list -> t

  val infer_type : TypeInference.Ctx.t -> t -> unit

  val to_seq : t -> clause Sequence.t
  val of_seq : t -> clause Sequence.t -> t
  val remove_seq : t -> clause Sequence.t -> t
  val remove_id_seq : t -> int Sequence.t -> t
end

(** {2 Positions in clauses} *)

type clause_pos = t * Position.t * Term.t

val compare_clause_pos : clause_pos -> clause_pos -> int

(** {2 IO} *)

val pp_tstp : Buffer.t -> t -> unit
val pp_debug : Buffer.t -> t -> unit

val to_string : t -> string               (** Debug printing to a  string *)
val fmt : Format.formatter -> t -> unit   (** debug printing *)

val pp_set_debug : Buffer.t -> CSet.t -> unit
val pp_set_tstp : Buffer.t -> CSet.t -> unit

val bij : ctx:Ctx.t -> t Bij.t
val bij_set : ctx:Ctx.t -> CSet.t Bij.t

