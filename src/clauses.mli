(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Literals and clauses *)

open Basic
open Symbols

val stat_fresh : statistics
val stat_mk_hclause : statistics
val stat_new_clause : statistics

(* ----------------------------------------------------------------------
 * boolean flags
 * ---------------------------------------------------------------------- *)

val flag_ground : int                             (** clause is ground *)
val flag_lemma : int                              (** clause is a lemma *)
val flag_persistent : int                         (** clause cannot be redundant *)

val set_flag : int -> hclause -> bool -> unit     (** set boolean flag *)
val get_flag : int -> hclause -> bool             (** get value of boolean flag *)

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

val eq_hclause : hclause -> hclause -> bool       (** equality of clauses *)
val hash_hclause : hclause -> int                 (** hash a clause *)
val compare_hclause : hclause -> hclause -> int   (** simple order on clauses (by ID) *)

module CHashtbl : Hashtbl.S with type key = clause

module CHashSet : 
  sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> hclause -> bool
    val iter : t -> (hclause -> unit) -> unit
    val add : t -> hclause -> unit
    val to_list : t -> hclause list
  end

val is_child_of : child:hclause -> hclause -> unit
  (** [is_child_of ~child c] is to be called to remember that [child] is a child
      of [c], is has been infered/simplified from [c] *)

module CHashcons : Hashcons.S with type t = hclause

val mk_hclause : ?parents:hclause list -> ?selected:Bitvector.t ->
                 ctx:context -> literal list ->
                  (compact_clause -> proof) -> hclause
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. *)

val mk_hclause_a : ?parents:hclause list -> ?selected:Bitvector.t ->
                   ctx:context -> literal array ->
                   (compact_clause -> proof) -> hclause
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. This function takes
      ownership of the input array. *)

val adapt_proof : proof -> compact_clause -> proof
  (** Adapt an old proof to the new compact_clause *)

val stats : unit -> (int*int*int*int*int*int) (** hashcons stats *)

val is_empty : hclause -> bool
  (** Is the clause an empty clause? *)

val descendants : hclause -> int SmallSet.t
  (** set of ID of descendants of the clause *)

val clause_of_fof : hclause -> hclause
  (** transform eq/not to literals *)

val update_ctx : ctx:context -> hclause -> hclause
  (** Change the context of the clause *)

val check_ord_hclause : ord:ordering -> hclause -> unit
  (** checks that the clause is up-to-date w.r.t. the ordering *)

val apply_subst : ?recursive:bool -> ?renaming:FoSubst.Renaming.t ->
                  substitution -> hclause bind -> hclause
  (** apply the substitution to the clause *)

val pos_lits : literal array -> Bitvector.t
  (** bitvector of literals that are positive *)

val neg_lits : literal array -> Bitvector.t
  (** bitvector of literals that are negative *)

val maxlits : clause bind -> substitution -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are maximal under [ord] *)

val is_maxlit : clause bind -> substitution -> int -> bool
  (** Is the i-th literal maximal in subst(clause)? Equivalent to
      Bitvector.get (maxlits ~ord c subst) i *)

val eligible_res : clause bind -> substitution -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. *)

val eligible_param : clause bind -> substitution -> Bitvector.t
  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. *)

val has_selected_lits : hclause -> bool
  (** does the clause have some selected literals? *)

val is_selected : hclause -> int -> bool
  (** check whether a literal is selected *)

val selected_lits : clause -> (literal * int) list
  (** get the list of selected literals *)

val is_unit_clause : hclause -> bool
  (** is the clause a unit clause? *)

val signature : hclause list -> signature
  (** Compute signature of this set of clauses *)

val from_term : ctx:context -> sourced_term -> hclause
  (** Conversion of a (boolean) term to a clause. *)

(* ----------------------------------------------------------------------
 * set of clauses, reachable by ID
 * ---------------------------------------------------------------------- *)

(** Simple set *)
module ClauseSet : Set.S with type elt = hclause

(** Set with access by ID, bookeeping of maximal var... *)
module CSet :
  sig

    (** Set of hashconsed clauses. 'a is the fantom type for hclauses.
        It also contains a payload that is updated on every addition/
        removal of clauses. The additional payload is also updated upon
        addition/deletion. *)
    type t

    val empty : t
      (** the empty set *)

    val is_empty : t -> bool
      (** is the set empty? *)

    val size : t -> int
      (** number of clauses in the set *)

    val add : t -> hclause -> t
      (** add the clause to the set *)

    val add_list : t -> hclause list -> t
      (** add several clauses to the set *)

    val remove_id : t -> int -> t
      (** remove clause by ID *)

    val remove : t -> hclause -> t
      (** remove hclause *)

    val remove_list : t -> hclause list -> t
      (** remove hclauses *)

    val get : t -> int -> hclause
      (** get a clause by its ID *)

    val mem : t -> hclause -> bool
      (** membership test *)

    val mem_id : t -> int -> bool
      (** membership test by hclause ID *)

    val choose : t -> hclause option
      (** Choose a clause in the set *)

    val iter : t -> (hclause -> unit) -> unit
      (** iterate on clauses in the set *)

    val iteri : t -> (int -> hclause -> unit) -> unit
      (** iterate on clauses in the set with their ID *)

    val fold : ('b -> int -> hclause -> 'b) -> 'b -> t -> 'b
      (** fold on clauses *)

    val to_list : t -> hclause list
    val of_list : hclause list -> t

    val to_seq : t -> hclause Sequence.t
    val of_seq : t -> hclause Sequence.t -> t
    val remove_seq : t -> hclause Sequence.t -> t
    val remove_id_seq : t -> int Sequence.t -> t
end

(* ----------------------------------------------------------------------
 * recognize some shapes of clauses
 * ---------------------------------------------------------------------- *)

val is_RR_horn_clause : hclause -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

val is_horn : hclause -> bool
  (** Recognizes Horn clauses (at most one positive literal) *)

val is_definition : hclause -> (term * term) option
  (** Check whether the clause defines a symbol, e.g.
      subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
      is a flat symbol with variables, and all variables in RHS
      are also in LHS *)

val is_rewrite_rule : hclause -> (term * term) list
  (** More general than definition. It means the clause is an
      equality where all variables in RHS are also in LHS. It
      can return two rewrite rules if the clause can be oriented
      in both ways, e.g. associativity axiom. *)

val is_const_definition : hclause -> (term * term) option
  (** Checks whether the clause is "const = ground composite term", e.g.
      a clause "aIbUc = inter(a, union(b, c))". In this case it returns
      Some(constant, definition of constant) *)

val is_pos_eq : hclause -> (term * term) option
  (** Recognize whether the clause is a positive unit equality. *)

(** {2 Positions in clauses} *)

type clause_pos = clause * position * term

val compare_clause_pos : clause_pos -> clause_pos -> int

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

(** pretty printer for clauses *)
class type pprinter_clause =
  object
    method pp_lits : Format.formatter -> literal array -> hclause -> unit
    method pp : Format.formatter -> clause -> unit      (** print clause *)
    method pp_h : Format.formatter -> hclause -> unit   (** print hclause *)
    method pp_pos : Format.formatter -> (clause * position) -> unit
    method pp_h_pos : Format.formatter -> (hclause * position * term) -> unit
    method pp_pos_subst : Format.formatter -> (clause * position * substitution) -> unit
    method horizontal : bool -> unit                    (** print in horizontal box? *)
  end

val pp_clause : pprinter_clause ref                     (** uses current term printer *)
val pp_clause_tstp : pprinter_clause                    (** TSTP syntax *)
val pp_clause_debug : pprinter_clause                   (** nice unicode syntax *)

val pp_set : Format.formatter -> CSet.t -> unit

val bij_compact : ord:ordering -> compact_clause Bij.t
val bij : ctx:context -> hclause Bij.t
val bij_set : ctx:context -> CSet.t Bij.t
