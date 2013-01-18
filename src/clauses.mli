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

open Types
open Symbols

val stat_fresh : statistics
val stat_mk_hclause : statistics
val stat_new_clause : statistics

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

(** left and right position in equation *)
val left_pos : int
val right_pos : int
val opposite_pos : int -> int

val eq_literal : literal -> literal -> bool         (** equality of literals *)
val eq_literal_com : literal -> literal -> bool     (** commutative equality of lits *)
val compare_literal : literal -> literal -> int     (** lexicographic comparison of literals *)
val compare_lits_partial : ord:ordering -> literal
                          -> literal -> comparison  (** partial comparison of literals *)
val hash_literal : literal -> int                   (** hashing of literal *)
val weight_literal : literal -> int                 (** weight of the lit *)

val pos_lit : literal -> bool                       (** is the literal positive? *)
val neg_lit : literal -> bool                       (** is the literal negative? *)
val equational_lit : literal -> bool                (** is the literal a proper equation? *)
val orientation_lit : literal -> comparison         (** get the orientation of the literal *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : ord:ordering -> term -> term -> literal
val mk_neq : ord:ordering -> term -> term -> literal
val mk_lit : ord:ordering -> term -> term -> bool -> literal
val reord_lit : ord:ordering -> literal -> literal  (** recompute order *)
val lit_of_fof : ord:ordering -> literal -> literal (** translate eq/not to literal *)
val term_of_lit : literal -> term                 (** translate lit to term *)

val apply_subst_lit : ?recursive:bool -> ord:ordering -> substitution -> literal -> literal

val negate_lit : literal -> literal (** negate literal *)
val fmap_lit : ord:ordering -> (term -> term) -> literal -> literal (** fmap in literal *)
val vars_of_lit : literal -> varlist (** gather variables *)

val lit_to_multiset : literal -> term list (** literal to multiset of terms *)

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

val eq_clause : clause -> clause -> bool          (** equality of clauses (as lists of literals) *)
val compare_clause : clause -> clause -> int      (** lexico order on literals of the clauses *)
val hash_clause : clause -> int                   (** hash of the clause *)

val eq_hclause : hclause -> hclause -> bool       (** equality of hashconsed clauses *)
val compare_hclause : hclause -> hclause -> int   (** simple order on hclauses (by ID) *)
val hash_hclause : hclause -> int
val stats : unit -> (int*int*int*int*int*int)     (** hashconsing stats *)

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

val mk_hclause : ord:ordering -> literal list -> proof -> hclause list -> hclause
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. *)

val mk_hclause_a : ord:ordering -> literal array -> proof -> hclause list -> hclause
  (** Build a new hclause from the given literals. If there are more than 31 literals,
      the prover becomes incomplete by returning [true] instead. This function takes
      ownership of the input array. *)

val mk_hclause_raw : selected:int -> maxlits:int -> selected_done:bool ->
                     literal array -> proof -> hclause list -> hclause
  (** Build a hclause with already computed max literals and selected literals.
      No check is (nor can) be performed. *)

val clause_of_fof : ord:ordering -> hclause -> hclause
  (** transform eq/not to literals *)

val reord_hclause : ord:ordering -> hclause -> hclause
  (** recompute order of literals in the clause *)

val check_ord_hclause : ord:ordering -> hclause -> unit
  (** checks that the clause is up-to-date w.r.t. the ordering *)

val select_clause : select:selection_fun -> hclause -> hclause
  (** select literals in clause, and computes ordering data *)

val descendants : hclause -> Ptset.t
  (** set of ID of descendants of the clause *)

val is_maxlit : hclause -> int -> bool
  (** i-th literal maximal in clause? *)

val check_maximal_lit : ord:ordering -> clause -> int
                    -> substitution -> bool
  (** is the i-th literal maximal in subst(clause)? *)

val maxlits : clause -> (literal * int) list
  (** get the list of maximal literals *)

val apply_subst_cl : ?recursive:bool -> ord:ordering -> substitution -> hclause -> hclause
  (** apply substitution to the clause *)

val get_lit : clause -> int -> literal
  (** get the literal at given index *)

val get_pos : clause -> position -> term
  (** get the subterm at position (TODO also with compact positions?) *)

val fresh_clause : ord:ordering -> int -> hclause -> clause
  (** rename a clause w.r.t. maxvar (all variables inside will be > maxvar) *)

val base_clause : hclause -> clause
  (** create a clause from a hclause, without renaming *)

val has_selected_lits : hclause -> bool
  (** does the clause have some selected literals? *)

val is_selected : hclause -> int -> bool
  (** check whether a literal is selected *)

val selected_lits : clause -> (literal * int) list
  (** get the list of selected literals *)

val eligible_res : ord:ordering -> clause -> int -> substitution -> bool
  (** check whether a literal is eligible for resolution *)

val eligible_param : ord:ordering -> clause -> int -> substitution -> bool
  (** check whether a literal is eligible for paramodulation *)

val is_unit_clause : hclause -> bool
  (** is the clause a unit clause? *)

val is_cnf : hclause -> bool
  (** Is the clause in CNF? *)

val signature : hclause list -> signature
  (** Compute signature of this set of clauses *)

val from_simple : ord:ordering -> Simple.sourced_formula -> hclause
  (** conversion to a clause. *)

val to_simple : hclause -> Simple.formula
  (** convert to a formula, losing the source information *)

(* ----------------------------------------------------------------------
 * set of hashconsed clauses
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
    type t = {
      maxvar : int;                 (** index of maximum variable *)
      clauses : hclause Ptmap.t;    (** clause ID -> hclause *)
    }

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

    val add_clause : t -> clause -> t
      (** add the hclause of this clause to the set *)

    val remove_id : t -> int -> t
      (** remove clause by ID *)

    val remove : t -> hclause -> t
      (** remove hclause *)

    val remove_list : t -> hclause list -> t
      (** remove hclauses *)

    val remove_ids : t -> Ptset.t -> t
      (** remove set of IDs *)

    val get : t -> int -> hclause
      (** get a clause by its ID *)

    val mem : t -> hclause -> bool
      (** membership test *)

    val mem_id : t -> int -> bool
      (** membership test by hclause ID *)

    val iter : t -> (hclause -> unit) -> unit
      (** iterate on clauses in the set *)

    val iteri : t -> (int -> hclause -> unit) -> unit
      (** iterate on clauses in the set with their ID *)

    val fold : ('b -> int -> hclause -> 'b) -> 'b -> t -> 'b
      (** fold on clauses *)

    val to_list : t -> hclause list
    val of_list : hclause list -> t
end

(* ----------------------------------------------------------------------
 * recognize some shapes of clauses
 * ---------------------------------------------------------------------- *)

val is_RR_horn_clause : hclause -> bool
  (** Recognized whether the clause is a Range-Restricted Horn clause *)

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

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

open Format

val string_of_comparison : comparison -> string
val string_of_pos : int -> string

(** pretty printer for literals *)
class type pprinter_literal =
  object
    method pp : Format.formatter -> literal -> unit     (** print literal *)
  end

val pp_literal : pprinter_literal                       (** use current term printer *)
val pp_literal_debug :                                  (** use debug unicode syntax *)
  < pp : Format.formatter -> literal -> unit;
    ord : bool -> unit;                                 (** print orientation of lit *)
  >
val pp_literal_tstp : pprinter_literal                  (** use TSTP syntax *)

val pp_pos : formatter -> position -> unit

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

val pp_lits : Format.formatter -> literal array -> unit

val pp_clause : pprinter_clause ref                     (** uses current term printer *)
val pp_clause_tstp : pprinter_clause                    (** TSTP syntax *)
val pp_clause_debug : pprinter_clause                   (** nice unicode syntax *)

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> hclause -> unit     (** pretty print proof from clause *)
  end

val pp_proof : pprinter_proof ref                       (** defaut printing of proofs *)
val pp_proof_tstp : pprinter_proof
val pp_proof_debug : pprinter_proof

val pp_set : Format.formatter -> CSet.t -> unit
