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

val eq_clause : clause -> clause -> bool                (** equality of clauses (as lists of literals) *)
val compare_clause : clause -> clause -> int            (** lexico order on literals of the clauses *)
val hash_clause : clause -> int                         (** hash of the clause *)

val eq_hclause : 'a hclause -> 'a hclause -> bool       (** equality of hashconsed clauses *)
val compare_hclause : 'a hclause -> 'a hclause -> int   (** simple order on hclauses (by ID) *)
val stats : unit -> (int*int*int*int*int*int)           (** hashcons stats *)

module CHashSet : 
  sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> c_ready hclause -> bool
    val iter : t -> (c_ready hclause -> unit) -> unit
    val add : t -> c_ready hclause -> unit
    val to_list : t -> c_ready hclause list
  end

val mk_hclause : ord:ordering -> literal list -> proof Lazy.t -> c_ready hclause list -> c_new hclause
  (** build a clause from literals *)

val mk_hclause_a : ord:ordering -> literal array -> proof Lazy.t -> c_ready hclause list -> c_new hclause
  (** build a clause from an array of literals (destructive on the array) *)

val clause_of_fof : ord:ordering -> 'a hclause -> c_new hclause
  (** transform eq/not to literals *)

val reord_hclause : ord:ordering -> 'a hclause -> c_new hclause
  (** recompute order of literals in the clause *)

val select_clause : ord:ordering -> select:selection_fun -> c_new hclause -> c_ready hclause
  (** select literals in clause, and computes ordering data *)

val selected: c_ready hclause -> int array
  (** indexes of selected literals *)

val parents : 'a hclause -> c_ready hclause list
  (** list of parents of the clause *)

val is_maxlit : clause -> int -> bool
  (** i-th literal maximal in clause? *)

val check_maximal_lit : ord:ordering -> clause -> int
                    -> substitution -> bool
  (** is the i-th literal maximal in subst(clause)? *)

val maxlits : clause -> (literal * int) list
  (** get the list of maximal literals *)

val apply_subst_cl : ?recursive:bool -> ord:ordering -> substitution -> 'a hclause -> c_new hclause
  (** apply substitution to the clause *)

val get_lit : clause -> int -> literal
  (** get the literal at given index *)

val get_pos : clause -> position -> term
  (** get the subterm at position (TODO also with compact positions?) *)

val fresh_clause : ord:ordering -> int -> c_ready hclause -> clause
  (** rename a clause w.r.t. maxvar (all variables inside will be > maxvar) *)

val is_selected : clause -> int -> bool
  (** check whether a literal is selected *)

val selected_lits : clause -> (literal * int) list
  (** get the list of selected literals *)

val eligible_res : ord:ordering -> clause -> int -> substitution -> bool
  (** check whether a literal is eligible for resolution *)

val eligible_param : ord:ordering -> clause -> int -> substitution -> bool
  (** check whether a literal is eligible for paramodulation *)

val is_unit_clause : 'a hclause -> bool
  (** is the clause a unit clause? *)

(* ----------------------------------------------------------------------
 * set of hashconsed clauses
 * ---------------------------------------------------------------------- *)

(** Data attached to the set of clauses, from/to which clauses can be
    removed/added. The purpose is to attach indexes or metadata
    about clauses. *)
module type Payload =
  sig
    type 'a t 
    val empty : 'a t
    val add : 'a t -> 'a hclause -> 'a t
    val remove : 'a t -> 'a hclause -> 'a t
  end

module CSet(P : Payload) :
  sig

    (** Set of hashconsed clauses. 'a is the fantom type for hclauses.
        It also contains a payload that is updated on every addition/
        removal of clauses. The additional payload is also updated upon
        addition/deletion. *)
    type 'a t = {
      maxvar : int;                 (** index of maximum variable *)
      clauses : 'a hclause Ptmap.t; (** clause ID -> clause *)
      payload : 'a P.t;             (** additional data *)
    }

    val empty : 'a t  (** the empty set *)
    val is_empty : 'a t -> bool               (** is the set empty? *)

    val size : 'a t -> int                    (** number of clauses in the set *)

    val add : 'a t -> 'a hclause -> 'a t      (** add the clause to the set *)
    val add_list : 'a t -> 'a hclause list -> 'a t
    val add_clause : 'a t -> clause -> 'a t   (** add the hclause of this clause to the set *)

    val union : 'a t -> 'a t -> 'a t          (** union of sets (slow) *)

    val remove : 'a t -> 'a hclause -> 'a t   (** remove hclause *)
    val remove_list : 'a t -> 'a hclause list -> 'a t

    val get : 'a t -> int -> 'a hclause       (** get a clause by its ID *)

    val mem : 'a t -> 'a hclause -> bool      (** membership test *)
    val mem_id : 'a t -> int -> bool

    val iter : 'a t -> (int -> 'a hclause -> unit)
                -> unit                       (** iterate on clauses in the set *)

    val fold : ('b -> int -> 'a hclause -> 'b) -> 'b -> 'a t
               -> 'b                          (** fold on clauses *)

    val partition : 'a t -> ('a hclause -> bool) -> 'a t * 'a t
      (** for a predicate p, returns (set of c s.t. p(c)), (set of c s.t. not p(c)) *)

    val to_list : 'a t -> 'a hclause list
    val of_list : 'a hclause list -> 'a t

    val pp : Format.formatter -> 'a t -> unit
end

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
    method pp : Format.formatter -> clause -> unit      (** print clause *)
    method pp_h : Format.formatter -> 'a hclause -> unit(** print hclause *)
    method pp_pos : Format.formatter -> (clause * position) -> unit
    method pp_h_pos : Format.formatter -> ('a hclause * position * term) -> unit
    method pp_pos_subst : Format.formatter -> (clause * position * substitution) -> unit
    method horizontal : bool -> unit                    (** print in horizontal box? *)
  end

val pp_clause : pprinter_clause ref                     (** uses current term printer *)
val pp_clause_tstp : pprinter_clause                    (** TSTP syntax *)
val pp_clause_debug : pprinter_clause                   (** nice unicode syntax *)

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> clause -> unit      (** pretty print proof from clause *)
  end

val pp_proof : pprinter_proof ref                       (** defaut printing of proofs *)
val pp_proof_tstp : pprinter_proof
val pp_proof_debug : pprinter_proof
