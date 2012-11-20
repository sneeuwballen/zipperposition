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

(* ----------------------------------------------------------------------
 * equations
 * ---------------------------------------------------------------------- *)

(** left and right position in equation *)
val left_pos : int
val right_pos : int
val opposite_pos : int -> int

val eq_eqn : equation -> equation -> bool             (** equality of equations *)
val eq_eqn_com : equation -> equation -> bool         (** commutative equality of equations *)
val compare_eqn : equation -> equation -> int         (** lexicographic comparison of eqns *)
val compare_eqn_partial : ord:ordering -> equation
                          -> equation -> comparison   (** partial comparison of eqns *)
val hash_eqn : equation -> int                        (** hashing of equation *)

val pos_eqn : equation -> bool                        (** is the eqn positive? *)
val neg_eqn : equation -> bool                        (** is the eqn negative? *)

(** Build equations. If sides so not have the same sort,
    a SortError will be raised. *)
val mk_eq : term -> term -> equation
val mk_neq : term -> term -> equation
val mk_eqn : term -> term -> bool -> equation
val eqn_of_fof : equation -> equation                 (** translate eq/not to literal *)
val term_of_eqn : equation -> term                    (** translate lit to term *)

val apply_subst_eqn : ?recursive:bool -> substitution -> equation -> equation

val negate_eqn : equation -> equation                 (** negate equation *)
val fmap_eqn : (term -> term) -> equation -> equation (** fmap in equation *)
val vars_of_eqn : equation -> varset                  (** gather variables *)

val eqn_depth : equation -> int                       (** depth of terms *)
val eqn_to_multiset : equation -> term list           (** equation to multiset of terms *)

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

val eq_lit : literal -> literal -> bool
val compare_lit : literal -> literal -> int

val mk_lit : equation -> literal                        (** build literal *)
val copy_lit : literal -> literal                       (** copy shared parts of literal *)

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

val eq_clause : clause -> clause -> bool                (** equality of clauses *)
val compare_clause : clause -> clause -> int            (** lexico order on clauses *)
val hash_clause : clause -> int                         (** hash of the clause *)

val hashcons_clause : clause -> hclause
val stats : unit -> (int*int*int*int*int*int)           (** hashcons stats *)

val eq_hclause : hclause -> hclause -> bool             (** equality of hashconsed clauses *)
val compare_hclause : hclause -> hclause -> int         (** simple order on lexico clauses *)

module CHashSet : 
  sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> clause -> bool
    val iter : t -> (clause -> unit) -> unit
    val add : t -> clause -> unit
    val to_list : t -> clause list
  end

(** container used to store the state necessary to build clauses *)
type clause_state =
  < ord : ordering;
    select : selection_fun >

val mk_state : ?select:selection_fun -> ord:ordering ->
                clause_state                            (** build a clause state *)

val mk_clause : cs:clause_state -> equation array ->
                proof Lazy.t -> clause list ->
                clause                                  (** build a clause *)

val clause_of_fof : cs:clause_state -> clause -> clause (** transform eq/not to literals *)

val parents : clause -> clause list                     (** list of parents of the clause *)
val check_maximal_lit: ord:ordering -> clause -> int -> (** is the i-th literal *)
                       substitution -> bool             (** maximal in subst(clause)? *)

val apply_subst_cl : ?recursive:bool -> cs:clause_state -> substitution -> clause -> clause

val get_lit : clause -> int -> literal                  (** get the literal at given index *)
val get_pos : clause -> position -> term                (** get the subterm at position *)

val iter_maxlits : clause ->
                   (int -> literal -> unit) -> unit     (** iterate on maximal literals *)
val iter_selected: clause ->
                   (int -> literal -> unit) -> unit     (** iterate on selected literals *)

val fold_lits : pos:bool -> neg:bool -> selected:bool -> max:bool
                -> clause -> 'a -> ('a -> int -> literal -> 'a)
                -> 'a                                   (** fold over literals that fit the parameters *)

(** rename a clause w.r.t. maxvar (all variables inside will be > maxvar) *)
val fresh_clause : cs:clause_state -> int -> clause -> clause
(** normalize (vars start at 1) *)
val normalize_clause : cs:clause_state -> clause -> clause                 

(** check whether a literal is selected *)
val selected_lit : clause -> int -> bool
(** get the list of selected literals *)
val selected_lits : clause -> (literal * int) list
(** check whether a literal is eligible for resolution *)
val eligible_res : cs:clause_state -> clause -> int -> substitution -> bool
(** check whether a literal is eligible for paramodulation *)
val eligible_param : cs:clause_state -> clause -> int -> substitution -> bool

(* ----------------------------------------------------------------------
 * bag of clauses
 * ---------------------------------------------------------------------- *)

(** set of hashconsed clauses *)
type bag = {
  bag_maxvar: int;                (** higher bound for the biggest var index *)
  bag_clauses : hclause Ptmap.t;  (** map tag -> hashconsed clause *)
}

val add_to_bag : bag -> clause -> bag * hclause     (** add the clause to the bag, hashconsing it *)
val add_hc_to_bag : bag -> hclause -> bag           (** add a hclause to the bag *)

val remove_from_bag : bag -> int -> bag             (** remove the clause from the bag *)

val get_from_bag : bag -> int -> hclause            (** get a clause by its ID *)

val is_in_bag : bag -> int -> bool

val iter_bag : bag -> (int -> hclause -> unit) -> unit  (** iterate on elements of the bag *)

(** for a predicate p, returns (bag of c s.t. p(c)), (bag of c s.t. not p(c)) *)
val partition_bag : bag -> (hclause -> bool) -> bag * bag

val empty_bag : bag

val size_bag : bag -> int         (** number of clauses in the bag (linear) *)

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
val pp_literal_debug : pprinter_literal                 (** use debug unicode syntax *)
val pp_literal_tstp : pprinter_literal                  (** use TSTP syntax *)

val pp_pos : formatter -> position -> unit

(** pretty printer for clauses *)
class type pprinter_clause =
  object
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

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> clause -> unit      (** pretty print proof from clause *)
  end

val pp_proof : pprinter_proof ref                       (** defaut printing of proofs *)
val pp_proof_tstp : pprinter_proof
val pp_proof_debug : pprinter_proof

(** print the content of a bag *)
val pp_bag : Format.formatter -> bag -> unit
