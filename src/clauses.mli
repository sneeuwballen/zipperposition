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

val pos_lit : literal -> bool                       (** is the literal positive? *)
val neg_lit : literal -> bool                       (** is the literal negative? *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : ord:ordering -> foterm -> foterm -> literal
val mk_neq : ord:ordering -> foterm -> foterm -> literal
val mk_lit : ord:ordering -> foterm -> foterm -> bool -> literal
val reord_lit : ord:ordering -> literal -> literal  (** recompute order *)
val lit_of_fof : ord:ordering -> literal -> literal (** translate eq/not to literal *)
val term_of_lit : literal -> foterm                 (** translate lit to term *)

val apply_subst_lit : ?recursive:bool -> ord:ordering -> substitution -> literal -> literal

val negate_lit : literal -> literal (** negate literal *)
val fmap_lit : ord:ordering -> (foterm -> foterm) -> literal -> literal (** fmap in literal *)
val vars_of_lit : literal -> varlist (** gather variables *)

val lit_to_multiset : literal -> foterm list (** literal to multiset of terms *)

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

module H : Hashcons.S with type key = clause
val clauses : H.t

val eq_clause : clause -> clause -> bool                (** equality of clauses *)
val compare_clause : clause -> clause -> int            (** lexico order on clauses *)

val hashcons_clause : clause -> hclause

val eq_hclause : hclause -> hclause -> bool             (** equality of hashconsed clauses *)
val compare_hclause : hclause -> hclause -> int         (** simple order on lexico clauses *)

val mk_clause : ord:ordering -> literal list ->
                selected:int list Lazy.t ->
                proof Lazy.t ->
                clause                                  (** build a clause *)
val clause_of_fof : ord:ordering -> clause -> clause    (** transform eq/not to literals *)
val reord_clause : ord:ordering -> clause -> clause     (** recompute order *)
val select_clause : select:selection_fun
                 -> clause -> clause                    (** select literals in clause *)
val maxlits : clause -> (literal * int) list            (** indexed list of max literals *)
val selected: clause -> int list                        (** indexes of selected literals *)
val check_maximal_lit : ord:ordering -> clause -> int   (** is the i-th literal *)
                    -> substitution -> bool             (** maximal in subst(clause)? *)

val apply_subst_cl : ?recursive:bool -> ord:ordering -> substitution -> clause -> clause

val get_lit : clause -> int -> literal                  (** get the literal at given index *)
val get_pos : clause -> position -> foterm              (** get the subterm at position *)

(** rename a clause w.r.t. maxvar (all variables inside will be > maxvar) *)
val fresh_clause : ord:ordering -> int -> clause -> clause * int  
(** rename clause w.r.t. varlist *)
val relocate_clause : ord:ordering -> varlist -> clause -> clause       
(** normalize (vars start at 1) *)
val normalize_clause : ord:ordering -> clause -> clause                 

(** check whether a literal is selected *)
val selected_lit : clause -> int -> bool
(** get the list of selected literals *)
val selected_lits : clause -> (literal * int) list
(** check whether a literal is eligible for resolution *)
val eligible_res : ord:ordering -> clause -> int -> substitution -> bool
(** check whether a literal is eligible for paramodulation *)
val eligible_param : ord:ordering -> clause -> int -> substitution -> bool

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
val string_of_direction : direction -> string

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
    method pp_h_pos : Format.formatter -> (hclause * position * foterm) -> unit
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
