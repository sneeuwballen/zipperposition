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

val eq_literal : literal -> literal -> bool       (** equality of literals *)
val compare_literal : literal -> literal -> int   (** lexicographic comparison of literals *)
val hash_literal : literal -> int                 (** hashing of literal *)

(** build literals. If sides so not have the same sort,
    a SortError will be raised. An ordering must be provided *)
val mk_eq : ord:ordering -> foterm -> foterm -> literal
val mk_neq : ord:ordering -> foterm -> foterm -> literal
val mk_lit : ord:ordering -> foterm -> foterm -> bool -> literal
val reord_lit : ord:ordering -> literal -> literal  (** recompute order *)

val apply_subst_lit : ?recursive:bool -> ord:ordering -> substitution -> literal -> literal

val negate_lit : literal -> literal (** negate literal *)
val fmap_lit : ord:ordering -> (foterm -> foterm) -> literal -> literal (** fmap in literal *)
val vars_of_lit : literal -> varlist (** gather variables *)

val lit_to_multiset : literal -> foterm list list (** literal to multiset of terms *)

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

val mk_clause : literal list -> proof Lazy.t -> clause  (** build a clause *)
val reord_clause : ord:ordering -> clause -> clause     (** recompute order *)

val apply_subst_cl : ?recursive:bool -> ord:ordering -> substitution -> clause -> clause

val get_lit : clause -> int -> literal                  (** get the literal at given index *)
val get_pos : clause -> position -> foterm              (** get the subterm at position *)

(** rename a clause w.r.t. maxvar (all variables inside will be > maxvar) *)
val fresh_clause : ord:ordering -> int -> clause -> clause * int  
(** rename clause w.r.t. varlist *)
val relocate_clause : ord:ordering -> varlist -> clause -> clause       
(** normalize (vars start at 1) *)
val normalize_clause : ord:ordering -> clause -> clause                 

(* ----------------------------------------------------------------------
 * bag of clauses
 * ---------------------------------------------------------------------- *)

(** set of hashconsed clauses *)
type bag = {
  bag_maxvar: int;                (** higher bound for the biggest var index *)
  bag_clauses : hclause Ptmap.t;  (** map tag -> hashconsed clause *)
}


val add_to_bag : bag -> clause -> bag * hclause   (** add the clause to the bag, hashconsing it *)
val add_hc_to_bag : bag -> hclause -> bag         (** add a hclause to the bag *)

val remove_from_bag : bag -> int -> bag           (** remove the clause from the bag *)

val get_from_bag : bag -> int -> hclause          (** get a clause by its ID *)

val is_in_bag : bag -> int -> bool

(** for a predicate p, returns (bag of c s.t. p(c)), (bag of c s.t. not p(c)) *)
val partition_bag : bag -> (hclause -> bool) -> bag * bag

val empty_bag : bag

val size_bag : bag -> int         (** number of clauses in the bag (linear) *)

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

open Format

val size_bag : bag -> int
val pp_clause : ?sort:bool -> formatter -> clause -> unit
val pp_pos : formatter -> position -> unit
val pp_clause_pos : formatter -> (clause * position) -> unit
val pp_hclause : formatter -> hclause -> unit
val pp_hclause_pos : formatter -> (hclause * position* foterm) -> unit
val pp_bag: formatter -> bag -> unit
val pp_proof: subst:bool -> formatter -> proof -> unit
val pp_clause_proof : formatter -> clause -> unit   (** print clause and its proof *)
val pp_proof_rec : formatter -> clause -> unit      (** also print premisses, recursively*)
val pp_tstp_clause : formatter -> clause -> unit    (** print as TSTP clause *)
val pp_tstp_proof : formatter -> clause -> unit    (** print the proof recursively in TSTP *)
