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

(** {1 Indexed set of clauses} *)

type t
  (** A set of clauses with annotation of type 'a *)


type clause_pos = clause * position * term
type rewrite_rule = clause * term * term

(** {2 Maintainance of a set} *)

val create : ?sup_into : clause_pos Index.t ->
             ?sup_from : clause_pos Index.t ->
             ?demod_from : rewrite_rule Index.t -> (* demodulate from set of clauses *)
             ?demo_into : clause_pos Index.t ->
             ?simplify_reflect_from : clause_pos Index.t ->
             ?fv : FeatureVector.t ->
             unit -> t
  (** Create an empty set, with the given indexes *)

val is_empty : t -> bool
  (** Is the set empty? *)

val mem : t -> clause -> bool
  (** Does the clause belong to the set? *)

val get : t -> int -> clause
  (** retrieve clause by its ID, or raise Not_found *)

val copy : t -> t
  (** Fresh copy of the set *)

val add : t -> clause -> unit
  (** Add the clause to the set *)

val add_gen : t -> clause Gen.t -> unit
  (** Add clauses *)

val remove : t -> clause -> unit
  (** Remove a clause *)

val remove_gen : t -> clause Gen.t -> unit
  (** Remove clauses *)

val gen : t -> clause Gen.t
  (** Iterate on the clauses *)

val size : t -> int
  (** Number of clauses *)

(** {2 Access indexes} *)

(** The clause set contains indexes, that have been provided at its
    creation. If one tries to access an absent index, Not_found will
    be raised. *)

val sup_into : t -> clause_pos Index.t
  (** Index for superposition into a set *)

val sup_from : t -> clause_pos Index.t
  (** Index for superposition from a set *)

val demod_from : t -> rewrite_rule Index.t
  (** Demodulate a clause with a set *)

val demo_into : t -> clause_pos Index.t
  (** Backward demodulation of a set by a clause *)

val simplify_reflect_from : t -> clause_pos Index.t
  (** Negative unit clauses used for simplify reflect *)

val fv : FeatureVector.t
  (** Feature vector for backward and forward subsumption *)
