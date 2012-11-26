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

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

open Types
open Symbols

(** a vector of feature *)
type feature_vector = int list


(** a function that computes a feature *)
type feature = clause -> int

(** use some features to compute a feature vector *)
val compute_fv : feature list -> clause -> feature_vector

val feat_size_plus : feature              (** size of positive clause *)
val feat_size_minus : feature             (** size of negative clause *)
val count_symb_plus : symbol -> feature   (** occurrences of symbol in positive clause *)
val count_symb_minus : symbol -> feature  (** occurrences of symbol in negative clause *)
val max_depth_plus : symbol -> feature    (** maximal depth of symb in positive clause *)
val max_depth_minus : symbol -> feature   (** maximal depth of symb in negative clause *)

type fv_index  (** a feature vector index, based on a trie *)

val mk_fv_index : feature list -> fv_index          (** create an index from features *)
val mk_fv_index_signature : symbol list -> fv_index (** create an index from signature *)

val index_clause : fv_index -> clause -> fv_index   (** add the clause to the index *)
val remove_clause : fv_index -> clause -> fv_index  (** remove the clause from the index *)

val retrieve_subsuming : fv_index -> clause ->
                         (clause -> unit) -> unit   (** clauses that subsume c *)
val retrieve_subsumed : fv_index -> clause ->
                        (clause -> unit) -> unit    (** clauses subsumed by c *)

