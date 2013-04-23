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

(** {1 Generic term indexing} *)

open Basic

(** A leaf maps terms to a set of 'a *)
module Leaf : sig
  type 'a t

  val empty : cmp:('a -> 'a -> int) -> 'a t
  val add : 'a t -> term -> 'a -> 'a t
  val remove : 'a t -> term -> 'a -> 'a t
  val is_empty : _ t -> bool
  val iter : 'a t -> (term -> 'a SmallSet.t -> unit) -> unit
  val fold : 'a t -> ('b -> term -> 'a SmallSet.t -> 'b) -> 'b -> 'b
  val size : _ t -> int

  val fold_unify : 'a t bind -> term bind -> 'b ->
                    ('b -> term -> 'a -> substitution -> 'b) -> 'b
  val fold_match: 'a t bind -> term bind -> 'b ->
                  ('b -> term -> 'a -> substitution -> 'b) -> 'b
    (** Match the indexed terms against the given query term *)

  val fold_matched: 'a t bind -> term bind -> 'b ->
                    ('b -> term -> 'a -> substitution -> 'b) -> 'b
    (** Match the query term against the indexed terms *)
end

(** A term index, that contains values of type 'a in its leaves *)
type 'a t =
  <
    name : string ;
    add : term -> 'a -> 'a t ;
    remove: term -> 'a -> 'a t ;
    is_empty : bool ;

    iter : (term -> 'a SmallSet.t -> unit) -> unit ;
    fold : 'b. ('b -> term -> 'a SmallSet.t -> 'b) -> 'b -> 'b ;

    retrieve_unifiables : 'b. offset -> term bind -> 'b ->
                          ('b -> term -> 'a -> substitution -> 'b) -> 'b ;
    retrieve_generalizations : 'b. offset -> term bind -> 'b ->
                                ('b -> term -> 'a -> substitution -> 'b) -> 'b ;
    retrieve_specializations : 'b. offset -> term bind -> 'b ->
                              ('b -> term -> 'a -> substitution -> 'b) -> 'b ;

    pp : ?all_clauses:bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> unit -> unit ;
    to_dot : ('a -> string) -> Format.formatter -> unit
      (** print oneself in DOT into the given file *)
  >

(** A subsumption index (non perfect!) *)
type subsumption_t =
  < name : string;
    add : clause -> subsumption_t ;
    add_clauses : clause Sequence.t -> subsumption_t;
    remove : clause -> subsumption_t ;
    remove_clauses : clause Sequence.t -> subsumption_t;
    retrieve_subsuming : literal array -> (hclause -> unit) -> unit ;
    retrieve_subsumed : literal array -> (hclause -> unit) -> unit ;
  >

(** A simplification index *)
type unit_t =
  <
    name : string ;
    maxvar : int ;
    is_empty : bool ;
    add_clause : hclause -> unit_t ;
    remove_clause : hclause -> unit_t ;
    add : term -> term -> bool -> hclause -> unit_t ;
    remove : term -> term -> bool -> hclause -> unit_t ;
    size : int ;
    retrieve : 'a. sign:bool -> offset -> term bind -> 'a ->
              ('a -> term bind -> term bind -> hclause -> substitution -> 'a) -> 'a;
      (** fold on (in)equations of given sign l=r where subst(l) = query term *)

    pp : Format.formatter -> unit -> unit ;
    to_dot : Format.formatter -> unit
      (** print oneself in DOT into the given file *)
  >

val mk_unit_index : (term * hclause) t -> unit_t
  (** From an implementation of a general index, make a unit index. *)
