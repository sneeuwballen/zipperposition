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

type 'a t
  (** Abstract type for an index with payload of type 'a *)

val empty : name:string -> cmp:('a -> 'a -> int) -> 'a t
  (** Empty index, with the given comparison function on payload *)

val add : 'a t -> term -> 'a -> 'a t
  (** Add the payload for this term *)

val remove : 'a t -> term -> 'a -> 'a t
  (** Remove the payload *)

val iter : (term -> 'a -> unit) -> 'a t -> unit
  (** Iterate on bindings *)

val fold : ('b -> term -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold on bindings *)

val retrieve_unifiables : 'a t bind -> term bind -> 'b ->
                          ('b -> term -> subst -> 'a -> 'b) -> 'b
  (** Retrieve terms unifiable with the query *)

val retrieve_generalizations : 'a t bind -> term bind -> 'b ->
                              ('b -> term -> subst -> 'a -> 'b) -> 'b
  (** Retrieve terms matching the query *)

val retrieve_specializations : 'a t bind -> term bind -> 'b ->
                              ('b -> term -> subst -> 'a -> 'b) -> 'b
  (** Retrieve terms matched by the query *)

val to_dot : ('a -> string) -> Format.formatter -> 'a t -> unit
  (** Print the structure to DOT *)

