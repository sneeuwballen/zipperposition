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

(** {2 Indexing on patterns} *)

type +'a t
  (** the type of the map that has values of type 'a *)

val empty : 'a t

val add : Pattern.pattern -> 'a -> 'a t -> 'a t

val fold : (Pattern.pattern -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val retrieve : 'a t -> literal array ->
               (Pattern.pattern -> term list -> 'a -> unit) -> unit
  (** [retrieve map lits k] calls [k] on every list [l] of terms
      such that [l = matching lits p] for some [p] that is a key of [map].
      [k] receives as arguments the pattern [p], the arguments [l]
      and the value associated to [p] *)

val to_seq : 'a t -> (Pattern.pattern * 'a) Sequence.t
val of_seq : 'a t -> (Pattern.pattern * 'a) Sequence.t -> 'a t

val to_json : ('a -> json) -> 'a t -> json
val of_json : (json -> 'a) -> 'a t -> json -> 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

