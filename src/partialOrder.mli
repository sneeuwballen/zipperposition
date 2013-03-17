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

open Basic
open Symbols

(** A partial ordering on symbols, as a matrix. It computes the
    transitive closure on the relation it induces, automatically. *)

type t
  (** the partial order *)

val mk_partial_order : symbol list -> t
  (** build an empty partial order for the list of symbols *)

val is_total : t -> bool
  (** is the ordering total? *)

val complete : t -> (symbol -> symbol -> int) -> unit
  (** complete the partial order using the given order on
      symbols to compare unordered pairs. If the given comparison
      function is not total, the ordering may still not be
      complete. The comparison function [f] is assumed to be such
      that [transitive_closure f] is a partial order. *)

val compare : t -> symbol -> symbol -> int
  (** compare two symbols in the ordering. The ordering must be total! *)

val symbols : t -> symbol list
  (** symbols, in decreasing order (assuming the ordering is total) *)

val pp : Format.formatter -> t -> unit
  (** pretty print the partial order as a boolean matrix *)
