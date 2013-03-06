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

(** {1 Term rewriting} *)

open Types

(** {2 Ordered rewriting} *)

module OrderedTRS : sig
  type t

  val create : ord:ordering -> t
  
  val add_clause : t -> hclause -> unit
  val add_seq : t -> hclause Sequence.t -> unit
  
  val to_seq : t -> hclause Sequence.t

  val size : t -> int
  
  val mk_rewrite : t -> size:int -> (term -> term)
    (** Given a TRS and a cache size, build a memoized function that
        performs term rewriting *)

  val pp : Format.formatter -> t -> unit
end

(** {2 Regular rewriting} *)

module TRS : sig
  type rule = (term * term)

  type t

  val create : unit -> t
  val add_rule : t -> rule -> unit
  val add_rules : t -> rule list -> unit
  val from_list : rule list -> t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit

  val pp_trs : Format.formatter -> t -> unit
  val pp_trs_index : Format.formatter -> t -> unit

  val rewrite : t -> term -> term
    (** Compute normal form of the term *)
end
