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

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

module Make(C : Index.CLAUSE) : sig
  type feature_vector = int list
    (** a vector of feature *)

  (** {2 Features} *)

  module Feature : sig
    type t = {
      name : string;
      f : Index.lits -> int;
    } (** a function that computes a given feature on clauses *)

    val name : t -> string
    val compute : t -> Index.lits -> int
    val pp : Buffer.t -> t -> unit
    val fmt : Format.formatter -> t -> unit

    val sum_of_depths : t                 (** sum of depths of symbols *)
    val size_plus : t                     (** size of positive clause *)
    val size_minus : t                    (** size of negative clause *)
    val count_split_symb : t              (** count number of distinct split symbols *)
    val count_skolem_symb : t             (** count number of distinct skolem symbols *)
    val count_symb_plus : Symbol.t -> t   (** occurrences of symbol in positive clause *)
    val count_symb_minus : Symbol.t -> t  (** occurrences of symbol in negative clause *)
    val max_depth_plus : Symbol.t -> t    (** maximal depth of symb in positive clause *)
    val max_depth_minus : Symbol.t -> t   (** maximal depth of symb in negative clause *)
  end

  val compute_fv : Feature.t list -> Index.lits -> feature_vector

  (** {2 Index} *)

  include Index.SUBSUMPTION_IDX with module C = C

  val empty_with : Feature.t list -> t

  val default_features : Feature.t list

  val features_of_signature : ?ignore:(Symbol.t -> bool) -> 
                              Signature.t -> Feature.t list
    (** Build a set of features from the given signature. Symbols
        that satisfy [ignore] are not considered (default ignores
        connectives) *)

  val of_signature : Signature.t -> t

  val features : t -> Feature.t list
end
