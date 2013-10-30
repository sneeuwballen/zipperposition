(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Signature} *)

type t = Type.t Symbol.Map.t
  (** A signature maps symbols to types *)

val empty : t
  (** Empty signature *)

val mem : t -> Symbol.t -> bool
  (** Is the symbol declared? *)

val declare : t -> Symbol.t -> Type.t -> t
  (** Declare the symbol, or
      @raise Invalid_argument if the symbol is already defined with
             a different type *)

val find : t -> Symbol.t -> Type.t
  (** Lookup the type of this symbol, or
      @raise Not_found if the symbol is not in the signature *)

val arity : t -> Symbol.t -> int
  (** Arity of the given symbol, or
      @raise Not_found if the symbol is not in the signature *)

val cardinal : t -> int
  (** Number of symbols *)

val is_ground : t -> bool
  (** Only ground types? *)

val is_bool : t -> Symbol.t -> bool
  (** Has the symbol a boolean return sort?
      @raise Not_found if the symbol is not in the signature *)

val is_not_bool : t -> Symbol.t -> bool

val merge : t -> t -> t
  (** Merge two signatures together *)

val map : t -> (Symbol.t -> Type.t -> Type.t) -> t
  (** Transform types *)

val filter : t -> (Symbol.t -> Type.t -> bool) -> t
  (** Only keep part of the signature *)

val diff : t -> t -> t
  (** [diff s1 s2] contains the symbols of [s1] that do not appear
      in [s2]. Useful to remove base symbols. *)

val size : t -> int
  (** Number of symbols *)

val well_founded : t -> bool
  (** Are there some symbols of arity 0 in the signature?
      @return true iff the Herbrand term universe of this signature is
        non empty  *)

val curry : t -> t
  (** Curry all types occurring in the signature *)

val uncurry : t -> t
  (** Uncurry all types occurring in the signature *)

val to_symbols : t -> Symbol.t list
  (** extract the list of symbols from the complete signature *)

val to_set : t -> Symbol.Set.t
  (** Set of symbols of the signature *)

val iter : t -> (Symbol.t -> Type.t -> unit) -> unit

val fold : t -> 'a -> ('a -> Symbol.t -> Type.t -> 'a) -> 'a

val to_seq : t -> (Symbol.t * Type.t) Sequence.t
val of_seq : (Symbol.t * Type.t) Sequence.t -> t

val to_list : t -> (Symbol.t * Type.t) list
val of_list : (Symbol.t * Type.t) list -> t

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val pp_no_base : Buffer.t -> t -> unit
  (** Print the signature, minus the base symbols *)

val arbitrary : t QCheck.Arbitrary.t  (* arbitrary well-founded signature *)
val arbitrary_ground : t QCheck.Arbitrary.t  (* only ground types *)

(** {2 Pre-defined symbols} *)

val base : t
val is_base_symbol : Symbol.t -> bool
val base_symbols : Symbol.Set.t

(** {2 Arith} *)

module Arith : sig
  val operators : Symbol.t list

  val is_operator : Symbol.t -> bool

  val signature : t
end
