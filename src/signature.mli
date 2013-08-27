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

type t = Type.t Symbol.SMap.t
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

val to_symbols : t -> Symbol.t list
  (** extract the list of symbols from the complete signature *)

val to_set : t -> Symbol.SSet.t
  (** Set of symbols of the signature *)

val to_seq : t -> (Symbol.t * Type.t) Sequence.t
val of_seq : (Symbol.t * Type.t) Sequence.t -> t

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Pre-defined symbols} *)

val base : t
val is_base_symbol : Symbol.t -> bool
val base_symbols : Symbol.SSet.t

