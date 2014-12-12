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

(** {1 LogtkSignature} *)

module SMap : module type of LogtkSymbol.Map

type t = private LogtkType.t SMap.t
  (** A signature maps symbols to types *)

val empty : t
  (** Empty signature *)

val is_empty : t -> bool

val singleton : LogtkSymbol.t -> LogtkType.t -> t

val mem : t -> LogtkSymbol.t -> bool
  (** Is the symbol declared? *)

val declare : t -> LogtkSymbol.t -> LogtkType.t -> t
  (** Declare the symbol, or
      @raise LogtkType.Error if the symbol is already defined with a different type
      @raise Invalid_argument if the type has free variables *)

val find : t -> LogtkSymbol.t -> LogtkType.t option
  (** Lookup the type of a symbol *)

val find_exn : t -> LogtkSymbol.t -> LogtkType.t
  (** Lookup the type of a symbol
      @raise Not_found if the symbol is not in the signature *)

val arity : t -> LogtkSymbol.t -> int * int
  (** Arity of the given symbol, or failure.
      see {!LogtkType.arity} for more details about the returned value.
      @raise Not_found if the symbol is not in the signature *)

val cardinal : t -> int
  (** Number of symbols *)

val is_ground : t -> bool
  (** Only ground types? *)

val merge : t -> t -> t
  (** Merge two signatures together.
      @raise LogtkType.Error if they share some symbols with distinct types *)

val filter : t -> (LogtkSymbol.t -> LogtkType.t -> bool) -> t
  (** Only keep part of the signature *)

val diff : t -> t -> t
  (** [diff s1 s2] contains the symbols of [s1] that do not appear
      in [s2]. Useful to remove base symbols. *)

val well_founded : t -> bool
  (** Are there some symbols of arity 0 in the signature?
      @return true iff the Herbrand term universe of this signature is
        non empty  *)

module Seq : sig
  val symbols : t -> LogtkSymbol.t Sequence.t
  val types : t -> LogtkType.t Sequence.t
  val to_seq : t -> (LogtkSymbol.t * LogtkType.t) Sequence.t
  val of_seq : (LogtkSymbol.t * LogtkType.t) Sequence.t -> t
end

val to_set : t -> LogtkSymbol.Set.t
  (** Set of symbols of the signature *)

val to_list : t -> (LogtkSymbol.t * LogtkType.t) list
val of_list : (LogtkSymbol.t * LogtkType.t) list -> t

val iter : t -> (LogtkSymbol.t -> LogtkType.t -> unit) -> unit

val fold : t -> 'a -> ('a -> LogtkSymbol.t -> LogtkType.t -> 'a) -> 'a

(** {2 IO} *)

include LogtkInterfaces.PRINT with type t := t

(** {2 Pre-defined signature in TPTP} *)

module TPTP : sig
  val base : t
  val is_base_symbol : LogtkSymbol.t -> bool
  val base_symbols : LogtkSymbol.Set.t

  val is_bool : t -> LogtkSymbol.t -> bool
    (** Has the symbol a boolean return sort?
        @raise Not_found if the symbol is not in the signature *)

  val is_not_bool : t -> LogtkSymbol.t -> bool

  val pp_no_base : Buffer.t -> t -> unit
    (** Print the signature, minus the base symbols *)

  (** {3 Arith} *)

  module Arith : sig
    val operators : LogtkSymbol.Set.t

    val is_operator : LogtkSymbol.t -> bool

    val base : t  (** arith op *)
    val full : t  (** arith op + regular op *)
  end
end
