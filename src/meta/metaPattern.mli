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

(** {1 FOFormula Patterns} *)

open Logtk

(** {2 Main type} *)

type t
type pattern = t

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val pp_apply : Buffer.t -> (t * HOTerm.t list) -> unit
val fmt : Format.formatter -> t -> unit
val debug : Format.formatter -> t -> unit
val bij : t Bij.t

val arbitrary : t QCheck.Arbitrary.t
val arbitrary_apply : (t * HOTerm.t list) QCheck.Arbitrary.t

(** {2 Basic Operations} *)

(** This module is used to handle the encoding of formulas and terms into
    patterns. FOTerms are supposed to be curried. *)

module EncodedForm : sig
  type t
  
  val encode_t : HOTerm.t -> t
  val decode_t : t -> HOTerm.t
  
  val encode : FOFormula.t -> t
  val decode : t -> FOFormula.t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val bij : t Bij.t
  val mapping : t MetaReasoner.Translate.mapping
  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
end

(** Pattern creation and application *)

val create : EncodedForm.t -> t * HOTerm.t list
  (** Create a pattern by abstracting its symbols (which are returned
      as a list of constants) *)

val arity : t -> int
  (** Number of arguments of the pattern *)

val can_apply : t * HOTerm.t list -> bool
  (** Is the application type-safe? *)

val apply : t * HOTerm.t list -> EncodedForm.t
  (** Apply the pattern to the given constants/terms to get back a formula.
      Arity of the pattern must match the length of the list. *)

val mapping : (t * HOTerm.t list) MetaReasoner.Translate.mapping
  (** Bidirectional translation to Datalog literals *)

val matching : t -> EncodedForm.t -> (t * HOTerm.t list) Sequence.t
  (** Match a pattern against an encoded formula. *)

(** {2 Set of patterns} *)

module Set : sig
  type t

  val is_empty : t -> bool

  val empty : t
    (** Empty set *)

  val add : t -> pattern -> t
    (** Add a pattern to the set *)

  val matching : t -> EncodedForm.t -> (pattern * HOTerm.t list) list
    (** Match the given formula against indexed patterns, returning
        instances of patterns of the set. *)

  val to_seq : t -> pattern Sequence.t
  val of_seq : ?init:t -> pattern Sequence.t -> t

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val bij : t Bij.t
end

