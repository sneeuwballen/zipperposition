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

(** {1 Formula Patterns} *)

type t

type pattern = t

(** {2 Basics} *)

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int

val pp : Buffer.t -> t -> unit
val pp_apply : Buffer.t -> (t * Term.t list) -> unit
val fmt : Format.formatter -> t -> unit
val bij : t Bij.t

val encode : Term.t -> Term.t
  (** Encode the term into a curryfied, guarded term suitable for pattern matching *)

val decode : Term.t -> Term.t
  (** Inverse of {! encode} *)

(** {2 Fundamental operations} *)

val create : Term.t -> t * Term.t list
  (** Create a pattern by abstracting its symbols (which are returned
      as a list of constants) *)

val arity : t -> int
  (** Number of arguments of the pattern *)

val apply : t * Term.t list -> Term.t
  (** Apply the pattern to the given constants/terms to get back a formula.
      Arity of the pattern must match the length of the list. *)

val mapping : (t * Term.t list) MetaReasoner.Translate.mapping
  (** Bidirectional translation to Datalog terms *)

val matching : t -> Term.t -> (t * Term.t list) Sequence.t
  (** Match a pattern against an encoded term *)

(** {2 Set of patterns} *)

module Set : sig
  type t

  val is_empty : t -> bool

  val empty : t
    (** Empty set *)

  val add : t -> pattern -> t
    (** Add a pattern to the set *)

  val matching : t -> Term.t -> (pattern * Term.t list) list
    (** Match the given formula against indexed patterns, returning
        instances of patterns of the set. *)

  val to_seq : t -> pattern Sequence.t
  val of_seq : ?init:t -> pattern Sequence.t -> t

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val bij : t Bij.t
end

