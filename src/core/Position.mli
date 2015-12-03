
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

(** {1 Positions in terms, clauses...} *)

type t =
  | Stop
  | Type of t       (** Switch to type *)
  | Left of t       (** Left term in curried application *)
  | Right of t      (** Right term in curried application, and subterm of binder *)
  | Record_field of string * t  (** Field of a record *)
  | Head of t       (** Head of uncurried term *)
  | Arg of int * t  (** argument term in uncurried term, or in multiset *)
  (** A position is a path in a tree *)

type position = t

val stop : t
val left : t -> t
val right : t -> t
val type_ : t -> t
val record_field : string -> t -> t
val head : t -> t
val arg : int -> t -> t

val opp : t -> t
  (** Opposite position, when it makes sense (left/right) *)

val rev : t -> t
  (** Reverse the position *)

val append : t -> t -> t
  (** Append two positions *)

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int

include Interfaces.PRINT with type t := t

(** {2 Position builder} *)

module Build : sig
  type t

  val empty : t
    (** Empty builder (position=[Stop]) *)

  val to_pos : t -> position
    (** Extract current position *)

  val of_pos : position -> t
    (** Start from a given position *)

  val prefix : position -> t -> t
    (** Prefix the builder with the given position *)

  val suffix : t -> position -> t
    (** Append position at the end *)

  (** All the following builders add elements to the {b end}
      of the builder. This is useful when a term is traversed and
      positions of subterms are needed, since positions are
      easier to build in the wrong order (leaf-to-root). *)

  val left : t -> t
    (** Add [left] at the end *)

  val right : t -> t
    (** Add [left] at the end *)

  val type_ : t -> t

  val record_field : string -> t -> t

  val head : t -> t

  val arg : int -> t -> t
    (** Arg position at the end *)

  include Interfaces.PRINT with type t := t
end
