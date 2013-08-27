
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

type t = int list
  (** A position is a path in a tree *)

type position = t

val left_pos : int
val right_pos : int

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int

val opp : int -> int
  (** Opposite position left->right, right->left *)

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

(** {2 Position builder} *)

module Build : sig
  type t

  val empty : t
    (** Empty builder *)

  val of_pos : position -> t
    (** Start from a given position *)

  val cons : int -> t -> t
    (** Prefix the position *)

  val add : t -> int -> t
    (** Add at the end *)

  val append : t -> position -> t
    (** Append position at the end *)

  val to_pos : t -> position
    (** Extract current position *)

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
end
