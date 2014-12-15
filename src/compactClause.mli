(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Compact clause representation} *)

open Logtk

type form = Formula.FO.t

type bool_lit =
  bool *
  [ `Box_clause of Literal.t array
  | `Qbf_artifact of int * string
  ]
(** A boolean literal, here, is a boxed (unsplittable) clause
    with a sign. The literal can be an explicit encoding of "lits are true"
    or some other QBF artifact (lit number + repr) *)

type t = {
  lits : Literal.t array;
  trail : bool_lit list;
}

val cmp : t -> t -> int
include Interfaces.HASH with type t := t

val compare : t -> t -> int

val make : Literal.t array -> bool_lit list -> t
(** Make a compact clause *)

val is_empty : t -> bool
val has_absurd_lits : t -> bool

val iter : t -> (Literal.t -> unit) -> unit

val to_seq : t -> Literal.t Sequence.t

val to_forms : t -> form array

val lits : t -> Literal.t array
val trail : t -> bool_lit list

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit

val to_string : t -> string
val fmt : Format.formatter -> t -> unit
