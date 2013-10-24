
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

(** {1 Intervals on integers} *)

open Logtk

(** {2 Signature a numberic type needs providing} *)

module type NUM = sig
  type t

  val compare : t -> t -> int
    (** Total order on numbers *)

  val pp : Buffer.t -> t -> unit
    (** Print into buffer *)
end

module type S = sig
  module N : NUM
    (** Number the interval is composed of *)

  type t

  val empty : t
    (** Empty interval. It doesn't contain any value *)

  val all : t
    (** Interval that contains all numbers *)

  val lt : N.t -> t
    (** Values strictly lower than this number *)

  val leq : N.t -> t
    (** Values lower or equal than this number *)

  val gt : N.t -> t
    (** Values greater than this number *)

  val geq : N.t -> t
    (** Infinite interval of all values bigger or equal than the number *)

  val range : N.t -> N.t -> t
    (** Make an interval that ranges from [low] to [high], both included. *)

  val mem : t -> N.t -> bool
    (** Is the number part of the interval? *)

  val inter : t -> t -> t
    (** Intersection of two intervals. *)

  val union : t -> t -> t
    (** Union of two intervals. This may lose precision if the two intervals
        are non-overlapping (e.g., [1,2] union [3,4] will give [1,4]. *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

  val arbitrary : N.t QCheck.Arbitrary.t -> t QCheck.Arbitrary.t
end

module Make(N : NUM) : S with module N = N

module Int : S with type N.t = Big_int.big_int

module Rat : S with type N.t = Ratio.ratio

module Real : S with type N.t = float
