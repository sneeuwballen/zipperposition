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

(** {1 Partial Ordering on symbols} *)

(** A partial ordering on symbols, as a matrix. It computes the
    transitive closure on the relation it induces, automatically. *)

type t
  (** the partial order *)

val mk_partial_order : Symbol.t list -> t
  (** build an empty partial order for the list of symbols *)

val is_total : t -> bool
  (** is the ordering total? *)

val complete : t -> (Symbol.t -> Symbol.t -> int) -> unit
  (** complete the partial order using the given order on
      symbols to compare unordered pairs. If the given comparison
      function is not total, the ordering may still not be
      complete. The comparison function [f] is assumed to be such
      that [transitive_closure f] is a partial order. *)

val compare : t -> Symbol.t -> Symbol.t -> int
  (** compare two symbols in the ordering. The ordering must be total! *)

val symbols : t -> Symbol.t list
  (** symbols, in decreasing order (assuming the ordering is total) *)

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
