
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

(** {1 Rewriting on HO terms} *)

open Logtk

type term = HOTerm.t
type rule = term * term

(** {2 Building rewrite systems} *)

type t
  (** rewrite system *)

val empty : t
  (** No rules *)

val add : t -> rule -> t
  (** Add a rule *)

module Seq : sig
  val of_seq : t -> rule Sequence.t -> t
  val to_seq : t -> rule Sequence.t
end

val of_list : rule list -> t
val to_list : t -> rule list

include Interfaces.PRINT with type t := t
include Interfaces.ORD with type t := t
include Interfaces.HASH with type t := t

(** {2 Normalizing terms} *)

val normalize : t -> term -> term
  (** Normalize  the term w.r.t to the rewrite system *)

val normalize_collect : t -> term -> term * rule list
  (** Normalize the term, and returns a list of rules used to normalize it. *)
