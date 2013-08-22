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

(** {1 Term rewriting} *)

(** {2 Ordered rewriting} *)

(** Although this module is parametrized by an EQUATION
    module, it only deals with positive equations. Negative
    equations will be discarded. *)

module type ORDERED = sig
  type t

  module E : Index.EQUATION

  val empty : ord:Orderings.t -> t
  
  val add : t -> E.t -> t
  val add_seq : t -> E.t Sequence.t -> t
  val add_list : t -> E.t list -> t
  
  val to_seq : t -> E.t Sequence.t

  val size : t -> int
  
  val mk_rewrite : t -> size:int -> (Term.t -> Term.t)
    (** Given a TRS and a cache size, build a memoized function that
        performs term rewriting *)
end

module MakeOrdered(E : Index.EQUATION) : ORDERED with module E = E

(** {2 Regular rewriting} *)

module TRS : sig
  type t

  type rule = Term.t * Term.t
    (** rewrite rule, from left to right *)

  val empty : t 

  val add : t -> rule -> t
  val add_seq : t -> rule Sequence.t -> t
  val add_list : t -> rule list -> t

  val to_seq : t -> rule Sequence.t

  val size : t -> int
  val iter : t -> (rule -> unit) -> unit

  val rewrite : t -> Term.t -> Term.t
    (** Compute normal form of the term *)
end
