
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

(** {6 Formulas with Proofs} *)

open Logtk

type t = {
  form : Formula.t;
  proof : Proof.t;
}

type pform = t

val get_form : t -> Formula.t
val get_proof : t -> Proof.t

val eq : t -> t -> bool
val hash : t -> int

val create : Formula.t -> Proof.t -> t

val of_sourced : Formula.sourced_form -> t
val to_sourced : t -> Formula.sourced_form option

val signature : t -> Signature.t
val signature_seq : ?init:Signature.t -> t Sequence.t -> Signature.t

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

(** {2 Set of formulas} *)

module FSet : sig
  type t

  val create : unit -> t

  val add : t -> pform -> unit

  val eq : t -> t -> bool

  val flatMap : t -> (pform -> pform list) -> t

  val remove : t -> pform -> unit

  val iter : t -> (pform -> unit) -> unit

  val of_seq : ?init:t -> pform Sequence.t -> t

  val to_seq : t -> pform Sequence.t

  val of_list : pform list -> t

  val to_list : t -> pform list

  val size : t -> int
end

(** {2 Transformations} *)

module TransformDag : Transform.DAG with type Form.t = pform
