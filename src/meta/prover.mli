
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

(** {1 Meta-Prover} *)

open Logtk
open Logtk_parsers

type 'a or_error = [`Error of string | `Ok of 'a]

type t
  (** Meta-prover *)

type clause = Reasoner.clause

val empty : t
  (** Fresh meta-prover (using default plugins' signature) *)

val reasoner : t -> Reasoner.t
  (** The inner reasoner, holding a set of clauses and facts *)

val signature : t -> Signature.t
  (** Current signature *)

val add : t -> Reasoner.clause -> t * Reasoner.consequence Sequence.t
  (** Add a clause *)

val add_fact : t -> Reasoner.fact -> t * Reasoner.consequence Sequence.t

val add_fo_clause : t -> Encoding.foclause ->
                    t * Reasoner.consequence Sequence.t
  (** Add a first-order clause (as "holds" predicate) *)

val add_signature : t -> Signature.t -> t
  (** Add the given signature *)

module Seq : sig
  val to_seq : t -> Reasoner.clause Sequence.t
  val of_seq : t -> Reasoner.clause Sequence.t -> t * Reasoner.consequence Sequence.t
end

(** {6 IO} *)

val of_ho_ast : t -> Ast_ho.t Sequence.t ->
                (t * Reasoner.consequence Sequence.t) or_error
  (** Add the given declarations to the meta-prover. In case of type
     inference failure, or other mismatch, an error is returned. *)

val parse_file : t -> string ->
                (t * Reasoner.consequence Sequence.t) or_error
  (** Attempt to parse file using {!Logtk_parsers.Parse_ho} and add the
      parsed content to the prover *)

