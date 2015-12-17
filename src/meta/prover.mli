
(* This file is free software, part of Logtk. See file "license" for more details. *)

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

