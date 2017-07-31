
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Call external provers with TSTP (Old)} *)

(** This module is intended to provide a uniform interface to invoke
    some classic first-order provers (E, SPASS, …) on a problem
    specified as a TPTP Ast.

    The point is that this AST might be generated programmatically,
    or manipulated from an existing TSTP proof (for checking purpose),
    rather than being handled as text.
*)

open Logtk

type 'a or_error = ('a, string) CCResult.t
type untyped = STerm.t

module A = Ast_tptp

(** {2 Description of provers} *)

module Prover : sig
  type t = {
    name : string;                (** name of the prover *)
    command : string;             (** command to call prover*)
    unsat : string list;          (** prover returned unsat (possible outputs)*)
    sat : string list;            (** prover returned sat (possible outputs)*)
  } (** data useful to invoke a prover. The prover must read from
        stdin. The command is interpolated using {! Buffer.add_substitude}, with
        the given patterns:

        - "timeout" is the timeout in seconds *)

  val lookup : string -> t
  (** Lookup a prover by its name.
      @raise Not_found if the prover is not registered. *)

  val list_provers : unit -> string list
  (** List of registered provers *)

  val register : string -> t -> unit
  (** Register the prover with the given name.
      @raise Invalid_argument if the name is already used. *)

  val p_E : t

  val p_Eproof : t

  val p_SPASS : t

  val p_Zenon : t

  val default : t list
end

val name : Prover.t -> string
(** Name of the prover *)

(** {2 Run provers} *)

type result =
  | Unsat
  | Sat
  | Unknown
  | Error of string

val call : ?timeout:int -> ?args:string list ->
  prover:Prover.t ->
  untyped A.t list ->
  result or_error
(** Call the prover (if present) on the given problem, and
    return a result. Default timeout is 30. *)

val call_proof : ?timeout:int -> ?args:string list ->
  prover:Prover.t ->
  untyped A.t list ->
  (result * Trace_tstp.t) or_error
(** Call the prover, and also tries to parse a TSTP derivation,
    if the prover succeeded *)

val call_with_out : ?timeout:int -> ?args:string list ->
  prover:Prover.t ->
  untyped A.t list ->
  (result * string) or_error
(** Same as {!call}, but also returns the raw output of the prover *)

(** {2 E-prover specific functions} *)

module Eprover : sig
  type result = {
    answer : szs_answer;
    output : string;
    decls : untyped A.t Sequence.t option;
    proof : Trace_tstp.t option;
  }
  and szs_answer =
    | Theorem
    | CounterSatisfiable
    | Unknown

  val string_of_answer : szs_answer -> string

  val run_eproof : steps:int -> input:string -> result or_error
  (** Run Eproof_ram, and tries to read a proof back. *)

  val run_eprover : ?opts:string list -> ?level:int ->
    steps:int -> input:string -> unit -> result or_error
  (** Runs E with the given input (optional verbosity level). The returned
      result will not contain a proof.
      [opts] is an additional list of command line options that will be
      given to E. *)

  val discover : ?opts:string list -> steps:int ->
    untyped A.t Sequence.t ->
    untyped A.t Sequence.t or_error
  (** explore the surrounding of this list of declarations, returning the
      TPTP output of E *)

  val cnf : ?opts:string list ->
    untyped A.t Sequence.t ->
    untyped A.t Sequence.t or_error
    (** Use E to convert a set of statements into CNF *)
end
