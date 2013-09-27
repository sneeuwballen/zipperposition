
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

(** {6 Call external provers with TSTP} *)

(** {2 Description of provers} *)

module Prover : sig
  type t = {
    name : string;                (** name of the prover *)
    command : string;             (** command to call prover*)
    unsat : Str.regexp;           (** prover returned unsat *)
    sat : Str.regexp;             (** prover returned sat *)
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

val call : ?timeout:int -> prover:Prover.t ->
           Ast_tptp.declaration list -> result
  (** Call the prover (if present) on the given problem, and
      return a result. Default timeout is 30. *)

val call_proof : ?timeout:int -> prover:Prover.t ->
                  Ast_tptp.declaration list ->
                  result * Trace_tstp.t option
  (** Call the prover, and also tries to parse a TSTP derivation,
      if the prover succeeded *)
