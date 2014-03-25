
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

(** {6 AC redundancy} *)

open Logtk

type spec = Theories.AC.t

val axioms : ctx:Ctx.t -> Symbol.t -> Type.t -> Clause.t list
  (** List of (persistent) axioms that are needed for simplifications to
      be complete for the given symbol. The [ctx] is required for type inference
      and building clauses . *)

(** {2 Rules} *)

val is_trivial_lit : spec:spec -> Literal.t -> bool
  (** Is the literal AC-trivial? *)

val is_trivial : spec:spec -> Clause.t -> bool
  (** Check whether the clause is AC-trivial *)

val simplify : spec:spec -> ctx:Ctx.t -> Clause.t -> Clause.t
  (** Simplify the clause modulo AC *)

val add_ac : ?proof:Proof.t list -> env:Env.t -> Symbol.t -> Type.t -> unit
  (** Declare that the given symbol is AC, and update the Env subsequently
      by adding clauses, etc. *)

val setup_penv : penv:PEnv.t -> unit

val setup_env : env:Env.t -> unit
