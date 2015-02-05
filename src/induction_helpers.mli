
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Common stuff for Induction}

Also registers some CLI options *)

type term = Logtk.FOTerm.t
type sym = Logtk.Symbol.t

val ind_types : unit -> (string * string list) list
(** List of [ty, constructors] *)

val cover_set_depth : unit -> int
(** Depth for generating cover sets *)

val is_constructor : sym -> bool
(** Is this symbol a registered inductive constructor? *)

val on_enable : unit Signal.t
(** Triggered if induction is enabled *)

val constr_cstors : sym -> sym -> Logtk.Comparison.t
(** Partial order on symbols, where constructors are smaller than other
    symbols *)

val init_from_decls :
  (string * Logtk_parsers.Ast_tptp.optional_info) Sequence.t -> unit
(** Initialize from a bunch of declarations' optional info, if one takes
    only pairs [(some_type_name : $tType, info)] *)

module Make(Ctx : Ctx_intf.S) : sig
  val declare_types : unit -> unit
  (** Declare the list of [ind_types ()] to the given context's *)

  val is_a_constructor : term -> bool
  (** Is the term the application of a constructor? *)

  val find_inductive_cst : Literals.t -> term Sequence.t
  (** Potential inductive constants in those literals *)

  val constr_sub_cst : sym -> sym -> Logtk.Comparison.t
  (** constants are bigger than their sub-constants *)
end
