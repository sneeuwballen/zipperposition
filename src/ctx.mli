
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

(** {1 Basic context for literals, clauses...} *)

open Logtk

type scope = Substs.scope

type t = private {
  mutable ord : Ordering.t;           (** current ordering on terms *)
  mutable select : Selection.t;       (** selection function for literals *)
  mutable skolem : Skolem.ctx;        (** Context for skolem symbols *)
  mutable signature : Signature.t;    (** Signature *)
  mutable complete : bool;            (** Completeness preserved? *)
  renaming : Substs.Renaming.t;       (** Renaming, always useful... *)
  ac : Theories.AC.t;                 (** AC symbols *)
  total_order : Theories.TotalOrder.t;(** Total ordering *)
}

val create : ?ord:Ordering.t -> ?select:Selection.t ->
              signature:Signature.t -> 
              unit -> t
  (** Fresh new context *)

val ord : ctx:t -> Ordering.t

val compare : ctx:t -> FOTerm.t -> FOTerm.t -> Comparison.t

val select : ctx:t -> Literal.t array -> BV.t

val skolem_ctx : ctx:t -> Skolem.ctx

val signature : ctx:t -> Signature.t

val ac : ctx:t -> Theories.AC.t

val total_order : ctx:t -> Theories.TotalOrder.t

val renaming_clear : ctx:t -> Substs.Renaming.t
  (** Obtain the global renaming. The renaming is cleared before
      it is returned. *)

val lost_completeness : ctx:t -> unit
  (** To be called when completeness is not preserved *)

val is_completeness_preserved : ctx:t -> bool
  (** Check whether completeness was preserved so far *)

val add_signature : ctx:t -> Signature.t -> unit
  (** Merge  the given signature with the context's one *)

val add_ac : ctx:t -> ?proof:Proof.t list -> Symbol.t -> unit
  (** Symbol is AC *)

val add_order : ctx:t -> ?proof:Proof.t list ->
                less:Symbol.t -> lesseq:Symbol.t ->
                Theories.TotalOrder.instance
  (** Pair of symbols that constitute an ordering.
      @return the corresponding instance. *)

val add_tstp_order : ctx:t -> Theories.TotalOrder.instance
  (** Specific version of {!add_order} for $less and $lesseq *)

val declare : ctx:t -> string -> Type.t -> unit
  (** Declare the type of a symbol (updates signature) *)
