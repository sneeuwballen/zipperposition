
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

(** {1 Skolem symbols} *)

type ctx
  (** Context needed to create new symbols *)

val create : ?prefix:string -> unit -> ctx
  (** New skolem contex. A prefix can be provided, which will be
      added to all newly created skolem symbols *)

val fresh_sym : ctx:ctx -> Symbol.t
  (** Just obtain a fresh skolem symbol *)

val fresh_var : ctx:ctx -> int
  (** Unique index for universal variables *)

val update_var : ctx:ctx -> Term.t -> unit
  (** Avoid collisions with variables of this term in calls to {!fresh_var}. *)

val skolem_term : ctx:ctx -> Term.t -> Term.t
  (** Skolemize the given term at root (assumes it occurs just under an
      existential quantifier, whose De Bruijn variable is replaced
      by a fresh symbol applied to free variables). This also
      caches symbols, so that the same term is always skolemized
      the same way.
      
      For instance, [skolem_term ~ctx p(a, b, db0, X)] will yield
      something like [p(a, b, sk42(X), X)].
      *)

val skolem_form : ctx:ctx -> var:Term.t -> Formula.t -> Formula.t
  (** Skolemize the variable [var] in this formula. *)
