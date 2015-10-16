
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

(** {1 Clause context}

A clause with a "hole" in it. Filling the whole with a term [t] is called
"applying the context to [t]".

The point is to relate different applications of the same context. *)

type term = Logtk.FOTerm.t
type scope = Logtk.Substs.scope
type subst = Logtk.Substs.t

(** A context is represented as a regular array of literals, containing
at least one specific variable [x], paired with this variable [x].
Applying the context is a mere substitution *)
type t = private {
  lits : Literals.t;
  var : term;
}

val compare : t -> t -> int
val equal : t -> t -> bool

val make : Literals.t -> var:term -> t
(** Make a context from a var and literals containing this var.
    @raise Assert_failure if the variable isn't present in any literal *)

val extract : Literals.t -> term -> t option
(** [extract lits t] returns [None] if [t] doesn't occur in [lits]. Otherwise,
    it creates a fresh var [x], replaces [t] with [x] within [lits], and
    returns the corresponding context.
    Basically, if [extract lits t = Some c] then [apply c t = lits] *)

val extract_exn : Literals.t -> term -> t
(** Unsafe version of {!extract}.
    @raise Invalid_argument if the term is not to be found within the lits *)

val apply : t -> term -> Literals.t
(** [apply c t] fills the hole of [c] with the given term [t]. [t] and [c]
    share no free variables. *)

val apply_same_scope : t -> term -> Literals.t
(** Same as {!apply}, but now variables from the context and variables
    from the term live in the same scope *)

val raw_lits : t -> Literals.t
(** give access to the underlying literals. Careful not to depend
    on the variable's actual name. *)

(*
val matching : t -> Literals.t -> term option
(** Match the context against a proper clause. On success, [matching ctx c]
    returns a term [Some t] such that [extract c t = ctx],
    and [apply ctx t = c] *)
FIXME: is this even doable?*)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit

(** {2 Sets of contexts} *)

module Set : Set.S with type elt = t
