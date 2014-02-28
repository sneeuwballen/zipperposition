
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

(** {1 Specialized plugins}

A plugin is a bridge between OCaml code and some meta-level property
of the prover. For instance, a default plugin is bound to the predicate
"holds", which states that some (encoded) clause holds in the current
problem. The plugin for "holds" provides a view centered on the "holds"
predicate, and allows to add "holds" facts and filter on them.
Plugins can handle some inner type that can be encoded/decoded
to meta-prover-compatible types. *)

open Logtk

type term = Reasoner.term

(** Core features of a plugin, that don't depend on its type parameter *)
class type core = object
  method signature : Signature.t
    (** Signature of symbols used *)

  method owns : term -> bool
    (** Does this term belong to the plugin? *)
end

class type ['a] t = object
  inherit core

  method clauses : Reasoner.clause list
    (** Initial clauses to add *)

  method to_fact : 'a -> term
    (** Encode an 'a value to a fact *)

  method of_fact : term -> 'a option
    (** Decode a fact into an 'a value, if it actually belongs
        to the plugin *)
end

type foclause = Encoding.foclause

module Set : sig
  type t
  val empty : t
  val add : t -> core -> t
  val signature : t -> Signature.t
end

(** {2 Builtin plugins} *)

val holds : foclause t
  (** holds: statement about which First-Order clause is true in the
      current problem *)

val axiom : (string * Type.t list * term) t
  (** axioms present in the problem (name of axiom+type args+argument) *)

val theory : (string * Type.t list * term) t
  (** theories that are present in the problem
     (name of theory +type args +argument) *)

val lemma : foclause t
  (** Lemma: similar to {!holds}, but explicitely used for facts
      deduced by the meta-prover. In general [lemma f => holds f]. *)

module Base : sig
  val set : Set.t
    (** The set of default plugins *)

  val signature : Signature.t
end

(** {2 Interaction with Reasoner} *)

val facts : Reasoner.t -> 'a t -> 'a Sequence.t
  (** Iterate on values that belong to the given plugin
      and are currently facts in the reasoner *)

val of_consequence : Reasoner.consequence -> 'a t -> 'a option
  (** Try to extract a value from a consequence *)

val of_consequences : Reasoner.consequence Sequence.t -> 'a t -> 'a Sequence.t
