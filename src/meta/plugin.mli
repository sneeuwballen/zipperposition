
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

Plugins can be extended to handle some inner type that can be encoded/decoded
to meta-prover-compatible types. *)

open Logtk

type term = Reasoner.term

class type t = object
  method symbol : Symbol.t
    (** Symbol used to represent properties of this plugin *)

  method ty : Type.t
    (** Type of the symbol *)

  method owns : term -> bool
    (** Does this term belong to the plugin? *)

  method clauses : Reasoner.clause list
    (** Initial clauses to add *)
end

class type ['inner] extended = object
  inherit t

  method to_fact : 'inner -> term
    (** Encode an 'inner value to a fact *)

  method of_fact : term -> 'inner option
    (** Decode a fact into an 'inner value, if it actually belongs
        to the plugin *)
end

type foclause = Encoding.foclause

type set = t Symbol.Map.t
  (** Set of plugins, by their symbols *)

val signature_of_set : set -> Signature.t
  (** Signature of all plugins *)

(** {2 Builtin plugins} *)

module Base : sig
  val holds : foclause extended
    (** holds: statement about which First-Order clause is true in the
     * current problem *)

  val axiom : (Symbol.t * term) extended
    (** axioms present in the problem (name of axiom+argument) *)

  val theory : (Symbol.t * term) extended
    (** theories that are present in the problem (name of theory +argument) *)

  val lemma : foclause extended
    (** Lemma: similar to {!holds}, but explicitely used for facts
     * deduced by the meta-prover. In general [lemma f => holds f]. *)

  val set : set
    (** The set of default plugins *)

  val signature : Signature.t
end

(** {2 Interaction with Reasoner} *)

val facts : Reasoner.t -> 'a extended -> 'a Sequence.t
  (** Iterate on values that belong to the given extended plugin
   * and are currently facts in the reasoner *)

val of_consequence : Reasoner.consequence -> 'a extended -> 'a option
  (** Try to extract a value from a consequence *)

val of_consequences : Reasoner.consequence Sequence.t -> 'a extended -> 'a Sequence.t
