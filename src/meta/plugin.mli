
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

class type t = object
  method symbol : Symbol.t
    (** Symbol used to represent properties of this plugin *)

  method ty : Type.t
    (** Type of the symbol *)

  method list_ : Reasoner.t -> Reasoner.term Sequence.t
    (** List of meta statements that belong to this plugin *)
end

class type ['inner] extended = object
  inherit t
  method encoding : ('inner, Reasoner.term) Encoding.t
    (** Encoding used to convert 'inner to a term *)

  method filter : 'a. ('a -> 'inner -> 'a) ->
                  Reasoner.t -> 'a -> Reasoner.consequence Sequence.t -> 'a
    (** React to some consequences by filtering over their decoded
     * version *)

  method add_fact : Reasoner.t -> 'inner -> Reasoner.t
    (** Add a fact to the reasoner *)
end

type foclause = Encoding.foclause

type set = t Symbol.Map.t
  (** Set of plugins, by their symbols *)

val signature_of_set : set -> Signature.t
  (** Signature of all plugins *)

val holds : foclause extended
  (** holds: statement about which First-Order clause is true in the
   * current problem *)

val axiom : t
  (** axioms present in the problem *)

val theory : t
  (** theories that is present in the problem *)

val lemma : Encoding.EncodedClause.t extended
  (** Lemma: similar to {!holds}, but explicitely used for facts
   * deduced by the meta-prover. In general [lemma f => holds f]. *)

val default_plugins : set
  (** Default set of plugins *)

val default_signature : Signature.t
  (** Signature of the set of default plugins *)
