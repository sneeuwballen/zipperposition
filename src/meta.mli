(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Types and basic functions for the meta-prover} *)

open Types
open Symbols

(** {2 Representation of patterns: higher-order terms} *)

(** The datalog provers reasons over first-order formulas. However, to make
    those formulas signature-independent, we curryfy them and abstract their
    symbols into lambda-bound variables.

    This way, the pattern for "f(X,Y)=f(Y,X)"
    is "\F. ((= @ ((F @ x) @ y)) @ ((F @ y) @ x))" *)

module Pattern : sig
  type pattern = term parametrized
    (** A pattern is a curryfied formula, along with a list of variables
        whose order matters. *)

  val pp_pattern : Format.formatter -> pattern -> unit
  val pattern_to_json : pattern -> json
  val pattern_of_json : json -> pattern

  type atom =
    | MString of string     (** Just a string *)
    | MPattern of pattern   (** A pattern, ie a signature-independent formula *)
    | MTerm of term         (** A ground term (constant...) *)
    (** A Datalog atom, in which we may want to fit any structure we want *)

  val equal_atom : atom -> atom -> bool
  val hash_atom : atom -> int
  val pp_atom : Format.formatter -> atom -> unit

  val atom_to_json : atom -> json
  val atom_of_json : json -> atom

  module Logic : Datalog.Logic.S with type symbol = atom
    (** The Datalog prover that reasons over atoms. *)

  type lemma =
    [ `Lemma of pattern parametrized * pattern parametrized list ]
    (** A lemma is the implication of a pattern by other patterns,
        but with some variable renamings to correlate the
        bindings of the distinct patterns. For instance,
        (F(x,y)=x, [F], [Mult]) may be implied by
        (F(y,x)=y, [F], [MyMult]) and
        (F(x,y)=G(y,x), [F,G], [Mult,MyMult]). *)

  type theory =
    [ `Theory of string parametrized * pattern parametrized list ]
    (** A theory, like a lemma, needs to correlate the variables
        in several patterns via renaming. It outputs an assertion
        about the theory being present for some symbols. *)

  type gnd_convergent =
    [ `GndConvergent of gnd_convergent_spec parametrized ]
  and gnd_convergent_spec = {
    gc_ord : string;
    gc_prec : varlist;
    gc_eqns : pattern list;
  } (** Abstract equations that form a ground convergent rewriting system
        when instantiated.
        gc_ord and gc_prec, once instantiated, give a constraint on the ordering
        that must be satisfied for the system to be a decision procedure. *)

  type item = [lemma | theory | gnd_convergent]
    (** Any meta-object *)

  (** {2 Conversion pattern <-> clause, and matching *)

  val abstract_clause : literal array -> pattern
    (** Abstracts the clause out *)

  val arity : pattern -> int
    (** number of arguments that have to be provided
        to instantiate the pattern *)

  val instantiate : pattern -> term list -> term
    (** This applies the pattern to the given arguments, beta-reduces,
        and uncurry the term back. It will fail if the result is not
        first-order. *)

  val matching : pattern -> literal array -> term list Sequence.t
    (** [matching p lits] attempts to match the literals against the pattern.
        It yields a list of solutions, each solution [s1,...,sn] satisfying
        [instantiate p [s1,...,sn] =_AC c] modulo associativity and commutativity
        of "or" and "=". *)

  (** {2 Printing/parsing} *)

  val pp_item : Format.formatter -> [< item] -> unit
  val item_to_json : [< item] -> json
  val item_of_json : json -> [> item]
end

(** {2 Persistent Knowledge Base} *)

module KB : sig
  type t

  val empty : t

  val add_item : t -> Pattern.item -> t

  val to_seq : t -> Pattern.item Sequence.t
  val of_seq : t -> Pattern.item Sequence.t -> t

  val to_json : t -> json
  val of_json : t -> json -> t

  val pp : Format.formatter -> t -> unit

  (** {2 Saving/restoring KB from disk} *)

  val save : file:string -> t -> unit

  val restore : file:string -> t -> t
end

(** {2 The meta-prover itself} *)

module Prover : sig
  type t
    (** A meta-prover, reasoning at the theory/lemma level *)

  val create : ctx:context -> KB.t -> t
    (** Fresh meta-prover, using the given KB *)

  val get_kb : t -> KB.t
    (** Get the current Knowledge Base of the prover *)

  type result =
    | Deduced of literal array * hclause list
    | Theory of string * term list
    | Expert of Experts.expert
    (** Feedback from the meta-prover *)

  (* TODO: call calculus#preprocess on resulting clauses (CNF, etc.) *)

  val scan_clause : t -> literal array -> result list
    (** Match the clause against patterns known to the KB. Matches
        are added to the Datalog engine, and if some theories and lemma
        are detected they are returned *)

  val theories : t -> (string * term list) Sequence.t
    (** List of theories detected so far *)

  val experts : t -> Experts.expert list
    (** Current list of experts that can be used *)
end
