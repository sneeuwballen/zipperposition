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
  (** {2 Basic type definitions} *)
  type t = term parametrized
    (** A pattern is a curryfied formula, along with a list of variables
        whose order matters. *)

  type atom =
    | MString of string     (** Just a string *)
    | MPattern of t         (** A pattern, ie a signature-independent formula *)
    | MTerm of term         (** A ground term (constant...) *)
    | MList of atom list    (** List of atoms *)
    (** A Datalog atom, in which we may want to fit any structure we want *)

  val eq_pattern : t -> t -> bool
  val hash_pattern : t -> int

  val eq_atom : atom -> atom -> bool
  val hash_atom : atom -> int

  (** {2 Printing/conversion to JSON} *)

  val pp_pattern : Format.formatter -> t -> unit
  val pp_atom : Format.formatter -> atom -> unit

  val to_json : t -> json
  val of_json : json -> t
  val atom_to_json : atom -> json
  val atom_of_json : json -> atom

  (** {2 Datalog atoms} *)

  module Logic : Datalog.Logic.S with type symbol = atom
    (** The Datalog prover that reasons over atoms. *)

  (** {2 Conversion pattern <-> clause, and matching *)

  val of_term : term -> t
    (** Finds which variables in the (curryfied) term are symbols
        that should be put in the parameters *)

  val abstract_clause : literal array -> t
    (** Abstracts the clause out *)

  val arity : t -> int
    (** number of arguments that have to be provided
        to instantiate the pattern *)

  val instantiate : t -> term list -> term
    (** This applies the pattern to the given arguments, beta-reduces,
        and uncurry the term back. It will fail if the result is not
        first-order. *)

  val matching : t -> literal array -> term list Sequence.t
    (** [matching p lits] attempts to match the literals against the pattern.
        It yields a list of solutions, each solution [s1,...,sn] satisfying
        [instantiate p [s1,...,sn] =_AC c] modulo associativity and commutativity
        of "or" and "=". *)

  val rename : t -> varlist -> t
    (** Rename the variables in the pattern. The provided list of variables
        must be of the same length as [arity pattern]. *)
end

(** {2 Persistent Knowledge Base} *)

module KB : sig
  (** {2 Knowledge Item} *)

  (** Assertions at the meta-level, that respectively state that
      a lemma is true, that define a theory, or that bind a ground
      convergent system to a theory *)
  type item =
  | Named of string * Pattern.t
    (** Named formula *)
  | Lemma of Pattern.t * Pattern.t list
    (** A lemma is the implication of a pattern by other patterns,
        but with some variable renamings to correlate the
        bindings of the distinct patterns. For instance,
        (F(x,y)=x, [F], [Mult]) may be implied by
        (F(y,x)=y, [F], [MyMult]) and
        (F(x,y)=G(y,x), [F,G], [Mult,MyMult]). *)
  | Theory of string parametrized
    (** A theory, like a lemma, needs to correlate the variables
        in several patterns via renaming. It states that some symbols
        are an instance of the theory *)
  | GC of gnd_convergent_spec
    (** Ground Convergent system of equations *)
  | Rule of item * item list
    (** Assertion that depends on other assertions *)
  and gnd_convergent_spec = {
    gc_vars : varlist;
    gc_ord : string;
    gc_prec : varlist;
    gc_eqns : Pattern.t list;
  } (** Abstract equations that form a ground convergent rewriting system
        when instantiated. It is parametrized by the theory it decides.
        gc_ord and gc_prec (once instantiated), give a constraint on the ordering
        that must be satisfied for the system to be a decision procedure. *)

  val rename : item -> varlist -> item
    (** Rename non-bound variables in the item *)

  (** {2 Knowledge Base} *)

  type t

  val empty : t

  val add_item : t -> item -> t

  val to_seq : t -> item Sequence.t
  val of_seq : t -> item Sequence.t -> t

  (** {2 Printing/parsing} *)

  val pp_item : Format.formatter -> item -> unit
  val pp : Format.formatter -> t -> unit

  val item_to_json : item -> json
  val item_of_json : json -> item
  val to_json : t -> json
  val of_json : t -> json -> t

  (** {2 Saving/restoring from/to disk} *)

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
