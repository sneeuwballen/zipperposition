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

  type t = term * sort list
    (** A pattern is a curryfied formula, whose symbols are abstracted into
        lambda-bound variables. The list is the list of the sorts of
        the bound variables, such that if [t1,...,tn] are terms whose sorts
        pairwise match the list of sort, then [instantiate p t1...tn] will
        be well-typed. *)

  val eq_pattern : t -> t -> bool
  val hash_pattern : t -> int

  (** {2 Printing/conversion to JSON} *)

  val pp_pattern : Format.formatter -> t -> unit
  val pp_pattern_p : Format.formatter -> t parametrized -> unit
  val to_json : t -> json
  val of_json : json -> t

  (** {2 Conversion pattern <-> clause, and matching *)

  val find_symbols : ?symbols:SSet.t -> term Sequence.t -> SSet.t
    (** Given a curryfied term, find the symbols that occur as head constants
        (ie "f" in "f @ _" where f is not a "_@_") *)

  val find_functions : term Sequence.t -> symbol list -> term list
    (** [find_functions t (s1,...,sn)] where t is currified
        maps s1,...,sn to constants that have the correct sort *)

  val of_term_with : term -> symbol list -> t * term list
    (** Abstracts the given constants out, in the given order. The
        term must be curryfied. *)

  val of_term : term -> t * term list
    (** Abstract over constants in the (curryfied) term. *)

  val abstract_clause : literal array -> t * term list
    (** Abstracts the clause out *)

  val sorts : t -> sort list
    (** Sorts of arguments that are accepted by the pattern *)

  val arity : t -> int
    (** number of arguments that have to be provided
        to instantiate the pattern *)

  val instantiate : ?uncurry:bool -> t -> term list -> term
    (** This applies the pattern to the given arguments, beta-reduces,
        and uncurry the term back (by default). It will fail if the result is not
        first-order. *)

  val apply_subst : ?uncurry:bool -> t parametrized bind -> substitution -> term
    (** Apply the substitution to variables that parametrize the pattern,
        then [instantiate] the pattern (beta-reduced and uncurryfied).
        [apply_subst (p,vars) subst] is equivalent to
        [instantiate p (List.map (S.apply_subst subst) vars)]. *)

  val matching : t -> literal array -> term list Sequence.t
    (** [matching p lits] attempts to match the literals against the pattern.
        It yields a list of solutions, each solution [s1,...,sn] satisfying
        [instantiate p [s1,...,sn] =_AC c] modulo associativity and commutativity
        of "or" and "=". *)
end

(** {2 Persistent Knowledge Base} *)

module KB : sig
  (** {2 Knowledge Item} *)

  (** Definitions at the meta-level, that respectively state that
      a lemma is true, that define a theory, or that bind a ground
      convergent system to a theory *)
  type definition =
  | Named of string * Pattern.t
  | Theory of string parametrized * premise list
  | Lemma of Pattern.t parametrized * premise list
  | GC of gnd_convergent_spec * premise list
  (** Condition for some meta-level assertion *)
  and premise =
  | IfNamed of string parametrized
  | IfTheory of string parametrized
  | IfPattern of Pattern.t parametrized
  (** Assertions about the problem *)
  and fact =
  | ThenPattern of Pattern.t parametrized
  | ThenTheory of string parametrized
  | ThenNamed of string parametrized
  | ThenGC of gnd_convergent_spec
  and gnd_convergent_spec = {
    gc_vars : varlist;
    gc_ord : string;
    gc_prec : varlist;
    gc_eqns : Pattern.t parametrized list;
  } (** Abstract equations that form a ground convergent rewriting system
        when instantiated. It is parametrized by the theory it decides.
        gc_ord and gc_prec (once instantiated), give a constraint on the ordering
        that must be satisfied for the system to be a decision procedure. *)

  (** {2 Printing/parsing} *)

  val pp_definition : Format.formatter -> definition -> unit
  val pp_premise : Format.formatter -> premise -> unit
  val pp_fact : Format.formatter -> fact -> unit

  val definition_to_json : definition -> json
  val definition_of_json : json -> definition

  (** {2 Datalog atoms} *)

  type atom =
  | MString of string
  | MPattern of Pattern.t
  | MPatternVars of Pattern.t parametrized
  | MTerm of term

  val eq_atom : atom -> atom -> bool
  val hash_atom : atom -> int

  val pp_atom : Format.formatter -> atom -> unit
  val atom_to_json : atom -> json
  val atom_of_json : json -> atom

  module Logic : Datalog.Logic.S with type symbol = atom
    (** The Datalog prover that reasons over atoms. *)
  
  (** {2 Conversion to Datalog} *)

  val definition_to_datalog : definition -> Logic.clause
    (** Translate a definition into a Datalog clause *)

  val definition_to_goals : definition -> Logic.literal list
    (** Find the most general goal that activates this definition *)

  val fact_to_datalog : fact -> Logic.literal
    (** Convert a meta-fact to a Datalog fact *)

  val of_datalog : Logic.literal -> fact option
    (** Try to convert back a Datalog fact into a meta-fact *)

  (** {2 Knowledge Base} *)

  type t

  val empty : t

  val add_definition : t -> definition -> t
  val add_definitions : t -> definition Sequence.t -> t

  val to_seq : t -> definition Sequence.t
  val of_seq : t -> definition Sequence.t -> t

  val pp : Format.formatter -> t -> unit
  val to_json : t -> json Stream.t
  val of_json : t -> json Stream.t -> t

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

  val update_ctx : ctx:context -> t -> unit
    (** Change the underlying context of the prover *)

  val get_kb : t -> KB.t
    (** Get the current Knowledge Base of the prover *)

  type result =
    | Deduced of literal array * hclause list
    | Theory of string * term list
    | Expert of Experts.t
    (** Feedback from the meta-prover *)

  val scan_clause : t -> hclause -> result list
    (** Match the clause against patterns known to the KB. Matches
        are added to the Datalog engine, and if some theories and lemma
        are detected they are returned *)

  val has_new_patterns : t -> bool
    (** Are there some new patterns? *)

  val scan_set : t -> Clauses.CSet.t -> result list
    (** Scan the set of clauses for patterns that are new. This should
        be called on the active set every time [has_new_patterns prover]
        returns true. After this, [has_new_patterns prover] returns false
        at least until the next call to [scan_clause]. *)

  val theories : t -> (string * term list) Sequence.t
    (** List of theories detected so far *)

  val experts : t -> Experts.t Sequence.t
    (** Current list of experts that can be used *)

  val results : t -> result Sequence.t
    (** All results *)

  val db : t -> KB.Logic.db
    (** Underlying Datalog base *)

  val pp_result : Format.formatter -> result -> unit
  val pp_results : Format.formatter -> result Sequence.t -> unit
end

(** {2 Parsing utils} *)
module ParseUtils : sig
  (** {2 Table of definitions} *)

  type table
  and table_val =
    | TableNamed of Pattern.t
    | TableTheory of sort list

  val create : unit -> table
  val clear : table -> unit

  val lookup_th : table:table -> string -> sort list
  val lookup_named : table:table -> string -> Pattern.t
  val lookup : table:table -> string -> table_val

  val define_th : table:table -> string -> sort list -> unit
  val define_named : table:table -> string -> Pattern.t -> unit

  (** {2 Premise for a definition} *)

  type premise =
    [ `Theory of string * symbol list
    | `Named of string * symbol list
    | `Term of term
    ]

  (** Lookup (symbol,sort) for the given premise(s) *)

  val lookup_premise : table:table -> premise -> (symbol * sort) Sequence.t
  val lookup_premises : table:table -> premise list -> (symbol * sort) Sequence.t

  (** {2 Build definitions from raw parsing data} *)

  val mk_lemma_term : table:table -> term -> premise list -> KB.definition
  val mk_lemma_named : table:table -> string * symbol list -> premise list -> KB.definition
  val mk_named : table:table -> string* symbol list -> term -> KB.definition
  val mk_theory : table:table -> string * symbol list -> premise list -> KB.definition
  val mk_gc : table:table -> term list -> string * symbol list -> premise list -> KB.definition
end
