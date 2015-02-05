
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

val create : ?ty_prop:LogtkType.t -> ?prefix:string -> ?prop_prefix:string ->
             LogtkSignature.t ->  ctx
  (** New skolem contex. A prefix can be provided, which will be
      added to all newly created skolem symbols.
      @param ty_prop the type of atomic propositions (default {!LogtkType.TPTP.o}).
      @param prefix used to name skolem functions/constants
      @param prop_prefix used to name sub-formulas during CNF
      @param signature initial signature the context holds. *)

val to_signature : ctx -> LogtkSignature.t
  (** Signature of all new skolem symbols that were created using this
      context. *)

val fresh_sym : ctx:ctx -> ty:LogtkType.t -> LogtkSymbol.t
  (** Just obtain a fresh skolem symbol. It is also declared
      in the inner signature. *)

val fresh_sym_with : ctx:ctx -> ty:LogtkType.t -> string -> LogtkSymbol.t
  (** Fresh symbol with a different name *)

val fresh_ty_const : ?prefix:string -> ctx:ctx -> unit -> LogtkSymbol.t
  (** New symbol to be used as a type constant (no need to declare it) *)

val instantiate_ty : LogtkFormula.FO.t -> LogtkType.t -> LogtkFormula.FO.t
  (** Instantiate first open (type) variable with the given type *)

val instantiate : LogtkFormula.FO.t -> LogtkFOTerm.t -> LogtkFormula.FO.t
  (** Instantiate first open variable with the given term*)

(** {2 Skolemization} *)

val clear_var : ctx:ctx -> unit
  (** reset the variable counter (once a formula has been processed) *)

val fresh_var : ctx:ctx -> int
  (** Unique index for universal variables *)

val update_var : ctx:ctx -> LogtkFOTerm.t -> unit
  (** Avoid collisions with variables of this term in calls to {!fresh_var}. *)

val skolem_form : ctx:ctx -> ty:LogtkType.t -> LogtkFormula.FO.t -> LogtkFormula.FO.t
  (** Skolemize the given formula at root (assumes it occurs just under an
      existential quantifier, whose De Bruijn variable 0 is replaced
      by a fresh symbol applied to free variables). This also caches symbols,
      so that the same formula (modulo alpha-renaming) is always skolemized the
      same way.

      For instance, [skolem_form ~ctx p(a, b, db0, X)] will yield
      something like [p(a, b, sk42(X), X)].

      @param ty the type of the De Bruijn variable to replace *)

(** {2 Definitions of Formulas} *)

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

type definition = {
  form : LogtkFormula.FO.t;
  proxy : LogtkFormula.FO.t;
  polarity : polarity ref;
}

val has_definition : ctx:ctx -> LogtkFormula.FO.t -> bool
  (** Does this formula already have a definition (in which case it's
      very cheap to reduce it to CNF) *)

val get_definition : ctx:ctx ->
                      polarity:polarity ->
                      LogtkFormula.FO.t -> LogtkFormula.FO.t
  (** [rename_form ~ctx f] returns a (possibly new) predicate for [f],
      with the free variables of [f] as arguments. If some other formula
      that is alpha-equivalent to [f] was defined, then the same name is
      used. This modifies the context to remember that [f] has a definition,
      and which polarity it is used with.

      {b NOTE}: we assume no free variable occurs in [f]. If any such variable
      occurs, alpha-equivalent but distinct formulas will have different
      names.

      @return the atomic formula that stands for [f]. *)

val all_definitions : ctx:ctx -> definition Sequence.t
  (** Definitions that were introduced so far. *)

val remove_def : ctx:ctx -> definition -> unit
  (** remove the definition of [f], so that we're sure it will
      never be used again *)

val pop_new_definitions : ctx:ctx -> definition list
  (** List of new definitions, that were introduced since the last
     call to {!new_definitions}. The list can be obtained only once,
     after which those definitions are not "new" anymore.

     Will call {!remove_def} so there is no risk of re-using a definition
     with a new polarity. *)

val has_new_definitions : ctx:ctx -> bool
  (** @return true if some new definitions were introduced. *)

val clear_skolem_cache : ctx:ctx -> unit
  (** Forget already skolemized formulas, so that new formulas use different
      Skolem symbols *)

val skolem_ho : ctx:ctx -> ty:LogtkType.t -> LogtkHOTerm.t -> LogtkHOTerm.t
  (** Skolemize a higher order term. Quite the same as {!skolem_form}.
      {b Not implemented} *)

