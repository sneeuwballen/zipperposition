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

open Logtk

module type S = sig
  val ord : unit -> Ordering.t
  (** current ordering on terms *)

  val selection_fun : unit -> Selection.t
  (** selection function for clauses *)

  val set_selection_fun : Selection.t -> unit

  val set_ord : Ordering.t -> unit

  val skolem : Skolem.ctx

  val signature : unit -> Signature.t
  (** Current signature *)

  val complete : unit -> bool
  (** Is completeness preserved? *)

  val renaming : Substs.Renaming.t

  (** {2 Utils} *)

  val compare : FOTerm.t -> FOTerm.t -> Comparison.t
  (** Compare two terms *)

  val select : Selection.t

  val renaming_clear : unit  -> Substs.Renaming.t
  (** Obtain the global renaming. The renaming is cleared before
      it is returned. *)

  val lost_completeness : unit -> unit
  (** To be called when completeness is not preserved *)

  val is_completeness_preserved : unit -> bool
  (** Check whether completeness was preserved so far *)

  val add_signature : Signature.t -> unit
  (** Merge  the given signature with the context's one *)

  val find_signature : Symbol.t -> Type.t option
  (** Find the type of the given symbol *)

  val find_signature_exn : Symbol.t -> Type.t
  (** Unsafe version of {!find_signature}.
      @raise Not_found for unknown symbols *)

  val declare : Symbol.t -> Type.t -> unit
  (** Declare the type of a symbol (updates signature) *)

  val on_new_symbol : (Symbol.t * Type.t) Signal.t
  val on_signature_update : Signature.t Signal.t

  val ad_hoc_symbols : unit -> Symbol.Set.t
  (** Current set of ad-hoc symbols *)

  val add_ad_hoc_symbols : Symbol.t Sequence.t -> unit
  (** Declare that some symbols are "ad hoc", ie they are not really
      polymorphic and should not be considered as such *)

  (** {2 Literals} *)

  module Lit : sig
    val from_hooks : unit -> Literal.Conv.hook_from list
    val add_from_hook : Literal.Conv.hook_from -> unit

    val to_hooks : unit -> Literal.Conv.hook_to list
    val add_to_hook : Literal.Conv.hook_to -> unit

    val of_form : Formula.FO.t -> Literal.t
      (** @raise Invalid_argument if the formula is not atomic *)

    val to_form : Literal.t -> Formula.FO.t
  end

  (** {2 Theories} *)

  module Theories : sig
    module AC : sig
      val on_add : Theories.AC.t Signal.t

      val add : ?proof:Proof.t list -> ty:Type.t -> Symbol.t -> unit

      val is_ac : Symbol.t -> bool

      val find_proof : Symbol.t -> Proof.t list
        (** Recover the proof for the AC-property of this symbol.
            @raise Not_found if the symbol is not AC *)

      val symbols : unit -> Symbol.Set.t
        (** set of AC symbols *)

      val symbols_of_terms : FOTerm.t Sequence.t -> Symbol.Set.t
        (** set of AC symbols occurring in the given term *)

      val symbols_of_forms : Formula.FO.t Sequence.t -> Symbol.Set.t
        (** Set of AC symbols occurring in the given formula *)

      val proofs : unit -> Proof.t list
        (** All proofs for all AC axioms *)

      val exists_ac : unit -> bool
        (** Is there any AC symbol? *)
    end

    module TotalOrder : sig
      val on_add : Theories.TotalOrder.t Signal.t

      val is_less : Symbol.t -> bool

      val is_lesseq : Symbol.t -> bool

      val find : Symbol.t -> Theories.TotalOrder.t
        (** Find the instance that corresponds to this symbol.
            @raise Not_found if the symbol is not part of any instance. *)

      val find_proof : Theories.TotalOrder.t -> Proof.t list
        (** Recover the proof for the given total ordering
            @raise Not_found if the instance cannot be found*)

      val is_order_symbol : Symbol.t -> bool
        (** Is less or lesseq of some instance? *)

      val axioms : less:Symbol.t -> lesseq:Symbol.t -> PFormula.t list
        (** Axioms that correspond to the given symbols being a total ordering.
            The proof of the axioms will be "axiom" *)

      val exists_order : unit -> bool
        (** Are there some known ordering instances? *)

      val add : ?proof:Proof.t list ->
                less:Symbol.t -> lesseq:Symbol.t -> ty:Type.t ->
                Theories.TotalOrder.t * [`New | `Old]
        (** Pair of symbols that constitute an ordering.
            @return the corresponding instance and a flag to indicate
              whether the instance was already present. *)

      val add_tstp : unit -> Theories.TotalOrder.t * [`New | `Old]
        (** Specific version of {!add_order} for $less and $lesseq *)
    end
  end

  (** {2 Booleans Literals} *)

  module BoolLit : BBox.S

  (** {2 Induction} *)

  module Induction : sig
    (** {6 Inductive Types} *)

    type constructor = Symbol.t * Type.t
    (** Constructor for an inductive type *)

    type inductive_type = private {
      pattern : Type.t;  (* type, possibly with free variables *)
      constructors : constructor list;
    }
    (** An inductive type, along with its covering,disjoint constructors *)

    val declare_ty : Type.t -> constructor list -> inductive_type
    (** Declare the given inductive type.
        @raise Failure if the type is already declared
        @raise Invalid_argument if the list of constructors is empty. *)

    val inductive_types : inductive_type Sequence.t
    (** Set of inductive types known so far. *)

    (** {6 Inductive Constants} *)

    type cst = private FOTerm.t
    (** A ground term of an inductive type. It must correspond to a
        term built with the corresponding {!inductive_type} only.
        For instance, a constant of type [nat] should be equal to
        [s^n(0)] in any model. *)

    val cst_of_term : ?parent:cst -> FOTerm.t -> cst option
    (** Check whether the  given term can be an inductive constant,
        and if possible, adds it to the set of inductive constants.

        Requirements: it must be ground, and its type must be a
        known {!inductive type}.

        @param parent if present, makes [c] depend on the given constant
          that must be inductive already.
        @raise Invalid_argument if the parent isn't inductive or the
          term is non-ground *)

    val is_inductive : FOTerm.t -> bool
    (** Check whether the given constant is ready for induction *)

    val inductive_cst : cst Sequence.t
    (** set of all inductive constants *)

    module Set : Set.S with type elt = cst
    (** Set of constants *)

    val depends_on : cst -> cst -> bool
    (** [depends_on a b] is [true] iff [a] depends on [b]. This forms a
        partial strict order. *)

    val parent : cst -> cst option
    (** [parent c] returns [Some d] iff [c] depends directly on [d]. *)

    val parent_exn : cst -> cst
    (** Unsafe version of {!parent}.
        @raise Failure if the constant has no parent *)

    val level : cst -> int
    (** Level of dependency (starts at 1 for constants that don't depend
        on any other constant, otherwise [1+level (parent c)]) *)

    val is_max_among : cst -> Set.t -> bool
    (** Checks whether the constant is maximal among the given ones, that is,
        if no constant in the set depends on it *)
  end
end

