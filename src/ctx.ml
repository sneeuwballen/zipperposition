
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

(** {1 Basic context for literals, clauses...} *)

open Logtk

module T = FOTerm
module S = Substs
module TO = Theories.TotalOrder

type scope = Substs.scope

(** {2 Context for a Proof} *)
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

  val select : Literal.t array -> BV.t

  val renaming_clear : unit  -> Substs.Renaming.t
  (** Obtain the global renaming. The renaming is cleared before
      it is returned. *)

  val lost_completeness : unit -> unit
  (** To be called when completeness is not preserved *)

  val is_completeness_preserved : unit -> bool
  (** Check whether completeness was preserved so far *)

  val add_signature : Signature.t -> unit
  (** Merge  the given signature with the context's one *)

  val declare : Symbol.t -> Type.t -> unit
  (** Declare the type of a symbol (updates signature) *)

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
                Theories.TotalOrder.t
        (** Pair of symbols that constitute an ordering.
            @return the corresponding instance. *)

      val add_tstp : unit -> Theories.TotalOrder.t
        (** Specific version of {!add_order} for $less and $lesseq *)
    end
  end
end

module Make(X : sig
  val signature : Signature.t
  val ord : Ordering.t
  val select : Selection.t
end) : S = struct
  let _ord = ref X.ord
  let _select = ref X.select
  let _signature = ref X.signature
  let _complete = ref true

  let skolem = Skolem.create ~prefix:"zsk" Signature.empty
  let renaming = S.Renaming.create ()
  let ord () = !_ord
  let set_ord o = _ord := o
  let selection_fun () = !_select
  let set_selection_fun s = _select := s
  let signature () = !_signature
  let complete () = !_complete

  let compare t1 t2 = Ordering.compare !_ord t1 t2

  let select lits = !_select lits

  let lost_completeness () =
    if !_complete then Util.debug 1 "completeness is lost";
    _complete := false

  let is_completeness_preserved = complete

  let add_signature signature =
    _signature := Signature.merge !_signature signature;
    _ord := !_signature
      |> Signature.Seq.to_seq
      |> Sequence.map fst
      |> Ordering.add_seq !_ord;
    ()

  let declare symb ty =
    _signature := Signature.declare !_signature symb ty

  let renaming_clear () =
    S.Renaming.clear renaming;
    renaming

  module Theories = struct
    module STbl = Symbol.Tbl
    module PF = PFormula

    module AC = struct
      let tbl = STbl.create 3
      let proofs = STbl.create 3
      let on_add = Signal.create ()

      let axioms s =
        (* FIXME: need to recover type of [f]
        let x = T.mk_var 0 in
        let y = T.mk_var 1 in
        let z = T.mk_var 2 in
        let f x y = T.mk_node s [x; y] in
        let mk_eq x y = F.mk_eq x y in
        let mk_pform name f =
          let f = F.close_forall f in
          let name = Util.sprintf "%s_%a" name Symbol.pp s in
          let proof = Proof.mk_f_axiom f ~file:"/dev/ac" ~name in
          PF.create f proof
        in
        [ mk_pform "associative" (mk_eq (f (f x y) z) (f x (f y z)))
        ; mk_pform "commutative" (mk_eq (f x y) (f y x))
        ]
        *)
        []

      let add ?proof ~ty s =
        let proof = match proof with
        | Some p -> p
        | None -> (* new axioms *)
          List.map PF.proof (axioms s)
        in
        if not (STbl.mem tbl s)
        then begin
          let instance = Theories.AC.({ty; sym=s}) in
          STbl.replace tbl s instance;
          STbl.replace proofs s proof;
          Signal.send on_add instance
        end

      let is_ac s = STbl.mem tbl s

      let exists_ac () = STbl.length tbl > 0

      let find_proof s = STbl.find proofs s

      let symbols () =
        STbl.fold
          (fun s _ set -> Symbol.Set.add s set)
          tbl Symbol.Set.empty

      let symbols_of_terms seq =
        let module A = T.AC(struct
          let is_ac = is_ac
          let is_comm _ = false
        end) in
        A.symbols seq

      let symbols_of_forms f =
        Sequence.flatMap Formula.FO.Seq.terms f |> symbols_of_terms

      let proofs () =
        STbl.fold
          (fun _ proofs acc -> List.rev_append proofs acc)
          proofs []
    end

    module TotalOrder = struct
      module InstanceTbl = Hashtbl.Make(struct
        type t = TO.t
        let equal = TO.eq
        let hash = TO.hash
      end)

      let less_tbl = STbl.create 3
      let lesseq_tbl = STbl.create 3
      let proofs = InstanceTbl.create 3
      let on_add = Signal.create ()

      let is_less s = STbl.mem less_tbl s

      let is_lesseq s = STbl.mem lesseq_tbl s

      let find s =
        try
          STbl.find less_tbl s
        with Not_found ->
          STbl.find lesseq_tbl s

      let is_order_symbol s =
        STbl.mem less_tbl s || STbl.mem lesseq_tbl s

      let find_proof instance =
        InstanceTbl.find proofs instance

      let axioms ~less ~lesseq =
        (* FIXME: need to recover type of less's arguments
        let x = T.mk_var 0 in
        let y = T.mk_var 1 in
        let z = T.mk_var 2 in
        let mk_less x y = F.mk_atom (T.mk_node ~ty:Type.o less [x;y]) in
        let mk_lesseq x y = F.mk_atom (T.mk_node ~ty:Type.o lesseq [ x;y]) in
        let mk_eq x y = F.mk_eq x y in
        let mk_pform name f =
          let f = F.close_forall f in
          let name = Util.sprintf "%s_%a_%a" name Symbol.pp less Symbol.pp lesseq in
          let proof = Proof.mk_f_axiom f ~file:"/dev/order" ~name in
          PF.create f proof
        in
        [ mk_pform "total" (F.mk_or [mk_less x y; mk_eq x y; mk_less y x])
        ; mk_pform "irreflexive" (F.mk_not (mk_less x x))
        ; mk_pform "transitive" (F.mk_imply (F.mk_and [mk_less x y; mk_less y z]) (mk_less x z))
        ; mk_pform "nonstrict" (F.mk_equiv (mk_lesseq x y) (F.mk_or [mk_less x y; mk_eq x y]))
        ]
        *)
        []

      let add ?proof ~less ~lesseq ~ty =
        let proof = match proof with
        | Some p -> p
        | None ->
          List.map PF.proof (axioms ~less ~lesseq)
        in
        let instance =
          try Some (STbl.find lesseq_tbl lesseq)
          with Not_found ->
            if STbl.mem less_tbl less
              then raise (Invalid_argument "ordering instances overlap")
              else None
        in
        match instance with
        | None ->
          (* new instance *)
          let instance = Theories.TotalOrder.({ less; lesseq; ty; }) in
          STbl.add less_tbl less instance;
          STbl.add lesseq_tbl lesseq instance;
          InstanceTbl.add proofs instance proof;
          Signal.send on_add instance;
          instance
        | Some instance ->
          if not (Unif.Ty.are_variant ty instance.TO.ty)
          then raise (Invalid_argument "incompatible types")
          else if not (Symbol.eq less instance.TO.less)
          then raise (Invalid_argument "incompatible symbol for lesseq")
          else instance

      let add_tstp () =
        try
          find Symbol.TPTP.Arith.less
        with Not_found ->
          let less = Symbol.TPTP.Arith.less in
          let lesseq = Symbol.TPTP.Arith.lesseq in
          (* add instance *)
          add ?proof:None
            ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0])) ~less ~lesseq

      let exists_order () =
        assert (STbl.length lesseq_tbl = STbl.length less_tbl);
        STbl.length less_tbl > 0
    end
  end
end
