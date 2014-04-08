
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

module S = Substs

type scope = Substs.scope

(** {2 Context for a Proof} *)
module type S = sig
  val ord : unit -> Ordering.t
  (** current ordering on terms *)

  val select : unit -> Selection.t
  (** selection function for clauses *)

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
    val ac : Theories.AC.t

    val total_order : Theories.TotalOrder.t

    val add_ac : ?proof:Proof.t list -> Symbol.t -> unit
    (** Symbol is AC *)

    val add_order : ?proof:Proof.t list ->
                    less:Symbol.t -> lesseq:Symbol.t ->
                    Theories.TotalOrder.instance
    (** Pair of symbols that constitute an ordering.
        @return the corresponding instance. *)

    val add_tstp_order : unit -> Theories.TotalOrder.instance
    (** Specific version of {!add_order} for $less and $lesseq *)
  end
end

module Make(Dummy : sig end) : S = struct
  let _ord = ref Ordering.none
  let _select = ref Selection.no_select
  let _signature = ref Signature.empty
  let _complete = ref true

  let skolem = Skolem.create ~prefix:"zsk" Signature.empty
  let renaming = S.Renaming.create ()
  let ord () = !_ord
  let select () = !_select
  let signature () = !_signature
  let complete () = !_complete

  let compare t1 t2 = Ordering.compare !_ord t1 t2

  let select lits = !_select lits

  let lost_completeness () =
    if !_complete then Util.debug 1 "completeness is lost";
    _complete := false

  let is_completeness_preserved () = complete

  let add_signature ~ctx signature =
    _signature := Signature.merge !_signature signature;
    _ord := !_signature
      |> Signature.Seq.to_seq
      |> Sequence.map fst
      |> Ordering.add_seq !_ord;
    ()

  let declare symb ty =
    _signature := Signature.declare !_signature symb ty

  let renaming_clear () = S.Renaming.clear renaming

  module Theories = struct
    let ac = Theories.AC.create ()
    let total_order = Theories.TotalOrder.create ()

    let add_ac ?proof s = Theories.AC.add ~spec:ac ?proof s

    let add_order ?proof ~less ~lesseq =
      let spec = total_order in
      try
        Theories.TotalOrder.find ~spec less
      with Not_found ->
        let instance = Theories.TotalOrder.add ~spec ?proof ~less ~lesseq in
        (* declare missing symbols, if any; also take care that
          less and lesseq have the same type, which is, a binary predicate 
          FIXME check types?
        if not (Signature.mem ctx.signature less) || not (Signature.mem ctx.signature lesseq)
          then failwith "Ctx.add_order: order symbols must be declared";
        let ty_less = Signature.find ctx.signature less in
        let ty_lesseq = Signature.find ctx.signature lesseq in
        if not (Type.eq ty_less ty_lesseq)
          then TypeUnif.fail Substs.Ty.empty ty_less 0 ty_lesseq 0;
        *)
        instance

    let add_tstp_order () =
      let less = Symbol.TPTP.Arith.less in
      let lesseq = Symbol.TPTP.Arith.lesseq in
      let spec = total_order in
      try
        Theories.TotalOrder.find ~spec less
      with Not_found ->
        (* declare types of $less and $lesseq *)
        let mysig = Signature.filter
          Signature.TPTP.Arith.base
          (fun s _ty -> Symbol.eq s less || Symbol.eq s lesseq)
        in
        _signature := Signature.merge !_signature mysig;
        let instance = Theories.TotalOrder.add ~spec ?proof:None ~less ~lesseq in
        instance
  end
