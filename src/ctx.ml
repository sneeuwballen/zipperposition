
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
    val ac : Theories.AC.t

    val total_order : Theories.TotalOrder.t

    val add_ac : ?proof:Proof.t list -> Symbol.t -> unit
    (** Symbol is AC *)

    val add_order : ?proof:Proof.t list ->
                    less:Symbol.t -> lesseq:Symbol.t -> ty:Type.t ->
                    Theories.TotalOrder.instance
    (** Pair of symbols that constitute an ordering.
        @return the corresponding instance. *)

    val add_tstp_order : unit -> Theories.TotalOrder.instance
    (** Specific version of {!add_order} for $less and $lesseq *)
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
    let ac = Theories.AC.create ()
    let total_order = Theories.TotalOrder.create ()

    let add_ac ?proof s = Theories.AC.add ~spec:ac ?proof s

    let add_order ?proof ~less ~lesseq ~ty =
      let spec = total_order in
      try
        Theories.TotalOrder.find ~spec less
      with Not_found ->
        let instance = Theories.TotalOrder.add ~spec ?proof ~less ~lesseq ~ty in
        let ty_less = Signature.find !_signature less in
        let ty_lesseq = Signature.find !_signature lesseq in
        begin match ty_less, ty_lesseq with
        | Some ty1, Some ty2 ->
          if not (Type.eq ty1 ty2)
            then raise (Type.Error (Util.sprintf
              "ordering predicates %a and %a have distinct types %a and %a"
              Symbol.pp less Symbol.pp lesseq Type.pp ty1 Type.pp ty2))
            else ()
        | None, _ ->
          failwith (Util.sprintf
            "add_order: type of symbol %a must be declared" Symbol.pp less)
        | _, None ->
          failwith (Util.sprintf
            "add_order: type of symbol %a must be declared" Symbol.pp lesseq)
        end;
        instance

    let add_tstp_order () =
      let spec = total_order in
      Theories.TotalOrder.tstp_instance ~spec
  end
end
