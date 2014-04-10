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

(** {6 Specifications of Built-in Theories} *)

open Logtk

module STbl = Symbol.Tbl
module T = FOTerm
module F = Formula.FO
module PF = PFormula

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = {
    tbl : Proof.t list STbl.t;
    signal : Symbol.t Signal.t;
  }

  let create () = {
    tbl = STbl.create 7;
    signal = Signal.create ();
  }

  let on_add_instance t = t.signal

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

  let add ~spec ?proof s =
    let proof = match proof with
    | Some p -> p
    | None -> (* new axioms *)
      List.map PF.proof (axioms s)
    in
    if not (STbl.mem spec.tbl s)
    then begin
      STbl.replace spec.tbl s proof;
      Signal.send spec.signal s
    end

  let is_ac ~spec s = STbl.mem spec.tbl s

  let exists_ac ~spec = STbl.length spec.tbl > 0

  let find_proof ~spec s = STbl.find spec.tbl s

  let symbols ~spec =
    STbl.fold
      (fun s _ set -> Symbol.Set.add s set)
      spec.tbl Symbol.Set.empty

  let symbols_of_terms ~spec seq =
    let module A = T.AC(struct
      let is_ac = is_ac ~spec
      let is_comm _ = false
    end) in
    A.symbols seq

  let symbols_of_forms ~spec f =
    let module A = T.AC(struct
      let is_ac = is_ac ~spec
      let is_comm _ = false
    end) in
    Sequence.flatMap F.Seq.terms f |> A.symbols

  let proofs ~spec =
    STbl.fold
      (fun _ proofs acc -> List.rev_append proofs acc)
      spec.tbl []
end

(** {2 Total Ordering} *)

module TotalOrder = struct
  type instance = {
    less : Symbol.t;
    lesseq : Symbol.t;
    ty : Type.t;
    proof : Proof.t list;
  } (** A single instance of total ordering *)

  type t = {
    less_tbl : instance STbl.t;
    lesseq_tbl : instance STbl.t;
    signal : instance Signal.t;
  } (** Specification of which symbols implement total orderings *)

  let on_add_instance t = t.signal

  type lit = {
    left : FOTerm.t;
    right : FOTerm.t;
    strict : bool;
    instance : instance;
  } (** A literal is an atomic inequality. [strict] is [true] iff the
      literal is a strict inequality, and the ordering itself
      is also provided. *)

  let is_less ~spec s = STbl.mem spec.less_tbl s

  let is_lesseq ~spec s = STbl.mem spec.lesseq_tbl s

  let find ~spec s =
    try
      STbl.find spec.less_tbl s
    with Not_found ->
      STbl.find spec.lesseq_tbl s

  let is_order_symbol ~spec s =
    STbl.mem spec.less_tbl s || STbl.mem spec.lesseq_tbl s

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

  let add ~spec ?proof ~less ~lesseq ~ty =
    let proof = match proof with
    | Some p -> p
    | None ->
      List.map PF.proof (axioms ~less ~lesseq)
    in
    let instance =
      try Some (STbl.find spec.lesseq_tbl lesseq)
      with Not_found ->
        if STbl.mem spec.less_tbl less
          then raise (Invalid_argument "ordering instances overlap")
          else None
    in
    match instance with
    | None ->
      (* new instance *)
      let instance = { less; lesseq; ty; proof; } in
      STbl.add spec.less_tbl less instance;
      STbl.add spec.lesseq_tbl lesseq instance;
      Signal.send spec.signal instance;
      instance
    | Some instance ->
      if not (Unif.Ty.are_variant ty instance.ty)
      then raise (Invalid_argument "incompatible types")
      else if not (Symbol.eq less instance.less)
      then raise (Invalid_argument "incompatible symbol for lesseq")
      else instance

  let eq i1 i2 =
    Symbol.eq i1.less i2.less && Symbol.eq i1.lesseq i2.lesseq

  let tstp_instance ~spec =
    try
      find ~spec Symbol.TPTP.Arith.less
    with Not_found ->
      let less = Symbol.TPTP.Arith.less in
      let lesseq = Symbol.TPTP.Arith.lesseq in
      (* add instance *)
      add ~spec ?proof:None
        ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0])) ~less ~lesseq

  let create () =
    let spec = {
      less_tbl = STbl.create 13;
      lesseq_tbl = STbl.create 13;
      signal = Signal.create ();
    } in
    spec

  let exists_order ~spec =
    assert (STbl.length spec.lesseq_tbl = STbl.length spec.less_tbl);
    STbl.length spec.less_tbl > 0

  let less_const ~instance = T.const ~ty:instance.ty instance.less
  let lesseq_const ~instance = T.const ~ty:instance.ty instance.lesseq

  let pp_instance buf i =
    Printf.bprintf buf "total_order{%a, %a}" Symbol.pp i.less Symbol.pp i.lesseq

  let to_string_instance i = Util.on_buffer pp_instance i

  let fmt_instance fmt i = Format.pp_print_string fmt (to_string_instance i)
end
