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

module STbl = Symbol.SHashtbl
module T = Term
module F = Formula
module PF = PFormula

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = Proof.t list STbl.t

  let create () = STbl.create 7

  let is_ac ~spec s = STbl.mem spec s

  let axioms s =
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

  let add ~spec ?proof s =
    let proof = match proof with
    | Some p -> p
    | None -> (* new axioms *)
      List.map PF.get_proof (axioms s)
    in
    STbl.replace spec s proof

  let is_ac ~spec s = STbl.mem spec s

  let exists_ac ~spec = STbl.length spec > 0

  let find_proof ~spec s = STbl.find spec s

  let symbols ~spec =
    STbl.fold
      (fun s _ set -> Symbol.SSet.add s set)
      spec Symbol.SSet.empty

  let symbols_of_terms ~spec seq =
    T.ac_symbols ~is_ac:(is_ac ~spec) seq

  let symbols_of_forms ~spec f =
    T.ac_symbols ~is_ac:(is_ac ~spec) (Sequence.flatMap F.terms_seq f)

  let proofs ~spec =
    STbl.fold
      (fun _ proofs acc -> List.rev_append proofs acc)
      spec []
end

(** {2 Total Ordering} *)

module TotalOrder = struct
  type instance = {
    less : Symbol.t;
    lesseq : Symbol.t;
    proof : Proof.t list;
  } (** A single instance of total ordering *)

  type t = {
    less_tbl : instance STbl.t;
    lesseq_tbl : instance STbl.t;
  } (** Specification of which symbols implement total orderings *)

  type lit = {
    left : Term.t;
    right : Term.t;
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

  let orders ~spec =
    STbl.fold
      (fun _ instance acc -> instance :: acc)
      spec.less_tbl []

  let is_order_symbol ~spec s =
    STbl.mem spec.less_tbl s || STbl.mem spec.lesseq_tbl s

  let axioms ~less ~lesseq =
    let x = T.mk_var 0 in
    let y = T.mk_var 1 in
    let z = T.mk_var 2 in
    let mk_less x y = F.mk_atom (T.mk_node less [x;y]) in
    let mk_lesseq x y = F.mk_atom (T.mk_node lesseq [x;y]) in
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

  let add ~spec ?proof ~less ~lesseq =
    let proof = match proof with
    | Some p -> p
    | None ->
      List.map PF.get_proof (axioms ~less ~lesseq)
    in
    if STbl.mem spec.less_tbl less || STbl.mem spec.lesseq_tbl lesseq
      then raise (Invalid_argument "ordering instances overlap");
    let instance = { less; lesseq; proof; } in
    STbl.add spec.less_tbl less instance;
    STbl.add spec.lesseq_tbl lesseq instance;
    instance

  let eq i1 i2 =
    Symbol.eq i1.less i2.less && Symbol.eq i1.lesseq i2.lesseq

  let tstp_instance ~spec =
    try
      find ~spec Symbol.Arith.less
    with Not_found ->
      let less = Symbol.Arith.less in
      let lesseq = Symbol.Arith.lesseq in
      (* add instance *)
      add ~spec ?proof:None ~less ~lesseq

  let create ?(base=true) () =
    let spec = {
      less_tbl = STbl.create 13;
      lesseq_tbl = STbl.create 13;
    } in
    if base then
      ignore (tstp_instance ~spec);
    spec

  let exists_order ~spec =
    assert (STbl.length spec.lesseq_tbl = STbl.length spec.less_tbl);
    STbl.length spec.less_tbl > 0

  let pp_instance buf i =
    Printf.bprintf buf "total_order{%a, %a}" Symbol.pp i.less Symbol.pp i.lesseq

  let to_string_instance i = Util.on_buffer pp_instance i

  let fmt_instance fmt i = Format.pp_print_string fmt (to_string_instance i)
end
