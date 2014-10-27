
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

module Lits = Literals

module type BLit = sig
  type t

  val of_int : int -> t
  (** Build a literal from a > 0 integer *)

  val neg : t -> t
  (** Boolean negation *)

  val to_int : t -> int
  (** Obtain the unique ID behind this literal (must be > 0) *)
end

module type S = BBox_intf.S

module Make(C : Clause.S)(BLit : BLit) = struct
  module C = C
  module Ctx = C.Ctx

  type bool_lit = BLit.t

  type injected =
    | C of C.t

  module FV = FeatureVector.Make(struct
    type t = C.t
    let cmp = C.compare
    let to_lits = C.Seq.abstract
  end)
  module ITbl = Hashtbl.Make(CCInt)

  let _next = ref 1 (* next lit *)
  let _clause_set = ref (FV.empty())
  let _lit2inj = ITbl.create 56

  (* clause -> boolean lit *)
  let inject_clause c =
    let c' = FV.retrieve_alpha_equiv_c !_clause_set c ()
      |> Sequence.map2 (fun _ c -> c)
      |> Sequence.filter (fun c' -> Lits.are_variant (C.lits c) (C.lits c'))
      |> Sequence.head
    in
    match c' with
    | Some c' -> BLit.of_int (C.as_bool_exn c')
    | None ->
        let i = !_next in
        incr _next;
        (* maintain mapping *)
        C.set_bool_name c i;
        _clause_set := FV.add !_clause_set c;
        ITbl.add _lit2inj i (C c);
        BLit.of_int i

  (* boolean lit -> injected *)
  let extract i =
    try Some (ITbl.find _lit2inj (BLit.to_int i))
    with Not_found -> None
end
