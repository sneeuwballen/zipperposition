
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

module type S = BBox_intf.S

module Make(Any : sig end) = struct

  type bool_lit = int

  let neg i = -i

  type injected =
    | Clause_component of Literals.t

  module FV = FeatureVector.Make(struct
    type t = Lits.t * bool_lit
    let cmp (l1,_)(l2,_) = Lits.compare l1 l2
    let to_lits (l,_) = Lits.Seq.abstract l
  end)
  module ITbl = Hashtbl.Make(CCInt)

  let _next = ref 1 (* next lit *)
  let _clause_set = ref (FV.empty())
  let _lit2inj = ITbl.create 56

  let _apply_sign sign b =
    if sign then b else neg b

  (* clause -> boolean lit *)
  let inject_lits lits  =
    (* special case: one negative literal. *)
    let lits, sign =
      if Array.length lits = 1 && Literal.is_neq lits.(0)
        then [| Literal.negate lits.(0) |], false
        else lits, true
    in
    (* retrieve clause. the index doesn't matter for retrieval *)
    let other = FV.retrieve_alpha_equiv_c !_clause_set (lits,1) ()
      |> Sequence.map2 (fun _ l -> l)
      |> Sequence.filter (fun (lits',_) -> Lits.are_variant lits lits')
      |> Sequence.head
    in
    match other with
    | Some (_, bool_lit) -> _apply_sign sign bool_lit
    | None ->
        let i = !_next in
        incr _next;
        (* maintain mapping *)
        let lits_copy = Array.copy lits in
        _clause_set := FV.add !_clause_set (lits_copy, i);
        ITbl.add _lit2inj i (Clause_component lits_copy);
        _apply_sign sign i

  (* boolean lit -> injected *)
  let extract i =
    if i<=0 then failwith "BBox.extract: require >0 integer";
    try Some (ITbl.find _lit2inj i)
    with Not_found -> None

  let pp buf i =
    if i<0 then Buffer.add_string buf "¬";
    let i = abs i in
    match extract i with
    | None -> Printf.bprintf buf "L%d" i
    | Some (Clause_component lits) ->
        Printf.bprintf buf "⟦%a⟧" (Util.pp_array ~sep:"∨" Literal.pp) lits

  let print fmt i =
    if i<0 then Format.pp_print_string fmt "¬";
    let i = abs i in
    match extract i with
    | None -> Format.fprintf fmt "L%d" i
    | Some (Clause_component lits) ->
        Format.fprintf fmt "@[⟦%a⟧@]"
          (CCArray.print ~sep:"∨" Literal.fmt) lits
end
