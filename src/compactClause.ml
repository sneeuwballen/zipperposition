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

(** {1 Compact clause representation} *)

open Logtk

module F = Formula.FO
module Lit = Literal
module Lits = Literals

type form = F.t

module BLit = Qbf.Lit

type bool_lit =
  bool *
  [ `Box_clause of Literal.t array
  | `Qbf_artifact of Qbf.Lit.t * string
  ]

(* compare boolean literals *)
let _cmp_blit lit1 lit2 = match lit1, lit2 with
  | (_, `Box_clause _), (_, `Qbf_artifact _) -> 1
  | (_, `Qbf_artifact _), (_, `Box_clause _) -> -1
  | (s1,`Box_clause l1), (s2,`Box_clause l2) ->
      CCOrd.(bool_ s1 s2 <?> (Lits.compare, l1, l2))
  | (s1, `Qbf_artifact (i1,_)), (s2, `Qbf_artifact (i2,_)) ->
      CCOrd.(bool_ s1 s2 <?> (BLit.compare, i1, i2))
let _eq_blit l1 l2 = _cmp_blit l1 l2 = 0
let _hash_blit lit h = match lit with
  | (s,`Box_clause l) -> h |> CCHash.bool_ s |> Lits.hash_fun l
  | (s,`Qbf_artifact (i,_)) -> h |> CCHash.bool_ s |> BLit.hash_fun i

type t = {
  lits : Literal.t array;
  trail : bool_lit list;
}

let eq c1 c2 =
  Lits.eq_com c1.lits c2.lits &&
  List.length c1.trail = List.length c2.trail &&
  List.for_all2 _eq_blit c1.trail c2.trail

let hash_fun {lits;trail} h =
  h |> Lits.hash_fun lits |> CCHash.list_ _hash_blit trail

let hash = CCHash.apply hash_fun

let cmp c1 c2 =
  CCOrd.(Lits.compare c1.lits c2.lits <?> (list_ _cmp_blit, c1.trail, c2.trail))

let compare = cmp

let make lits trail = {lits; trail; }

let has_absurd_lits c = Lits.is_absurd c.lits

let is_empty c = has_absurd_lits c && List.length c.trail = 0

let iter c f = Array.iter f c.lits

let to_seq c = Sequence.of_array c.lits

let _pp_blit buf lit = match lit with
  | (s, `Box_clause l) ->
      let prefix = if s then "" else "¬" in
      Printf.bprintf buf "%s⟦%a⟧" prefix Lits.pp l
  | (s, `Qbf_artifact (_,n)) ->
      let prefix = if s then "" else "¬" in
      Printf.bprintf buf "%s%s" prefix n

let _pp_trail buf = function
  | [] -> ()
  | l -> Printf.bprintf buf " ← %a" (Util.pp_list ~sep:" ⊓ " _pp_blit) l

let pp buf c =
  begin match c.lits with
  | [| |] -> Buffer.add_string buf "⊥"
  | [| x |] -> Lit.pp buf x
  | l -> Printf.bprintf buf "%a" (Util.pp_array ~sep:" ∨ " Lit.pp) l
  end;
  _pp_trail buf c.trail

(* lits -> closed formula *)
let _c2f lits = F.close_forall (Lits.to_form lits)
let _blit2f = function
  | (sign,`Box_clause lits) ->
      let f = _c2f lits in
      if sign then F.Base.not_ f else f
  | (_, `Qbf_artifact (_,n)) ->
      failwith ("cannot encode QBF artifact " ^ n ^ " to TPTP")

let pp_trail_tstp buf = function
  | [] -> ()
  | l ->
      let forms = List.map _blit2f l in
      Printf.bprintf buf " <= (%a)" (Util.pp_list ~sep:" & " F.TPTP.pp) forms

let pp_tstp buf c =
  begin match c.lits with
  | [| |] -> Buffer.add_string buf "$false"
  | [| x |] -> Lit.pp_tstp buf x
  | l -> Printf.bprintf buf "(%a)" (Util.pp_array ~sep:" | " Lit.pp_tstp) l
  end;
  pp_trail_tstp buf c.trail

let to_forms c =
  Array.map (fun l -> Lit.Conv.to_form l) c.lits

let to_form c =
  let l = to_forms c |> Array.to_list in
  F.close_forall (F.Base.or_ l)

let to_string c =
  Util.on_buffer pp c

let lits c = c.lits
let trail c = c.trail

let fmt fmt c =
  Format.pp_print_string fmt (to_string c)

