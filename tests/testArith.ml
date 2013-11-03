
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

(** {1 Test arithmetic} *)

open Logtk
open Libzipperposition
open Libzipperposition_arbitrary
open QCheck

module T = FOTerm
module M = Monome
module E = Arith.Lit.Extracted

let arTerm ty =
  Arbitrary.(lift M.to_term (ArMonome.of_ty ty))

(* simplify (simplify t) == simplify t *)
let check_simplify_idempotent =
  let gen = Arbitrary.(among [Type.int; Type.rat] >>= arTerm) in
  let prop t =
    let t' = Arith.T.simplify t in
    let t'' = Arith.T.simplify t' in
    T.eq t' t''
  in
  let name = "arith_term_simplify_is_idempotent" in
  mk_test ~name ~n:1000 ~size:T.size ~pp:T.to_string gen prop

(* if e non trivial {!Arith.Lit.Extracted.t}, then
   to_lit (extract lit) = lit *)
let check_lit_extract_inverse_is_to_lit n =
  let gen = Arbitrary.(among [Type.int; Type.rat] >>= fun ty ->
    Arith.Lit.arbitrary ty)
  in
  let prop lit =
    let symbols = Literal.symbols lit in
    Prop.assume (not (Arith.Lit.is_trivial lit));
    (* create ordering *)
    let ord = Ordering.default symbols in
    (* lit -> elit -> lit' -> elit' -> lit'' *)
    let elit = E.extract lit in
    let lit' = E.to_lit ~ord elit in
    let elit' = E.extract lit' in
    let lit'' = E.to_lit ~ord elit' in
    (* check lit' = lit'' *)
    let res = Literal.eq_com lit' lit'' in
    if not res then begin
      Util.debug 1 "lit: %a" Literal.pp lit;
      Util.debug 1 "elit: %a" E.pp elit;
      Util.debug 1 "lit': %a" Literal.pp lit';
      Util.debug 1 "elit': %a" E.pp elit';
      Util.debug 1 "lit'': %a" Literal.pp lit'';
      end;
    res
  in
  let name = "arith_lit_extract_inverse_is_to_lit" in
  let size = Literal.weight in
  mk_test ~name ~n ~size ~pp:Literal.to_string gen prop

let props =
  [ check_simplify_idempotent
  ; check_lit_extract_inverse_is_to_lit 1000
  ]
