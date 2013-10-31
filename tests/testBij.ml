
(*
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

(** Test bij *)

open Logtk
open Logtk_arbitrary
open QCheck

module T = FOTerm
module F = FOFormula

let test_bijection_symbol =
  let gen = ArSymbol.default in
  let name = "bij_bijection_for_symb" in
  let prop t =
    let s = Bij.TrBencode.to_string ~bij:Symbol.bij t in
    let t' = Bij.TrBencode.of_string ~bij:Symbol.bij s in
    Symbol.eq t t'
  in
  mk_test ~pp:Symbol.to_string ~name gen prop

let test_bijection_term =
  let gen = ArTerm.default in
  let name = "bij_bijection_for_term" in
  let prop t =
    let s = Bij.TrBencode.to_string ~bij:T.bij t in
    let t' = Bij.TrBencode.of_string ~bij:T.bij s in
    T.eq t t'
  in
  mk_test ~pp:T.to_string ~name gen prop

let test_bijection_pred =
  let gen = ArTerm.pred in
  let name = "bij_bijection_for_pred" in
  let prop t =
    let s = Bij.TrBencode.to_string ~bij:T.bij t in
    let t' = Bij.TrBencode.of_string ~bij:T.bij s in
    T.eq t t'
  in
  mk_test ~pp:T.to_string ~name gen prop

let test_bijection_form =
  let gen = ArForm.default in
  let name = "bij_bijection_for_form" in
  let prop f =
    let s = Bij.TrBencode.to_string ~bij:F.bij f in
    let f' = Bij.TrBencode.of_string ~bij:F.bij s in
    F.ac_eq (F.simplify f) (F.simplify f')
  in
  mk_test ~pp:F.to_string_tstp ~name gen prop

let props =
  [ test_bijection_symbol
  ; test_bijection_term
  ; test_bijection_pred
  ; test_bijection_form
  ]
