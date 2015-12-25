
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

(** Test formulas *)

open Logtk
open Logtk_arbitrary
open OUnit

module F = Formula.FO

(** OUnit *)

let printer = F.to_string

(* parse a formula *)
let pform s =
  let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
  let ctx = TypeInference.Ctx.create Signature.TPTP.base in
  TypeInference.FO.convert_form_exn ~ctx f

let test_mk_not () =
  assert_equal ~cmp:F.equal ~printer F.Base.true_ (F.Base.not_ F.Base.false_);
  assert_equal ~cmp:F.equal ~printer F.Base.false_ (F.Base.not_ F.Base.true_);
  ()

let test_simplify () =
  let f = pform "![X]: (p(a) <=> ($true => $false))" in
  let f' = pform "~ p(a)" in
  assert_equal ~cmp:F.equal ~printer f' (F.simplify f);
  ()

let suite =
  "test_formula" >:::
    [ "test_mk_not" >:: test_mk_not
    ; "test_simplify" >:: test_simplify
    ]

(** QuickCheck *)

open QCheck

let pp = F.to_string

let __closed f = ScopedTerm.DB.closed (f : F.t :> ScopedTerm.t)

let check_simplify_preserve_closed =
  let gen = ArForm.default in
  let name = "formula_simplify_preserve_closed" in
  let prop f =
    __closed f = __closed (F.simplify f)
  in
  mk_test ~n:1000 ~pp ~name gen prop

let check_db_lift_preserve_closed =
  let gen = ArForm.default in
  let name = "formula_db_shift_preserve_closed" in
  let prop f =
    Prop.assume (__closed f);
    ScopedTerm.DB.closed (ScopedTerm.DB.shift 1 (f : F.t :> ScopedTerm.t))
  in
  mk_test ~n:100 ~pp ~name gen prop

let check_db_unlift_preserve_closed =
  let gen = ArForm.default in
  let name = "formula_db_unshift_preserve_closed" in
  let prop f =
    Prop.assume (__closed f);
    ScopedTerm.DB.closed (ScopedTerm.DB.unshift 1 (f : F.t :> ScopedTerm.t))
  in
  mk_test ~n:100 ~pp ~name gen prop

let check_forall_close_is_closed =
  let gen = ArForm.atom in
  let name = "formula_forall_close_is_closed" in
  let prop f =
    F.is_closed (F.close_forall f)
  in
  mk_test ~n:100 ~pp ~name gen prop

let props =
  [ check_simplify_preserve_closed
  ; check_db_lift_preserve_closed
  ; check_db_unlift_preserve_closed
  ; check_forall_close_is_closed
  ]
