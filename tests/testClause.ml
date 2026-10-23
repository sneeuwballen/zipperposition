(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

let name_a = Name.make "a"
let name_b = Name.make "b"
let name_f = Name.make "f"
let ty = Type.term
let comp_test = Alcotest.testable Comparison.pp Comparison.equal

let mk_ctx ord =
  Ctx.create ~signature:Signature.empty ~ord ~select:(Selection.default ~ord)
    ~bool_select:(Bool_selection.from_string ~ord "none")
    ~sk_ctx:(Skolem.create ())

let mk_clause ~ord lits =
  Clause.create ~ctx:(mk_ctx ord) ~penalty:1 ~trail:Trail.empty lits
    Proof.Step.trivial

let test_oriented_rule =
  ( "clause.oriented_rule",
    `Quick,
    fun () ->
      let ord =
        Ordering.derived_ho_kbo ~ignore_quans_under_lam:false
          (Precedence.default [ name_f; name_a; name_b ])
      in
      let a = Term.const ~ty name_a in
      let f = Term.const ~ty:(Type.arrow [ ty ] ty) name_f in
      let fa = Term.app f [ a ] in
      Alcotest.(check comp_test)
        "f a > a" Comparison.Gt
        (Ordering.compare ord fa a);
      let unit = mk_clause ~ord [ Literal.mk_lit fa a true ] in
      Alcotest.(check bool)
        "unit equation is oriented" true
        (Clause.is_oriented_rule unit);
      let two =
        mk_clause ~ord [ Literal.mk_lit fa a true; Literal.mk_lit fa a false ]
      in
      Alcotest.(check bool)
        "two-literal clause is not an oriented rule" false
        (Clause.is_oriented_rule two) )

let suite = [ test_oriented_rule ]
