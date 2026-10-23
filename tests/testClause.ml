(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

let name_a = Name.make "a"
let name_b = Name.make "b"
let name_f = Name.make "f"
let name_p = Name.make "p"
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

let test_eligible_param =
  ( "clause.eligible_param",
    `Quick,
    fun () ->
      let ord =
        Ordering.derived_ho_kbo ~ignore_quans_under_lam:false
          (Precedence.default [ name_f; name_a; name_b ])
      in
      let a = Term.const ~ty name_a in
      let f = Term.const ~ty:(Type.arrow [ ty ] ty) name_f in
      let fa = Term.app f [ a ] in
      let mk bool_select lits =
        let ctx =
          Ctx.create ~signature:Signature.empty ~ord
            ~select:(Selection.default ~ord) ~bool_select
            ~sk_ctx:(Skolem.create ())
        in
        Clause.create ~ctx ~penalty:1 ~trail:Trail.empty lits Proof.Step.trivial
      in
      let no_bool_select = Bool_selection.from_string ~ord "none" in
      let param c =
        Clause.eligible_param (Scoped.make c 0) Subst.empty |> CCBV.to_list
      in
      let is_eligible c =
        Clause.is_eligible_param (Scoped.make c 0) Subst.empty ~idx:0
      in
      let eq = mk no_bool_select [ Literal.mk_lit fa a true ] in
      Alcotest.(check (list int))
        "no bool-selected: maximal positive literal is eligible" [ 0 ]
        (param eq);
      Alcotest.(check bool)
        "no bool-selected: is_eligible_param" true (is_eligible eq);
      let p = Term.const ~ty:Type.prop name_p in
      let neg_eq = mk no_bool_select [ Literal.mk_lit p Term.false_ true ] in
      Alcotest.(check (list int))
        "positive p = false literal is eligible" [ 0 ] (param neg_eq);
      let bool_sel =
        mk (fun _ -> [ fa, Position.stop ]) [ Literal.mk_lit fa a true ]
      in
      Alcotest.(check bool)
        "mock bool-select is non-empty" true
        (not (CCList.is_empty (Clause.bool_selected bool_sel)));
      Alcotest.(check (list int))
        "bool-selected: no eligible literal" [] (param bool_sel);
      Alcotest.(check bool)
        "bool-selected: not is_eligible_param" false (is_eligible bool_sel);
      (* max is recomputed on the substituted literals *)
      let xv = HVar.fresh ~ty () in
      let x = Term.var xv in
      let two =
        mk no_bool_select
          [
            Literal.mk_lit x a true;
            Literal.mk_lit (Term.const ~ty name_b) a true;
          ]
      in
      Alcotest.(check bool)
        "b=a maximal without substitution" true
        (Clause.is_eligible_param (Scoped.make two 0) Subst.empty ~idx:1);
      let subst = Subst.FO.of_list' [ Scoped.make xv 0, Scoped.make fa 0 ] in
      Alcotest.(check bool)
        "b=a not maximal after substituting x := f a" false
        (Clause.is_eligible_param (Scoped.make two 0) subst ~idx:1) )

let suite = [ test_oriented_rule; test_eligible_param ]
