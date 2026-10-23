(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

let name_a = Name.make "a"
let name_b = Name.make "b"
let name_f = Name.make "f"
let name_p = Name.make "p"
let name_q = Name.make "q"
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

let test_update_trail =
  ( "clause.update_trail",
    `Quick,
    fun () ->
      let ord = Ordering.none in
      let p = Term.const ~ty:Type.prop name_p in
      let c = mk_clause ~ord [ Literal.mk_lit p Term.true_ true ] in
      let b = BBox.make_fresh () in
      let c' = Clause.update_trail (fun _ -> Trail.singleton b) c in
      Alcotest.(check bool)
        "original trail is empty" true
        (Trail.is_empty (Clause.trail c));
      Alcotest.(check bool)
        "updated trail is used" true
        (Trail.mem b (Clause.trail c'));
      Alcotest.(check bool)
        "literals are preserved" true
        (Literals.equal (Clause.lits c) (Clause.lits c')) )

let test_eta_reduce =
  ( "clause.eta_reduce",
    `Quick,
    fun () ->
      let ord = Ordering.none in
      let f = Term.const ~ty:(Type.arrow [ ty ] ty) name_f in
      let v = HVar.fresh ~ty () in
      let redex = Term.fun_of_fvars [ v ] (Term.app f [ Term.var v ]) in
      let trail = Trail.singleton (BBox.make_fresh ()) in
      let c =
        Clause.create ~ctx:(mk_ctx ord) ~penalty:7 ~trail
          [ Literal.mk_lit redex f true ]
          Proof.Step.trivial
      in
      match Clause.eta_reduce c with
      | None -> Alcotest.fail "expected an eta-reduced clause"
      | Some c' ->
        Alcotest.(check bool)
          "eta-reduced literal" true
          (Literal.equal (Clause.lits c').(0) (Literal.mk_lit f f true));
        Alcotest.(check bool)
          "penalty preserved" true
          (Clause.penalty c' = Clause.penalty c);
        Alcotest.(check bool)
          "trail preserved" true
          (Trail.equal (Clause.trail c') (Clause.trail c));
        Alcotest.(check bool)
          "proof preserved" true
          (Proof.Step.equal (Clause.proof_step c') (Clause.proof_step c)) )

let test_is_inj_axiom =
  ( "clause.is_inj_axiom",
    `Quick,
    fun () ->
      let ord = Ordering.none in
      let f = Term.const ~ty:(Type.arrow [ ty ] ty) name_f in
      let x = Term.var (HVar.fresh ~ty ()) in
      let y = Term.var (HVar.fresh ~ty ()) in
      let c =
        mk_clause ~ord
          [
            Literal.mk_lit (Term.app f [ x ]) (Term.app f [ y ]) false;
            Literal.mk_lit x y true;
          ]
      in
      match Clause.is_inj_axiom c with
      | None -> Alcotest.fail "expected an injectivity axiom"
      | Some (sym, n) ->
        Alcotest.(check bool) "injective symbol" true (Name.equal sym name_f);
        Alcotest.(check int) "injectivity argument index" 0 n )

let test_is_orphaned =
  ( "clause.is_orphaned",
    `Quick,
    fun () ->
      let ord = Ordering.none in
      let ctx = mk_ctx ord in
      let p = Term.const ~ty:Type.prop name_p in
      let lit = Literal.mk_lit p Term.true_ true in
      let parent =
        Clause.create ~ctx ~penalty:1 ~trail:Trail.empty [ lit ]
          Proof.Step.trivial
      in
      let parent_proof = Clause.proof parent in
      let child =
        Clause.create ~ctx ~penalty:1 ~trail:Trail.empty [ lit ]
          (Proof.Step.inference ~rule:(Proof.Rule.mk "test")
             [ Proof.Parent.from parent_proof ])
      in
      Alcotest.(check bool)
        "live parent is not orphaned" false (Clause.is_orphaned child);
      Clause.mark_redundant parent;
      Alcotest.(check bool)
        "dead parent makes the clause orphaned" true (Clause.is_orphaned child);
      let tagged =
        Clause.create ~ctx ~penalty:1 ~trail:Trail.empty [ lit ]
          (Proof.Step.inference ~rule:(Proof.Rule.mk "test")
             ~tags:[ Proof.Tag.T_cannot_orphan ]
             [ Proof.Parent.from parent_proof ])
      in
      Alcotest.(check bool)
        "cannot-orphan inference is never orphaned" false
        (Clause.is_orphaned tagged) )

let test_to_s_form =
  ( "clause.to_s_form",
    `Quick,
    fun () ->
      let ord = Ordering.none in
      let p = Term.const ~ty:Type.prop name_p in
      let lit = Literal.mk_lit p Term.true_ true in
      let flex_ref = ref Flex_state.empty in
      let b = BBox.inject_lits ~flex_ref [| lit |] in
      let c =
        Clause.create ~ctx:(mk_ctx ord) ~penalty:1 ~trail:(Trail.singleton b)
          [ lit ] Proof.Step.trivial
      in
      (match TypedSTerm.Form.view (Clause.to_s_form c) with
      | TypedSTerm.Form.Imply _ -> ()
      | _ -> Alcotest.fail "expected the trail to imply the clause");
      let q = Term.const ~ty:(Type.arrow [ ty ] Type.prop) name_q in
      let x = Term.var (HVar.fresh ~ty ()) in
      let c =
        mk_clause ~ord [ Literal.mk_lit (Term.app q [ x ]) Term.true_ true ]
      in
      match TypedSTerm.Form.view (Clause.to_s_form c) with
      | TypedSTerm.Form.Forall _ -> ()
      | _ -> Alcotest.fail "expected the clause to be closed" )

let suite =
  [
    test_oriented_rule;
    test_eligible_param;
    test_update_trail;
    test_eta_reduce;
    test_is_inj_axiom;
    test_is_orphaned;
    test_to_s_form;
  ]
