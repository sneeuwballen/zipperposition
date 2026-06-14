open Logtk
module TD = Term_dag

let ty = Type.term
let prop = Type.prop
let f_id = Name.make "f"
let a_id = Name.make "a"
let b_id = Name.make "b"

let rec structurally_equal (a : InnerTerm.t) (b : InnerTerm.t) : bool =
  match InnerTerm.view a, InnerTerm.view b with
  | InnerTerm.Var hv1, InnerTerm.Var hv2 ->
    HVar.id hv1 = HVar.id hv2 && structurally_equal (HVar.ty hv1) (HVar.ty hv2)
  | InnerTerm.DB n1, InnerTerm.DB n2 -> n1 = n2
  | InnerTerm.Const id1, InnerTerm.Const id2 -> Name.equal id1 id2
  | InnerTerm.Bind (b1, vt1, t1), InnerTerm.Bind (b2, vt2, t2) ->
    Binder.equal b1 b2 && structurally_equal vt1 vt2 && structurally_equal t1 t2
  | InnerTerm.App (f1, args1), InnerTerm.App (f2, args2) ->
    structurally_equal f1 f2
    && List.length args1 = List.length args2
    && List.for_all2 structurally_equal args1 args2
  | InnerTerm.AppBuiltin (b1, args1), InnerTerm.AppBuiltin (b2, args2) ->
    Builtin.equal b1 b2
    && List.length args1 = List.length args2
    && List.for_all2 structurally_equal args1 args2
  | _ -> false

let roundtrip (term : Term.t) =
  let data, _ =
    TD.encode_to_string (fun td ->
        ignore (TD.encode_term td (term :> InnerTerm.t)))
  in
  let decoded = TD.decode_all data in
  let t =
    match List.rev decoded with
    | [] -> Alcotest.fail "no terms decoded"
    | (_, t) :: _ -> t
  in
  structurally_equal (term :> InnerTerm.t) t

let test_const =
  ( "const roundtrip",
    `Quick,
    fun () ->
      let c = Term.const ~ty f_id in
      Alcotest.(check bool) "const roundtrip" true (roundtrip c) )

let test_app =
  ( "app roundtrip",
    `Quick,
    fun () ->
      let f = Term.const ~ty:Type.([ ty; ty ] ==> ty) f_id in
      let a = Term.const ~ty a_id in
      let b = Term.const ~ty b_id in
      let t = Term.app f [ a; b ] in
      Alcotest.(check bool) "app roundtrip" true (roundtrip t) )

let test_bvar =
  ( "bvar roundtrip",
    `Quick,
    fun () ->
      let t = Term.bvar ~ty 0 in
      Alcotest.(check bool) "bvar roundtrip" true (roundtrip t) )

let test_bind =
  ( "bind roundtrip",
    `Quick,
    fun () ->
      let body = Term.bvar ~ty 0 in
      let t = Term.fun_ ty body in
      Alcotest.(check bool) "bind roundtrip" true (roundtrip t) )

let test_builtin_int =
  ( "builtin Int roundtrip",
    `Quick,
    fun () ->
      let t =
        Term.app_builtin ~ty:Type.int
          (Builtin.make_payload (Builtin.Int (Z.of_int 42)))
          []
      in
      Alcotest.(check bool) "builtin int roundtrip" true (roundtrip t) )

let test_builtin_arrow =
  ( "builtin Arrow roundtrip",
    `Quick,
    fun () ->
      let arr = Type.([ ty ] ==> ty) in
      let t = Term.of_ty arr in
      Alcotest.(check bool) "arrow roundtrip" true (roundtrip t) )

let test_nested_app =
  ( "nested app roundtrip",
    `Quick,
    fun () ->
      let f = Term.const ~ty:Type.([ ty; ty; ty ] ==> ty) f_id in
      let a = Term.const ~ty a_id in
      let t = Term.app f [ a; a; a ] in
      Alcotest.(check bool) "nested app roundtrip" true (roundtrip t) )

let test_quantifier =
  ( "forall bind roundtrip",
    `Quick,
    fun () ->
      let body = Term.bvar ~ty 0 in
      let t =
        InnerTerm.bind
          ~ty:(prop :> InnerTerm.t)
          ~varty:(ty :> InnerTerm.t)
          Binder.Forall
          (body :> InnerTerm.t)
      in
      let data, _ =
        TD.encode_to_string (fun td -> ignore (TD.encode_term td t))
      in
      let decoded = TD.decode_all data in
      let t' =
        match List.rev decoded with
        | [] -> Alcotest.fail "no terms decoded"
        | (_, t) :: _ -> t
      in
      Alcotest.(check bool)
        "forall bind roundtrip" true (structurally_equal t t') )

let test_sharing =
  ( "sharing roundtrip",
    `Quick,
    fun () ->
      let a = Term.const ~ty a_id in
      let f = Term.const ~ty:Type.([ ty; ty ] ==> ty) f_id in
      let t = Term.app f [ a; a ] in
      let data, () =
        TD.encode_to_string (fun td ->
            ignore (TD.encode_term td (t :> InnerTerm.t));
            ignore (TD.encode_term td (t :> InnerTerm.t)))
      in
      let decoded = TD.decode_all data in
      let n = List.length decoded in
      let _, last = List.nth decoded (n - 1) in
      Alcotest.(check bool)
        "sharing roundtrip" true
        (structurally_equal (t :> InnerTerm.t) last) )

let suite =
  [
    test_const;
    test_app;
    test_bvar;
    test_bind;
    test_builtin_int;
    test_builtin_arrow;
    test_nested_app;
    test_quantifier;
    test_sharing;
  ]
