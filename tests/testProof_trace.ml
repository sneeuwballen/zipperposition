open Logtk
open Logtk_proofs

(* Proof-trace encoder/decoder round-trip tests.
   These build a small [Proof.t] DAG, encode it with [Proof_trace.emit_proof],
   decode with [Proof_trace_decode.decode_proof], and compare the resulting
   [LLProof.t] against the one produced directly by [LLProof_conv.conv]. *)

let prop_ty = Type.prop
let a_name = Name.make "a"
let b_name = Name.make "b"
let c_name = Name.make "c"
let t_a = Term.const ~ty:prop_ty a_name
let t_b = Term.const ~ty:prop_ty b_name
let t_c = Term.const ~ty:prop_ty c_name
let lit_a_eq_b = Literal.mk_eq t_a t_b
let lit_a_neq_b = Literal.mk_neq t_a t_b
let lit_b_eq_c = Literal.mk_eq t_b t_c
let empty_clause = [||]

(* Dummy form used as proof result (the encoder only reads literals via
   [get_lits], never the result itself). *)
let dummy_form = TypedSTerm.of_string ~ty:TypedSTerm.prop "dummy"
let dummy_result = Proof.Result.of_form dummy_form
let src = Proof.Src.from_file ~name:"test" "test.p"

let clause_equal a b =
  Array.length a = Array.length b
  && Array.for_all2 (fun l1 l2 -> Literal.compare l1 l2 = 0) a b

let inst_equal (a : LLProof.inst) (b : LLProof.inst) =
  List.length a = List.length b
  && List.for_all2
       (fun (v1, t1) (v2, t2) -> HVar.id v1 = HVar.id v2 && Term.equal t1 t2)
       a b

let rec llproof_struct_equal (a : LLProof.t) (b : LLProof.t) : bool =
  clause_equal (LLProof.concl a) (LLProof.concl b)
  &&
  match LLProof.step a, LLProof.step b with
  | LLProof.Goal, LLProof.Goal -> true
  | LLProof.Assert, LLProof.Assert -> true
  | LLProof.Trivial, LLProof.Trivial -> true
  | LLProof.By_def id1, LLProof.By_def id2 -> Name.equal id1 id2
  | LLProof.Define id1, LLProof.Define id2 -> Name.equal id1 id2
  | LLProof.Esa (n1, ps1), LLProof.Esa (n2, ps2) ->
    String.equal n1 n2
    && List.length ps1 = List.length ps2
    && List.for_all2 llproof_struct_equal ps1 ps2
  | ( LLProof.Inference { name = n1; tags = t1; parents = pa1 },
      LLProof.Inference { name = n2; tags = t2; parents = pa2 } ) ->
    String.equal n1 n2
    && List.length t1 = List.length t2
    && List.for_all2 (fun x y -> Builtin.Tag.compare x y = 0) t1 t2
    && List.length pa1 = List.length pa2
    && List.for_all2 parent_struct_equal pa1 pa2
  | _ -> false

and parent_struct_equal (pa : LLProof.parent) (pb : LLProof.parent) : bool =
  llproof_struct_equal pa.LLProof.p_proof pb.LLProof.p_proof
  && inst_equal pa.LLProof.p_inst pb.LLProof.p_inst

(* --- Encode/decode via in-memory buffer --- *)

let encode_decode root ~get_lits : LLProof.t =
  let buf = Buffer.create 4096 in
  let enc =
    Proof_trace.create
      (object
        method write s ofs len = Buffer.add_subbytes buf s ofs len
      end)
  in
  let _ = Proof_trace.emit_proof enc ~get_lits root in
  Proof_trace.close enc;
  let data = Buffer.contents buf in
  let dec = Proof_trace_decode.create data in
  let proof, _footer = Proof_trace_decode.decode_proof dec in
  proof

let test_multistep =
  ( "multi-step roundtrip",
    `Quick,
    fun () ->
      let step_ax = Proof.Step.intro src Proof.R_assert in
      let step_goal = Proof.Step.goal src in
      let p_ax = Proof.S.mk_f step_ax dummy_form in
      let p_goal = Proof.S.mk_f step_goal dummy_form in
      let rule = Proof.Rule.mk "resolution" in
      let p_false =
        Proof.S.mk_f_inference ~rule dummy_form
          [ Proof.Parent.from p_ax; Proof.Parent.from p_goal ]
      in
      let lits = Proof.S.Tbl.create 16 in
      Proof.S.Tbl.add lits p_ax [| lit_a_neq_b |];
      Proof.S.Tbl.add lits p_goal [| lit_a_eq_b |];
      Proof.S.Tbl.add lits p_false empty_clause;
      let get_lits p = Proof.S.Tbl.find lits p in
      let expected = LLProof_conv.conv ~get_clause:get_lits p_false in
      let actual = encode_decode p_false ~get_lits in
      Alcotest.(check bool)
        "multi-step structure preserved" true
        (llproof_struct_equal expected actual) )

(* --- Test 2: single trivial step --- *)

let test_trivial =
  ( "trivial roundtrip",
    `Quick,
    fun () ->
      let p_trivial = Proof.S.mk_f_trivial dummy_form in
      let lits = Proof.S.Tbl.create 4 in
      Proof.S.Tbl.add lits p_trivial [| lit_a_eq_b |];
      let get_lits p = Proof.S.Tbl.find lits p in
      let expected = LLProof_conv.conv ~get_clause:get_lits p_trivial in
      let actual = encode_decode p_trivial ~get_lits in
      Alcotest.(check bool)
        "trivial structure preserved" true
        (llproof_struct_equal expected actual) )

(* --- Test 3: inference with tags --- *)

let test_tags =
  ( "tagged inference roundtrip",
    `Quick,
    fun () ->
      let step_ax = Proof.Step.intro src Proof.R_assert in
      let p_ax = Proof.S.mk_f step_ax dummy_form in
      let step =
        Proof.Step.inference
          ~tags:[ Builtin.Tag.T_ho; Builtin.Tag.T_lia ]
          ~rule:(Proof.Rule.mk "sup")
          [ Proof.Parent.from p_ax ]
      in
      let p_inf = Proof.S.mk step dummy_result in
      let lits = Proof.S.Tbl.create 4 in
      Proof.S.Tbl.add lits p_ax [| lit_a_neq_b |];
      Proof.S.Tbl.add lits p_inf [| lit_b_eq_c |];
      let get_lits p = Proof.S.Tbl.find lits p in
      let expected = LLProof_conv.conv ~get_clause:get_lits p_inf in
      let actual = encode_decode p_inf ~get_lits in
      Alcotest.(check bool)
        "tagged inference structure preserved" true
        (llproof_struct_equal expected actual) )

(* --- Test 4: deeper DAG (3-level chain) --- *)

let test_chain =
  ( "chain roundtrip",
    `Quick,
    fun () ->
      let step_ax = Proof.Step.intro src Proof.R_assert in
      let p_ax = Proof.S.mk_f step_ax dummy_form in
      let rule = Proof.Rule.mk "sup" in
      let p1 =
        Proof.S.mk_f_inference ~rule dummy_form [ Proof.Parent.from p_ax ]
      in
      let p2 =
        Proof.S.mk_f_inference ~rule dummy_form [ Proof.Parent.from p1 ]
      in
      let p3 =
        Proof.S.mk_f_inference ~rule dummy_form [ Proof.Parent.from p2 ]
      in
      let lits = Proof.S.Tbl.create 8 in
      Proof.S.Tbl.add lits p_ax [| lit_a_eq_b |];
      Proof.S.Tbl.add lits p1 [| lit_b_eq_c |];
      Proof.S.Tbl.add lits p2 [| lit_a_neq_b |];
      Proof.S.Tbl.add lits p3 empty_clause;
      let get_lits p = Proof.S.Tbl.find lits p in
      let expected = LLProof_conv.conv ~get_clause:get_lits p3 in
      let actual = encode_decode p3 ~get_lits in
      Alcotest.(check bool)
        "chain structure preserved" true
        (llproof_struct_equal expected actual) )

(* --- Test 5: AC tag round-trip (symbol name must survive) --- *)

let test_ac_tag =
  ( "AC tag roundtrip",
    `Quick,
    fun () ->
      let step_ax = Proof.Step.intro src Proof.R_assert in
      let p_ax = Proof.S.mk_f step_ax dummy_form in
      let ac_sym = Name.make "plus" in
      let step =
        Proof.Step.inference
          ~tags:[ Builtin.Tag.T_ac ac_sym ]
          ~rule:(Proof.Rule.mk "ac_superpose")
          [ Proof.Parent.from p_ax ]
      in
      let p_inf = Proof.S.mk step dummy_result in
      let lits = Proof.S.Tbl.create 4 in
      Proof.S.Tbl.add lits p_ax [| lit_a_neq_b |];
      Proof.S.Tbl.add lits p_inf [| lit_b_eq_c |];
      let get_lits p = Proof.S.Tbl.find lits p in
      let expected = LLProof_conv.conv ~get_clause:get_lits p_inf in
      let actual = encode_decode p_inf ~get_lits in
      Alcotest.(check bool)
        "AC tag symbol preserved" true
        (llproof_struct_equal expected actual) )

let suite = [ test_multistep; test_trivial; test_tags; test_chain; test_ac_tag ]
