(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

let mk_env () =
  let ord = Ordering.none in
  let ctx =
    Ctx.create ~signature:Signature.empty ~ord ~select:(Selection.default ~ord)
      ~bool_select:(Bool_selection.from_string ~ord "none")
      ~sk_ctx:(Skolem.create ())
  in
  Env.create ~params:Params.default ~flex_state:Flex_state.empty ~ctx ()

let mk_assert ?(attrs = []) () =
  Statement.assert_ ~attrs ~proof:Proof.Step.trivial ([] : Statement.clause)

let mk_clause env =
  Clause.create ~ctx:(Env.get_ctx env) ~penalty:1 ~trail:Trail.empty
    [ Literal.mk_tauto ] Proof.Step.trivial

let mk_stream l = Stream.make ~parents:[] (OSeq.of_list l)

let queue_with ~guard ~ratio ~clause_num =
  let env = mk_env () in
  Env.flex_ensure env StreamQueue.k_guard guard;
  Env.flex_ensure env StreamQueue.k_ratio ratio;
  Env.flex_ensure env StreamQueue.k_clause_num clause_num;
  env, Env.get_stm_queue env

let test_stm_queue_guard =
  ( "env.stm_queue.guard",
    `Quick,
    fun () ->
      let env, q = queue_with ~guard:3 ~ratio:100 ~clause_num:(-1) in
      Env.StmQ.add q (mk_stream [ None; None; Some (mk_clause env) ]);
      Alcotest.(check int)
        "guard is read from flex state" 1
        (List.length (Env.StmQ.take_stm_nb q)) )

let test_stm_queue_ratio =
  ( "env.stm_queue.ratio",
    `Quick,
    fun () ->
      let env, q = queue_with ~guard:30 ~ratio:0 ~clause_num:(-1) in
      Env.StmQ.add q (mk_stream [ Some (mk_clause env); Some (mk_clause env) ]);
      Env.StmQ.add q (mk_stream [ Some (mk_clause env); Some (mk_clause env) ]);
      Alcotest.(check int)
        "ratio is read from flex state" 1
        (List.length (Env.StmQ.take_stm_nb q)) )

let test_stm_queue_clause_num =
  ( "env.stm_queue.clause_num",
    `Quick,
    fun () ->
      let env, q = queue_with ~guard:30 ~ratio:100 ~clause_num:1 in
      Env.StmQ.add q (mk_stream [ Some (mk_clause env); Some (mk_clause env) ]);
      Env.StmQ.add q (mk_stream [ Some (mk_clause env); Some (mk_clause env) ]);
      Alcotest.(check int)
        "clause_num is read from flex state" 1
        (List.length (Env.StmQ.take_stm_nb q)) )

let test_sos_split =
  ( "env.convert_input_statements.sos",
    `Quick,
    fun () ->
      let env = mk_env () in
      let stmts =
        CCVector.of_list
          [ mk_assert ~attrs:[ Statement.A_sos ] (); mk_assert () ]
      in
      let sets = Env.convert_input_statements_env env stmts in
      Alcotest.(check int)
        "sos clause is in c_sos" 1
        (CCVector.length sets.Clause.c_sos);
      Alcotest.(check int)
        "non-sos clause is in c_set" 1
        (CCVector.length sets.Clause.c_set) )

let test_cr_add =
  ( "env.convert_input_statements.cr_add",
    `Quick,
    fun () ->
      let env = mk_env () in
      let extra =
        Clause.create ~ctx:(Env.get_ctx env) ~penalty:1 ~trail:Trail.empty []
          Proof.Step.trivial
      in
      Env.add_clause_conversion env (fun _ _ -> Env.CR_add [ extra ]);
      let sets =
        Env.convert_input_statements_env env (CCVector.of_list [ mk_assert () ])
      in
      Alcotest.(check int)
        "CR_add keeps converting with the remaining rules" 2
        (CCVector.length sets.Clause.c_set) )

let suite =
  [
    test_stm_queue_guard;
    test_stm_queue_ratio;
    test_stm_queue_clause_num;
    test_sos_split;
    test_cr_add;
  ]
