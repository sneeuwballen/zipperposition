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

let suite = [ test_sos_split; test_cr_add ]
