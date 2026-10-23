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
  let env =
    Env.create ~params:Params.default ~flex_state:Flex_state.empty ~ctx ()
  in
  let sat = Sat_solver.create () in
  Sat_solver.setup sat;
  Env.flex_add_of env Libzipperposition_avatar.k_sat_solver sat;
  env

(* [convert_lemma] must convert its literals with the Env's own ctx, so a hook
   registered on that ctx is applied. Guards vhq2. *)
let test_convert_lemma_uses_env_ctx =
  ( "avatar.convert_lemma.env_ctx",
    `Quick,
    fun () ->
      let env = mk_env () in
      let p = Term.const ~ty:Type.prop (Name.make "p") in
      let st : Statement.clause_t =
        Statement.lemma ~proof:Proof.Step.trivial [ [ SLiteral.atom_true p ] ]
      in
      Ctx.add_lit_from_hook (Env.get_ctx env) (fun _ -> Some Literal.mk_tauto);
      match Libzipperposition_avatar.convert_lemma env st with
      | Env.CR_return [ c ] ->
        Alcotest.(check int) "one literal" 1 (Array.length (Clause.lits c));
        Alcotest.(check bool)
          "hook applied" true
          (Literal.equal (Clause.lits c).(0) Literal.mk_tauto)
      | Env.CR_return l -> Alcotest.(check int) "one clause" 1 (List.length l)
      | Env.CR_skip | Env.CR_drop | Env.CR_add _ ->
        Alcotest.fail "expected CR_return" )

let suite = [ test_convert_lemma_uses_env_ctx ]
