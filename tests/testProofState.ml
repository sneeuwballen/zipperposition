(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Libzipperposition

let mk_clause () =
  let ord = Ordering.none in
  let ctx =
    Ctx.create ~signature:Signature.empty ~ord ~select:(Selection.default ~ord)
      ~bool_select:(Bool_selection.from_string ~ord "none")
      ~sk_ctx:(Skolem.create ())
  in
  Clause.create ~ctx ~penalty:1 ~trail:Trail.empty [] Proof.Step.trivial

let test_passive_signals =
  ( "proofState.passive_signals",
    `Quick,
    fun () ->
      let passive = (ProofState.create ()).ProofState.passive in
      let n_add = ref 0 and n_remove = ref 0 in
      let passive_on_add = ref true in
      Signal.on_every passive#on_add_clause (fun c ->
          incr n_add;
          passive_on_add := !passive_on_add && passive#is_passive c);
      Signal.on_every passive#on_remove_clause (fun _ -> incr n_remove);
      let c = mk_clause () in
      passive#add (Iter.singleton c);
      Alcotest.(check int) "one on_add per new clause" 1 !n_add;
      Alcotest.(check bool)
        "clause is passive during on_add" true !passive_on_add;
      passive#add (Iter.singleton c);
      Alcotest.(check int) "no on_add for a duplicate" 1 !n_add;
      passive#remove (Iter.singleton c);
      Alcotest.(check int) "one on_remove per present clause" 1 !n_remove;
      passive#remove (Iter.singleton c);
      Alcotest.(check int) "no on_remove for an absent clause" 1 !n_remove;
      passive#add (Iter.singleton c);
      Alcotest.(check int) "re-add fires on_add" 2 !n_add;
      (match passive#next () with
      | None -> Alcotest.fail "expected a passive clause"
      | Some c' ->
        Alcotest.(check bool) "next returns the clause" true (Clause.equal c c'));
      Alcotest.(check int) "one on_remove when a clause is taken" 2 !n_remove;
      passive#remove (Iter.singleton c);
      Alcotest.(check int) "no on_remove for a taken clause" 2 !n_remove;
      () )

let suite = [ test_passive_signals ]
