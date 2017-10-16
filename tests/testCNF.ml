
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Tests for CNF *)

open Logtk
open Logtk_arbitrary

module T = TypedSTerm
module F = T.Form

let check_cnf_gives_clauses =
  let gen = QCheck.(map_same_type F.close_forall ArForm.default) in
  let name = "cnf_gives_clauses" in
  (* check that the CNf of a formula is in clausal form *)
  let prop f =
    let proof = Proof.Step.intro (Proof.Src.from_file "<none>") Proof.R_goal in
    Cnf.cnf_of (Statement.assert_ ~proof f)
    |> CCVector.flat_map_list
      (fun st -> match Statement.view st with
        | Statement.Data _
        | Statement.Def _
        | Statement.Rewrite _
        | Statement.TyDecl (_,_) -> []
        | Statement.Lemma l
        | Statement.NegatedGoal (_,l) -> l
        | Statement.Goal c
        | Statement.Assert c -> [c])
    |> CCVector.map
      (fun c -> F.or_ (List.map SLiteral.to_form c))
    |> CCVector.for_all Cnf.is_clause
  in
  QCheck.Test.make ~long_factor:20 ~name gen prop

let check_miniscope_db_closed =
  let gen = QCheck.(map F.close_forall ArForm.default) in
  let name = "cnf_miniscope_db_closed" in
  (* check that miniscoping preserved db_closed *)
  let prop f =
    let f = Cnf.miniscope f in
    T.closed f
  in
  QCheck.Test.make ~long_factor:20 ~name gen prop

let props =
  [ check_cnf_gives_clauses
  ; check_miniscope_db_closed
  ]
