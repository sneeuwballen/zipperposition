open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_fp_inprocessing = Flex_state.create_key ()

module BCE = Bce
module PE = Pred_elim

let run_fixpoint env =
  PE.begin_fixpoint env;
  BCE.begin_fixpoint env;

  let done_ = ref false in
  while not !done_ do
    ignore (PE.fixpoint_step env);
    done_ := not (BCE.fixpoint_step env)
  done;

  PE.end_fixpoint env;
  BCE.end_fixpoint env

let steps = ref 0

let inprocessing env =
  if !steps = 0 then (
    let done_ = ref false in
    while not !done_ do
      ignore (PE.fixpoint_step env);
      done_ := not (BCE.fixpoint_step env)
    done
  );

  steps := (!steps + 1) mod Env.flex_get_of env k_check_at

let setup env =
  if Env.flex_get_of env k_enabled then
    if Env.flex_get_of env k_fp_inprocessing then (
      Env.flex_add_of env Pred_elim.k_enabled true;
      Env.flex_add_of env Bce.k_enabled true;
      PE.setup env ~in_fp_mode:true;
      BCE.setup env ~in_fp_mode:true;
      Env.Ctx.lost_completeness (Env.get_ctx env);
      Env.add_clause_elimination_rule env ~priority:5 "bce-pe-fp" inprocessing
    ) else
      Signal.once (Env.on_start env) (fun () -> run_fixpoint env)

let extension =
  let action (env : Env.t) =
    Env.flex_ensure env k_enabled false;
    Env.flex_ensure env k_fp_inprocessing false;
    Env.flex_ensure env k_check_at 10;
    setup env
  in
  {
    Extensions.default with
    Extensions.name = "bce_pe_fp";
    prio = 90;
    env_actions = [ action ];
  }

let () =
  Params.add_flex_opts (fun ~flex_ref ->
      [
        ( "--bce-pe-fixpoint",
          Arg.Bool (fun v -> flex_ref := Flex_state.add k_enabled v !flex_ref),
          " enable BCE/PE fixpoint simplification" );
        ( "--bce-pe-fixpoint-inprocessing",
          Arg.Bool
            (fun v -> flex_ref := Flex_state.add k_fp_inprocessing v !flex_ref),
          " enable BCE/PE fixpoint as inprocessing rule" );
        ( "--bce-pe-fixpoint-check-at",
          Arg.Int (fun n -> flex_ref := Flex_state.add k_check_at n !flex_ref),
          " BCE/PE fixpoint inprocessing periodicity" );
      ])
