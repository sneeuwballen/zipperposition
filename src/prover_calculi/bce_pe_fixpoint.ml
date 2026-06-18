open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_fp_inprocessing = Flex_state.create_key ()

module BCE = Bce
module PE = Pred_elim

let run_fixpoint () =
  PE.begin_fixpoint ();
  BCE.begin_fixpoint ();

  let done_ = ref false in
  while not !done_ do
    ignore (PE.fixpoint_step ());
    done_ := not (BCE.fixpoint_step ())
  done;

  PE.end_fixpoint ();
  BCE.end_fixpoint ()

let steps = ref 0

let inprocessing () =
  if !steps = 0 then (
    let done_ = ref false in
    while not !done_ do
      ignore (PE.fixpoint_step ());
      done_ := not (BCE.fixpoint_step ())
    done
  );

  steps := (!steps + 1) mod Env.flex_get_of (Env.get_global ()) k_check_at

let setup () =
  if Env.flex_get_of (Env.get_global ()) k_enabled then
    if Env.flex_get_of (Env.get_global ()) k_fp_inprocessing then (
      Env.flex_add_of (Env.get_global ()) Pred_elim.k_enabled true;
      Env.flex_add_of (Env.get_global ()) Bce.k_enabled true;
      PE.setup ~in_fp_mode:true ();
      BCE.setup ~in_fp_mode:true ();
      Env.Ctx.lost_completeness (Env.get_ctx (Env.get_global ()));
      Env.add_clause_elimination_rule (Env.get_global ()) ~priority:5
        "bce-pe-fp" inprocessing
    ) else
      Signal.once (Env.on_start (Env.get_global ())) run_fixpoint

let _enabled = ref false
let _check_at = ref 10
let _inprocessing = ref false

let extension =
  let action (env : Env.t) =
    Env.flex_add_of (Env.get_global ()) k_enabled !_enabled;
    Env.flex_add_of (Env.get_global ()) k_fp_inprocessing !_inprocessing;
    Env.flex_add_of (Env.get_global ()) k_check_at !_check_at;
    setup ()
  in
  {
    Extensions.default with
    Extensions.name = "bce_pe_fp";
    prio = 90;
    env_actions = [ action ];
  }

let () =
  Options.add_opts
    [
      ( "--bce-pe-fixpoint",
        Arg.Bool (( := ) _enabled),
        " enable BCE/PE fixpoint simplification" );
      ( "--bce-pe-fixpoint-inprocessing",
        Arg.Bool (( := ) _inprocessing),
        " enable BCE/PE fixpoint as inprocessing rule" );
      ( "--bce-pe-fixpoint-check-at",
        Arg.Int (( := ) _check_at),
        " BCE/PE fixpoint inprocessing periodicity" );
    ]
