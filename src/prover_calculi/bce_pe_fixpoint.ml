open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_fp_inprocessing = Flex_state.create_key ()

module type S = sig
  module Env : Env.S

  val setup : unit -> unit
  (** {5 Registration} *)
end

module Make (E : Env.S) : S with module Env = E = struct
  module OuterEnv = Env
  module Env = E
  module BCE = Bce.Make (E)
  module PE = Pred_elim.Make (E)

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

    steps :=
      (!steps + 1) mod OuterEnv.flex_get_of (OuterEnv.get_global ()) k_check_at

  let setup () =
    if OuterEnv.flex_get_of (OuterEnv.get_global ()) k_enabled then
      if OuterEnv.flex_get_of (OuterEnv.get_global ()) k_fp_inprocessing then (
        OuterEnv.flex_add_of (OuterEnv.get_global ()) Pred_elim.k_enabled true;
        OuterEnv.flex_add_of (OuterEnv.get_global ()) Bce.k_enabled true;
        PE.setup ~in_fp_mode:true ();
        BCE.setup ~in_fp_mode:true ();
        Env.Ctx.lost_completeness (OuterEnv.get_ctx (OuterEnv.get_global ()));
        OuterEnv.add_clause_elimination_rule (OuterEnv.get_global ())
          ~priority:5 "bce-pe-fp" inprocessing
      ) else
        Signal.once (OuterEnv.on_start (OuterEnv.get_global ())) run_fixpoint
end

let _enabled = ref false
let _check_at = ref 10
let _inprocessing = ref false

let extension =
  let action (env : Env.t) =
    let module E = (val (module Env) : Env.S) in
    let module FP = Make (E) in
    Env.flex_add_of (Env.get_global ()) k_enabled !_enabled;
    Env.flex_add_of (Env.get_global ()) k_fp_inprocessing !_inprocessing;
    Env.flex_add_of (Env.get_global ()) k_check_at !_check_at;
    FP.setup ()
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
