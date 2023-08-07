open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_fp_inprocessing = Flex_state.create_key ()

module type S = sig
  module Env : Env.S

  (** {5 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module BCE = Bce.Make(E)
  module PE = Pred_elim.Make(E)
  
  let run_fixpoint () =
    PE.begin_fixpoint();
    BCE.begin_fixpoint();

    let done_ = ref false in
    while not !done_ do
      ignore(PE.fixpoint_step());
      done_ := not (BCE.fixpoint_step ());
    done;

    PE.end_fixpoint();
    BCE.end_fixpoint()

  let steps = ref 0
  let inprocessing () =
    if !steps = 0 then (
      let done_ = ref false in
      while not !done_ do
        ignore (PE.fixpoint_step ());
        done_ := not (BCE.fixpoint_step ())
      done;
    );

    steps := (!steps + 1) mod Env.flex_get k_check_at

  let setup () =
    if E.flex_get k_enabled then (
      if E.flex_get k_fp_inprocessing then (
        E.flex_add Pred_elim.k_enabled true;
        E.flex_add Bce.k_enabled true;
        PE.setup ~in_fp_mode:true ();
        BCE.setup ~in_fp_mode:true ();
        Env.Ctx.lost_completeness ();
        E.add_clause_elimination_rule ~priority:5 "bce-pe-fp" inprocessing;
      ) else Signal.once E.on_start run_fixpoint);

end

let _enabled = ref false
let _check_at = ref 10
let _inprocessing = ref false


let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module FP = Make(E) in

    E.flex_add k_enabled !_enabled;
    E.flex_add k_fp_inprocessing !_inprocessing;
    E.flex_add k_check_at !_check_at;
    FP.setup ()
  in
  { Extensions.default with Extensions.
                         name="bce_pe_fp";
                         prio = 90;
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--bce-pe-fixpoint", Arg.Bool ((:=) _enabled), " enable BCE/PE fixpoint simplification";
    "--bce-pe-fixpoint-inprocessing", Arg.Bool ((:=) _inprocessing), " enable BCE/PE fixpoint as inprocessing rule";
    "--bce-pe-fixpoint-check-at", Arg.Int ((:=) _check_at), " BCE/PE fixpoint inprocessing periodicity";
  ]
