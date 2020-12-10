open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()

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

  let setup () =
    if E.flex_get k_enabled then (
      Signal.once E.on_start run_fixpoint);

end

let _enabled = ref false


let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module FP = Make(E) in

    E.flex_add k_enabled !_enabled;

    FP.setup ()
  in
  { Extensions.default with Extensions.
                         name="bce_pe_fp";
                         prio = 70;
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--bce-pe-fixpoint", Arg.Bool ((:=) _enabled), " enable BCE/PE fixpoint simplification";
  ]

