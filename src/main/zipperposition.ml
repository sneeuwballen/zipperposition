
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main file for the prover} *)

open Libzipperposition
open Libzipperposition_prover

let section = Const.section

let phases =
  let open Phases.Infix in
  Phases_impl.setup_gc >>= fun () ->
  Phases_impl.setup_signal >>= fun () ->
  Phases_impl.parse_cli >>= fun (files, _params) ->
  Phases_impl.load_extensions >>= fun _ ->
  Phases_impl.process_files_and_print files >>= fun () ->
  Phases.exit

let () =
  match Phases.run phases with
  | CCResult.Error msg ->
      print_endline msg;
      exit 1
  | CCResult.Ok (_, ()) -> ()

let _ =
  at_exit
    (fun () ->
      Util.debugf ~section 1 "run time: %.3f" (fun k->k (Util.total_time_s ()));
      Signal.send Signals.on_exit 0)
