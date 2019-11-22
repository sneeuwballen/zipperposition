
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main file for the prover} *)

(** This module just calls the main {!Phases} runner that processes files
    passed as arguments on the command line. *)

open Logtk
open Libzipperposition_phases

let section = Libzipperposition.Const.section

let phases = Phases_impl.main_cli ~setup_gc:true ()

let () =
  begin match Phases.run phases with
    | CCResult.Error msg ->
      print_endline msg;
      exit 1
    | CCResult.Ok (_, 0) -> ()
    | CCResult.Ok (_, errcode) -> exit errcode (* failure *)
  end

let _ =
  at_exit
    (fun () ->
       Util.debugf ~section 1 "run time: %.3f" (fun k->k (Util.total_time_s ()));
       Signal.send Libzipperposition.Signals.on_exit 0)
