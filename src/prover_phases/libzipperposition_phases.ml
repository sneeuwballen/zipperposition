
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module Phases = Phases
module Phases_impl = Phases_impl
open Phases.Infix
open Libzipperposition

(* List the extensions as late in the compilation chain as possible. This lets them access as much functionality as possible without a cyclic dependency. That is here where the command line interface is defined. *)
let load_extensions =
  let open Libzipperposition_calculi in
  Phases.start_phase Phases.LoadExtensions >>= fun () ->
  let _= List.map Extensions.register [
    Lazy_cnf.extension;
    Combinators.extension;
    Higher_order.extension;
    Superposition.extension;
    Bce_pe_fixpoint.extension;
    Bce.extension;
    Pred_elim.extension;
    Hlt_elim.extension;
    AC.extension;
    Heuristics.extension;
    Libzipperposition_avatar.extension;
    EnumTypes.extension;
    Libzipperposition_induction.extension;
    Rewriting.extension;
    Libzipperposition_arith.int_ext;
    Libzipperposition_arith.rat_ext;
    Ind_types.extension;
    Fool.extension;
    Booleans.extension;
    Lift_lambdas.extension;
    Summation_equality.extension;
    Pure_literal_elim.extension;
    Bool_encode.extension;
    App_encode.extension;
    Eq_encode.extension;
    Pure_literal_elim.extension;
  ] in
  Phases.return_phase (Extensions.extensions ())


let main_cli ?setup_gc:(gc=true) () =
  Phases.run(
    (if gc then Phases_impl.setup_gc else Phases.return ()) >>= fun () ->
    Phases_impl.setup_signal >>= fun () ->
    Phases_impl.parse_cli >>= fun (files, params) ->
    load_extensions >>= fun _ ->
    Phases_impl.process_files_and_print ~params files >>= fun errcode ->
    Phases.exit >|= fun () ->
    errcode
  ) |> CCResult.map snd

let main ?setup_gc:(gc=true) ?params file =
  Phases.run(
    (if gc then Phases_impl.setup_gc else Phases.return ()) >>= fun () ->
    (* pseudo-parse *)
    Phases_impl.skip_parse_cli ?params file >>= fun (files, params) ->
    load_extensions >>= fun _ ->
    Phases_impl.process_files_and_print ~params files >>= fun errcode ->
    Phases.exit >|= fun () ->
    errcode
  ) |> CCResult.map snd