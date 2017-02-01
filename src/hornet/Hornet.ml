
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Superposition + Splitting} *)

open Libzipperposition
open Libzipperposition_parsers
open CCResult.Infix

(* setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler _ =
    Format.printf "%% SZS Status ResourceOut@.";
    Unix.kill (Unix.getpid ()) Sys.sigterm
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

let section = Util.Section.(make ~parent:root "hornet")

let start_ k = Util.debugf ~section 1 k

let parse_file file =
  Util.debugf ~section 1 "@[@{<Yellow>### process file@ `%s` ###@}@]"
    (fun k->k file);
  Parsing_utils.parse file

let typing stmts =
  start_ (fun k->k "start typing");
  Phases.get_key Params.key >>= fun params ->
  let def_as_rewrite = params.Params.param_def_as_rewrite in
  TypeInference.infer_statements ~def_as_rewrite ?ctx:None stmts >>?= fun stmts ->
  do_extensions ~field:(fun e -> e.Extensions.post_typing_actions)
    ~x:stmts >>= fun () ->
  Phases.return_phase stmts

(* obtain clauses  *)
let cnf ~file decls =
  Phases.start_phase Phases.CNF >>= fun () ->
  let stmts =
    decls
    |> CCVector.to_seq
    |> Sequence.map (Statement.add_src ~file)
    |> Cnf.cnf_of_seq
    |> CCVector.to_seq
    |> Cnf.convert
  in
  do_extensions ~field:(fun e -> e.Extensions.post_cnf_actions)
    ~x:stmts >>= fun () ->
  Phases.return_phase stmts

(* compute a precedence *)
let compute_prec stmts =
  Phases.start_phase Phases.Compute_prec >>= fun () ->
  (* use extensions *)
  Phases.get >>= fun state ->
  let cp =
    Extensions.extensions ()
    |> List.fold_left
      (fun cp e -> List.fold_left (fun cp f -> f state cp) cp e.Extensions.prec_actions)
      Compute_prec.empty

    (* add constraint about inductive constructors, etc. *)
    |> Compute_prec.add_constr 10 Classify_cst.prec_constr

    (* use "invfreq", with low priority *)
    |> Compute_prec.add_constr_rule 90
      (fun seq ->
         seq
         |> Sequence.flat_map Statement.Seq.terms
         |> Sequence.flat_map FOTerm.Seq.symbols
         |> Precedence.Constr.invfreq)
  in
  let prec = Compute_prec.mk_precedence cp stmts in
  Phases.return_phase prec

let compute_ord_select precedence =
  Phases.start_phase Phases.Compute_ord_select >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let ord = Ordering.by_name params.param_ord precedence in
  let select = Selection.selection_from_string ~ord params.param_select in
  do_extensions ~field:(fun e->e.Extensions.ord_select_actions)
    ~x:(ord,select) >>= fun () ->
  Util.debugf ~section 2 "@[<2>selection function:@ %s@]" (fun k->k params.param_select);
  Phases.return_phase (ord, select)

let () =
  match Phases.run phases with
    | `Error msg ->
      print_endline msg;
      exit 1
    | `Ok (_, ()) -> ()

let _ =
  at_exit
    (fun () ->
       Util.debugf ~section 1 "run time: %.3f" (fun k->k (Util.total_time_s ()));
       Signal.send Signals.on_exit 0)

