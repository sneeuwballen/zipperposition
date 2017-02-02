
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Superposition + Splitting} *)

open Libzipperposition
open Libzipperposition_parsers
module E = CCResult
open E.Infix

let section = Util.Section.(make ~parent:root "hornet")

let start_ k = Util.debugf ~section 1 k

let parse_file file =
  Util.debugf ~section 1 "@[@{<Yellow>### process file@ `%s` ###@}@]"
    (fun k->k file);
  Parsing_utils.parse file

type conf = Flex_state.t

let k_def_as_rewrite : bool Flex_state.key = Flex_state.create_key()

let typing conf stmts =
  start_ "start typing" (fun k->k);
  let def_as_rewrite =
    Flex_state.get_or ~or_:false k_def_as_rewrite conf
  in
  TypeInference.infer_statements ~def_as_rewrite ?ctx:None stmts

(* obtain clauses  *)
let cnf ~file decls =
  let stmts =
    decls
    |> CCVector.to_seq
    |> Sequence.map (Statement.add_src ~file)
    |> Cnf.cnf_of_seq
    |> CCVector.to_seq
    |> Cnf.convert
  in
  E.return stmts

(* TODO: make defined symbols smaller, skolems bigger *)

(* compute a precedence *)
let compute_prec stmts =
  let cp =
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
  E.return prec

let compute_ord precedence =
  let ord = Ordering.by_name "kbo" precedence in
  E.return ord

(* setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler _ =
    Format.printf "%% SZS Status ResourceOut@.";
    Unix.kill (Unix.getpid ()) Sys.sigterm
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  ignore (Unix.alarm (max 1 (int_of_float timeout)))

let setup_gc () =
  let gc = Gc.get () in
  Gc.set { gc with Gc.space_overhead=150; }

(** {2 Main} *)

let time : float ref = ref 0.
let file : string ref = ref ""
let def_as_rewrite : bool ref = ref false

let options =
  Arg.align
    ([ "--time", Arg.Set_float time, " set timeout";
       "-t", Arg.Set_float time, " alias to --time";
       "--def-as-rewrite", Arg.Set def_as_rewrite, " definitions as rewrite rules";
       "--no-def-as-rewrite", Arg.Clear def_as_rewrite, " definitions as axioms";
    ] @ Options.make ())

let main () =
  Arg.parse options (fun s->file := s) "usage: hornet <file>";
  setup_alarm !time;
  setup_gc ();
  let conf =
    Flex_state.empty
    |> Flex_state.add k_def_as_rewrite !def_as_rewrite
  in
  parse_file !file >>=
  typing conf >>=
  cnf ~file:!file >>= fun stmts ->
  compute_prec (CCVector.to_seq stmts) >>=
  compute_ord >>= fun ord ->
  (* TODO *)
  E.return ()

let () =
  at_exit
    (fun () ->
       Util.debugf ~section 1 "run time: %.3f" (fun k->k (Util.total_time_s ())));
  begin match main () with
    | E.Error msg ->
      print_endline msg;
      exit 1
    | E.Ok () -> ()
  end
