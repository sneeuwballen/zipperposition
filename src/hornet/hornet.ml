
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Superposition + Splitting} *)

open Libzipperposition
open Libzipperposition_parsers
module E = CCResult
open E.Infix
module Fmt = CCFormat

let section = Util.Section.(make ~parent:root "hornet")

let start_ k = Util.debugf ~section 1 k

let parse_file file =
  Util.debugf ~section 1 "@[@{<Yellow>### process file@ `%s` ###@}@]"
    (fun k->k file);
  Parsing_utils.parse file

type conf = Flex_state.t

let k_def_as_rewrite : bool Flex_state.key = Flex_state.create_key()

let typing conf stmts : (_ Statement.t CCVector.ro_vector * Type.t ID.Map.t, string) E.t =
  start_ "start typing" (fun k->k);
  let def_as_rewrite =
    Flex_state.get_or ~or_:false k_def_as_rewrite conf
  in
  TypeInference.infer_statements ~def_as_rewrite ?ctx:None stmts >|= fun stmts ->
  (* compute signature *)
  let signature =
    let conv = Type.Conv.create () in
    CCVector.to_seq stmts
    |> Sequence.flat_map Statement.Seq.ty_decls
    |> Sequence.map (CCPair.map2 (Type.Conv.of_simple_term_exn conv))
    |> ID.Map.of_seq
  in
  Util.debugf ~section 2 "@[<hv2>signature {@ %a@,}@]"
    (fun k->k
        Fmt.(seq (Dump.pair ID.pp Type.pp))
        (ID.Map.to_seq signature));
  Util.debugf ~section 2 "@[<hv2>typed statements {@ %a@,}@]"
    (fun k->
     let module T = TypedSTerm in
     k (CCVector.print ~sep:" " (Statement.pp T.pp T.pp T.pp)) stmts);
  stmts, signature

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
  Util.debugf ~section 2 "@[<hv2>CNF {@ %a@,}@]"
    (fun k-> k (CCVector.print ~sep:" " Cnf.pp_fo_c_statement) stmts);
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
  Util.debugf ~section 2 "@[<2>precedence: %a@]" (fun k->k Precedence.pp prec);
  E.return prec

let compute_ord precedence =
  let ord = Ordering.by_name "kbo" precedence in
  Util.debugf ~section 2 "@[<2>ord: %a@]" (fun k->k Ordering.pp ord);
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
let max_depth : int ref = ref 100

let options =
  Arg.align
    ([ "--time", Arg.Set_float time, " set timeout";
       "-t", Arg.Set_float time, " alias to --time";
       "--def-as-rewrite", Arg.Set def_as_rewrite, " definitions as rewrite rules";
       "--no-def-as-rewrite", Arg.Clear def_as_rewrite, " definitions as axioms";
       "--max-depth", Arg.Set_int max_depth, (Fmt.sprintf " maximum depth (default %d)" !max_depth);
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
  typing conf >>= fun (stmts, signature) ->
  cnf ~file:!file stmts >>= fun stmts ->
  compute_prec (CCVector.to_seq stmts) >>=
  compute_ord >>= fun ord ->
  (* convert statements in {!Clause.t} *)
  let stmts =
    CCVector.map
      (fun stmt ->
         Statement.map stmt ~form:(Clause.of_slit_l ~stmt) ~term:CCFun.id ~ty:CCFun.id)
      stmts
  in
  let st =
    let module A = struct
      let ord = ord
      let signature = signature
      let conf = conf
      let statements = stmts
      let max_depth = !max_depth
      let theories = [
        Horn_superposition.theory;
        Splitting.theory;
      ]
    end
    in
    State.create (module A)
  in
  let res = State.run st in
  Format.printf "%a@." State.pp_res res;
  E.return () (* done *)

let () =
  CCFormat.set_color_default true;
  at_exit
    (fun () ->
       Util.debugf ~section 1 "run time: %.3f" (fun k->k (Util.total_time_s ())));
  begin match main () with
    | E.Error msg ->
      print_endline msg;
      exit 1
    | E.Ok () -> ()
  end
