
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Main file for the prover} *)

open Logtk
open Logtk_parsers
open Params

module T = FOTerm
module PF = PFormula
module O = Ordering
module Lit = Literal
module S = Substs

(* load some other modules, but they might not be registered *)
module Import = struct
  open! Chaining
  open! ArithInt
  open! EnumTypes
  open! Avatar
  open! Induction
  open! Zipperposition_quantor  (* TODO: remove, should be a plugin *)
  open! Zipperposition_depqbf  (* TODO remove *)
end

let section = Const.section

(** setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler s =
    begin
      Printf.printf "%% SZS Status ResourceOut\n";
      Unix.kill (Unix.getpid ()) Sys.sigterm
    end
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

let print_version ~params =
  if params.param_version
    then (Printf.printf "%% zipperposition v%s\n" Const.version; exit 0)

(* load default extensions *)
let () =
  Extensions.register Superposition.extension;
  Extensions.register AC.extension;
  Extensions.register Heuristics.extension;
  ()

let setup_penv ~penv () =
  Extensions.extensions ()
    |> List.iter (Extensions.apply_penv ~penv);
  if (PEnv.get_params ~penv).param_expand_def then
    PEnv.add_operation ~penv ~prio:1 PEnv.expand_def;
  (* use "invfreq" *)
  PEnv.add_constr_rule ~penv 50
    (fun set ->
      let symbols = PF.Set.to_seq set
        |> Sequence.map PF.form
        |> Sequence.flatMap Formula.FO.Seq.symbols
      in
      Precedence.Constr.invfreq symbols
    );
  (* be sure to get a total order on symbols, as last resort *)
  PEnv.add_constr ~penv 99 Precedence.Constr.alpha;
  ()

let setup_env ~env =
  Extensions.extensions ()
    |> List.iter (Extensions.apply_env ~env);
  ()

(** Load plugins (all "libzipperposition*.cmxs" in same path as main) *)
let load_plugins ~params =
  let dir = Filename.dirname Sys.argv.(0) in
  Gen.append (CCIO.File.read_dir dir) (CCIO.File.read_dir Const.home)
    |> Gen.filter
      (fun s -> CCString.prefix ~pre:"libzipperposition_" s
       && CCString.suffix ~suf:".cmxs" s
      )
    |> Gen.iter
      (fun filename ->
        Util.debug ~section 1 "trying to load file %s..." filename;
        match Extensions.dyn_load filename with
        | `Error msg -> () (* Could not load plugin *)
        | `Ok ext ->
          Util.debug ~section 0 "loaded extension %s" ext.Extensions.name;
          ()
      );
  Extensions.extensions ()

module MakeNew(X : sig
  module Env : Env.S
  val params : Params.t
end) = struct
  module Ctx = X.Env.Ctx
  module Env = X.Env
  module C = Env.C
  module Sat = Saturate.Make(Env)

  let params = X.params

  let has_conjecture = ref false

  (** print stats *)
  let print_stats () =
    Signal.send Signals.on_print_stats ();
    let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
      Util.debug ~section 1 ("hashcons stats for %s: size %d, num %d, sum length %d, "
                  ^^ "buckets: small %d, median %d, big %d")
        what sz num sum_length small median big
    and print_state_stats (num_active, num_passive, num_simpl) =
      Util.debug ~section 1 "proof state stats:";
      Util.debug ~section 1 "stat:  active clauses          %d" num_active;
      Util.debug ~section 1 "stat:  passive clauses         %d" num_passive;
      Util.debug ~section 1 "stat:  simplification clauses  %d" num_simpl
    and print_gc () =
      let stats = Gc.stat () in
      Util.debug ~section 1 ("GC: minor words %.0f; major_words: %.0f; max_heap: %d; "
                  ^^ "minor collections %d; major collections %d")
        stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
        stats.Gc.minor_collections stats.Gc.major_collections
    in
    print_gc ();
    print_hashcons_stats "terms" (ScopedTerm.hashcons_stats ());
    print_hashcons_stats "clauses" (C.CHashcons.stats ());
    print_state_stats (Env.stats ());
    if Util.Section.cur_level section > 0
      then Util.print_global_stats ()
      else ();
    ()

  let print_json_stats ~env =
    let encode_hashcons (x1,x2,x3,x4,x5,x6) =
      Util.sprintf "[%d, %d, %d, %d, %d, %d]" x1 x2 x3 x4 x5 x6 in
    (*
    let theories = match (Env.get_meta ~env) with
      | None -> "[]"
      | Some meta ->
        let seq = MetaProverState.theories meta in
        Util.sprintf "[%a]" (Util.pp_seq MetaProverState.pp_theory) seq
    in
    *)
    let o = Util.sprintf
      "{ \"terms\": %s, \"clauses\": %s }"
      (encode_hashcons (ScopedTerm.hashcons_stats ()))
      (encode_hashcons (C.CHashcons.stats ()))
    in
    Util.debug ~section 1 "json_stats: %s" o

  (* TODO
  (** Make an optional meta-prover and parse its KB *)
  let mk_meta ~params =
    if params.param_theories then begin
      (* create meta *)
      let meta = MetaProverState.create () in
      (* handle KB *)
      begin match MetaProverState.parse_kb_file meta params.param_kb with
      | Monad.Err.Ok () -> ()
      | Monad.Err.Error msg -> Util.debug ~section 0 "error: %s" msg
      end;
      (* read some theory files *)
      List.iter
        (fun file ->
          match MetaProverState.parse_theory_file meta file with
          | Monad.Err.Ok () -> ()
          | Monad.Err.Error msg -> Util.debug ~section 0 "%s" msg)
        params.param_kb_load;
      Some meta
    end else
      None
  *)

  (* pre-saturation *)
  let presaturate_clauses clauses =
    Util.debug ~section 1 "presaturate initial clauses";
    Env.add_passive clauses;
    let result, num = Sat.presaturate () in
    Util.debug ~section 1 "initial presaturation in %d steps" num;
    (* pre-saturated set of clauses *)
    let clauses = Env.get_active () in
    (* remove clauses from [env] *)
    Env.remove_active clauses;
    Env.remove_passive clauses;
    result, clauses

  (** Print some content of the state, based on environment variables *)
  let print_dots result =
    Signal.send Signals.on_dot_output ();
    (* see if we need to print proof state *)
    begin match params.param_dot_file, result with
    | Some dot_f, Saturate.Unsat proof ->
      let name = "unsat_graph" in
      (* print proof of false *)
        let proof =
          if X.params.param_dot_all_roots
          then
            Env.(Sequence.append (get_active()) (get_passive()))
            |> Sequence.filter_map
              (fun c -> if Literals.is_absurd (C.lits c) then Some (C.proof c) else None)
          else Sequence.singleton proof
        in
        Proof.pp_dot_seq_file ~name dot_f proof
    | Some dot_f, (Saturate.Sat | Saturate.Unknown) when params.param_dot_sat ->
      (* print saturated set *)
      let name = "sat_set" in
      let seq = Sequence.append (Env.get_active ()) (Env.get_passive ()) in
      let seq = Sequence.map C.proof seq in
      Proof.pp_dot_seq_file ~name dot_f seq
    | _ -> ()
    end;
    ()

  (*
  let print_meta ~env =
    (* print theories *)
    match Env.get_meta ~env with
    | Some meta ->
      Util.debug ~section 1 "meta-prover results (%d): %a"
        (Sequence.length (MetaProverState.results meta))
        (Util.pp_seq MetaProverState.pp_result) (MetaProverState.results meta);
      Util.debug ~section 1 "datalog contains %d clauses"
        (MetaReasoner.size (MetaProverState.reasoner meta))
    | None -> ()
  *)

  let _sat () =
    if !has_conjecture then "CounterSatisfiable" else "Satisfiable"
  let _unsat () =
    if !has_conjecture then "Theorem" else "Unsatisfiable"

  let print_szs_result ~file result =
    match result with
    | Saturate.Unknown
    | Saturate.Timeout ->
      Printf.printf "%% SZS status ResourceOut for '%s'\n" file
    | Saturate.Error s ->
      Printf.printf "%% SZS status InternalError for '%s'\n" file;
      Util.debug ~section 1 "error is: %s" s
    | Saturate.Sat when Ctx.is_completeness_preserved () ->
      Printf.printf "%% SZS status %s for '%s'\n" (_sat ()) file
    | Saturate.Sat ->
      Printf.printf "%% SZS status GaveUp for '%s'\n" file;
      begin match params.param_proof with
        | "none" -> ()
        | "tstp" ->
          Util.debug ~section 1 "saturated set:\n  %a\n"
            (Util.pp_seq ~sep:"\n  " C.pp_tstp_full) (Env.get_active ())
        | "debug" ->
          Util.debug ~section 1 "saturated set:\n  %a\n"
            (Util.pp_seq ~sep:"\n  " C.pp) (Env.get_active ())
        | n -> failwith ("unknown proof format: " ^ n)
      end
    | Saturate.Unsat proof ->
      (* print status then proof *)
      Printf.printf "%% SZS status %s for '%s'\n" (_unsat ()) file;
      Util.printf "%% SZS output start Refutation\n";
      Util.printf "%a" (Proof.pp params.param_proof) proof;
      Printf.printf "%% SZS output end Refutation\n";
      ()

  (* perform type inference on those formulas *)
  let annotate_formulas ~signature formulas =
    let tyctx = TypeInference.Ctx.create signature in
    let formulas = Sequence.map
      (fun (f, file,name) ->
        let f = TypeInference.FO.convert_form ~ctx:tyctx f in
        f, file, name)
      formulas
    in
    (* caution: evaluate formulas BEFORE *)
    let formulas = Sequence.persistent formulas in
    let signature = TypeInference.Ctx.to_signature tyctx in
    formulas, signature

  let setup_handlers() =
    Signal.on Ctx.on_signature_update
      (fun signature ->
        Ctx.update_prec (Signature.Seq.symbols signature);
        Signal.ContinueListening);
    ()

  (* try to refute the set of clauses contained in the [env]. Parameters are
     used to influence how saturation is done, for how long it runs, etc.
     @return the result and final env. *)
  let try_to_refute ~env ~params result =
    let steps = if params.param_steps = 0
      then None else (Util.debug ~section 0 "run for %d steps" params.param_steps;
                      Some params.param_steps)
    and timeout = if params.param_timeout = 0.
      then None else (Util.debug ~section 0 "run for %f s" params.param_timeout;
                      ignore (setup_alarm params.param_timeout);
                      Some (Util.get_start_time () +. params.param_timeout -. 0.25))
    in
    let result, num = match result with
      | Saturate.Unsat _ -> result, 0  (* already found unsat during presaturation *)
      | _ -> Sat.given_clause ~generating:true ?steps ?timeout ()
    in
    Util.debug ~section 1 "done %d iterations" num;
    Util.debug ~section 1 "final precedence: %a" Precedence.pp (Env.precedence ());
    result, env
end

(* print weight of [s] within precedence [prec] *)
let _pp_weight prec buf s =
  Printf.bprintf buf "w(%a)=%d" Symbol.pp s (Precedence.weight prec s)

(* preprocess formulas and choose signature,select,ord *)
let preprocess ~signature ~params formulas =
  Util.debug ~section 2 "start preprocessing";
  (* penv *)
  let penv = PEnv.create ~base:signature params in
  setup_penv ~penv ();
  let formulas = PEnv.process ~penv formulas in
  Util.debug ~section 3 "formulas pre-processed into:\n  %a"
    (Util.pp_seq ~sep:"\n  " PF.pp) (PF.Set.to_seq formulas);
  (* now build a context *)
  let precedence, constr_list = PEnv.mk_precedence ~penv formulas in
  Util.debug ~section 1 "precedence: %a" Precedence.pp precedence;
  Util.debug ~section 1 "weights: %a"
    (Sequence.pp_buf (_pp_weight precedence))
    (Signature.Seq.symbols signature);
  let ord = params.param_ord precedence in
  let select = Selection.selection_from_string ~ord params.param_select in
  Util.debug ~section 1 "selection function: %s" params.param_select;
  Util.debug ~section 1 "signature: %a" Signature.pp (Signature.diff signature !Params.signature);
  let module Result = struct
    let signature = signature
    let select = select
    let ord = ord
    let constr_list = constr_list
  end in
  (module Result : Ctx.PARAMETERS), formulas

(* does the sequence of declarations contain at least one conjecture? *)
let _has_conjecture decls =
  let _roles k =
    let visitor = object
      inherit [unit] Ast_tptp.Untyped.visitor
      method clause () role _ = k role
      method any_form () role _ = k role
      end
    in
    decls (visitor#visit ())
  in
  Sequence.exists (fun r -> r = Ast_tptp.R_conjecture) _roles

(** Process the given file (try to solve it) *)
let process_file ?meta ~plugins ~params file =
  let open CCError in
  Util.debug ~section 1 "================ process file %s ===========" file;
  (* parse formulas *)
  Util_tptp.parse_file ~recursive:true file
  >>= fun decls ->
  let has_conjecture = _has_conjecture decls in
  Util.debug ~section 1 "parsed %d declarations (%sconjecture)"
    (Sequence.length decls) (if has_conjecture then "" else "no ");
  (* obtain a typed AST *)
  Util_tptp.infer_types (`sign !Params.signature) decls
  >>= fun (signature,decls) ->
  (* obtain a set of proved formulas *)
  let formulas =
    Util_tptp.Typed.sourced_formulas ~file decls
    |> Sequence.map PF.of_sourced
    |> PF.Set.of_seq
  in
  (* obtain clauses + env *)
  Util.debug ~section 2 "input formulas:\n%%  %a" (Util.pp_seq ~sep:"\n%  " PF.pp)
    (PF.Set.to_seq formulas);
    Util.debug ~section 2 "input signature: %a"
      Signature.pp (Signature.diff signature !Params.signature);
  let res, signature = preprocess ~signature ~params formulas in
  let module Res = (val res : Ctx.PARAMETERS) in
  (* build the context and env *)
  let module Ctx = Ctx.Make(Res) in
  let module MyEnv = Env.Make(struct
    module Ctx = Ctx
    let params = params
  end) in
  let env = (module MyEnv : Env.S) in
  setup_env ~env;
  (* reduce to CNF *)
  Util.debug ~section 1 "reduce to CNF...";
  let clauses = MyEnv.cnf formulas in
  Util.debug ~section 3 "CNF:\n  %a"
    (Util.pp_seq ~sep:"\n  " MyEnv.C.pp) (MyEnv.C.CSet.to_seq clauses);
  (* main workload *)
  let module Main = MakeNew(struct
    module Env = MyEnv
    let params = params
  end) in
  Main.has_conjecture := has_conjecture;
  Main.setup_handlers();
  (* pre-saturation *)
  let num_clauses = MyEnv.C.CSet.size clauses in
  let result, clauses = if params.param_presaturate
    then Main.presaturate_clauses (MyEnv.C.CSet.to_seq clauses)
    else Saturate.Unknown, MyEnv.C.CSet.to_seq clauses
  in
  Util.debug ~section 1 "signature: %a" Signature.pp
    (Signature.diff (MyEnv.signature ()) !Params.signature);
  Util.debug ~section 2 "%d clauses processed into:\n%%  %a"
    num_clauses (Util.pp_seq ~sep:"\n%  " MyEnv.C.pp) clauses;
  (* add clauses to passive set of [env] *)
  MyEnv.add_passive clauses;
  (* saturate, possibly changing env *)
  let result, env = Main.try_to_refute ~env ~params result in
  Util.debug ~section 1 "=================================================";
  (* print some statistics *)
  if params.param_stats then begin
    Main.print_stats ();
    Main.print_json_stats ();
    end;
  Main.print_dots result;
  Main.print_szs_result ~file result;
  Util.debug ~section 1 "=================================================";
  return ()

let () =
  Util.debug ~section 0 "setup GC and signal handler";
  (* GC! increase max overhead because we want the GC to be faster, even if
      it implies more wasted memory. *)
  let gc = Gc.get () in
  Gc.set { gc with Gc.space_overhead=150; };
  (* signal handler. Re-raise, bugs shouldn't keep hidden *)
  Signal.set_exn_handler (fun e ->
    let msg = Printexc.to_string e in
    output_string stderr ("exception raised in signal: " ^ msg ^ "\n");
    flush stderr;
    raise e);
  ()

let () =
  (* parse arguments *)
  let params = Params.parse_args () in
  Util.debug ~section 2 "extensions loaded: %a"
    (Util.pp_list Buffer.add_string) (Extensions.names ());
  Random.init params.param_seed;
  print_version params;
  (* plugins *)
  let plugins = load_plugins ~params in
  (* initialize plugins *)
  List.iter Extensions.init plugins;
  (* master process: process files *)
  CCVector.iter
    (fun file ->
      match process_file ~plugins ~params file with
      | `Error msg ->
          print_endline msg;
          exit 1
      | `Ok () -> ()
    ) params.param_files;
  ()

let _ =
  at_exit (fun () ->
    Util.debug ~section 1 "run time: %.3f" (Util.get_total_time ());
    Signal.send Signals.on_exit 0)
