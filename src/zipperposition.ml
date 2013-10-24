
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
open Logtk_meta
open Params

module T = FOTerm
module PF = PFormula
module O = Ordering
module C = Clause
module Lit = Literal
module PS = ProofState
module CQ = ClauseQueue
module S = Substs
module Sup = Superposition
module Sat = Saturate
module Sel = Selection

(* TODO: if real arith detected, print warning and exit *)
(* TODO: params to limit depth of preprocessing *)
(* TODO: params to enable/disable some preprocessing *)

(** print stats *)
let print_stats ~env =
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Util.debug 1 ("hashcons stats for %s: size %d, num %d, sum length %d, "
                ^^ "buckets: small %d, median %d, big %d")
      what sz num sum_length small median big
  and print_state_stats (num_active, num_passive, num_simpl) =
    Util.debug 1 "proof state stats:";
    Util.debug 1 "stat:  active clauses          %d" num_active;
    Util.debug 1 "stat:  passive clauses         %d" num_passive;
    Util.debug 1 "stat:  simplification clauses  %d" num_simpl
  and print_gc () =
    let stats = Gc.stat () in
    Util.debug 1 ("GC: minor words %.0f; major_words: %.0f; max_heap: %d; "
                ^^ "minor collections %d; major collections %d")
      stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
      stats.Gc.minor_collections stats.Gc.major_collections
  in
  print_gc ();
  print_hashcons_stats "terms" (T.H.stats ());
  print_hashcons_stats "clauses" (C.CHashcons.stats ());
  print_state_stats (Env.stats ~env);
  if Util.get_debug () > 0
    then Util.print_global_stats ()
    else ();
  ()

let print_json_stats ~env =
  let open Sequence.Infix in
  let encode_hashcons (x1,x2,x3,x4,x5,x6) =
    Util.sprintf "[%d, %d, %d, %d, %d, %d]" x1 x2 x3 x4 x5 x6 in
  let theories = match (Env.get_meta ~env) with
    | None -> "[]"
    | Some meta ->
      let seq = MetaProverState.theories meta in
      Util.sprintf "[%a]" (Util.pp_seq MetaProverState.pp_theory) seq
  in
  let experts = Experts.Set.size (Env.get_experts ~env) in
  let o = Util.sprintf
    "{ \"terms\": %s, \"clauses\": %s, \"theories\": %s, \"experts\":%d }"
    (encode_hashcons (T.H.stats ()))
    (encode_hashcons (C.CHashcons.stats ())) theories experts
  in
  Util.debug 1 "json_stats: %s" o

(** print the final state to given file in DOT, with
    clauses in result if needed *)
let print_state ?name filename (state, result) =
  match result with
  | Sat.Unsat c -> Proof.pp_dot_file ?name filename c.C.hcproof
  | _ -> Util.debug 1 "no empty clause; do not print state"

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

let setup_penv ?(ctx=Skolem.create ()) ~penv () =
  begin match (PEnv.get_params ~penv).param_calculus with
  | "superposition" -> Superposition.setup_penv ~ctx ~penv
  | x -> failwith ("unknown calculus " ^ x)
  end;
  if (PEnv.get_params ~penv).param_expand_def then
    PEnv.add_operation ~penv ~prio:1 PEnv.expand_def;
  if (PEnv.get_params ~penv).param_arith then
    ArithElim.setup_penv ~penv;  (* TODO chaining! *)
  AC.setup_penv ~penv;
  Chaining.setup_penv ~penv;
  (* be sure to get a total order on symbols *)
  PEnv.add_constr ~penv Precedence.alpha_constraint;
  ()

let setup_env ~env =
  begin match (Env.get_params ~env).param_calculus with
  | "superposition" -> Superposition.setup_env ~env
  | x -> failwith ("unknown calculus " ^ x)
  end;
  AC.setup_env ~env;
  Chaining.setup_env ~env;
  if (Env.get_params ~env).param_arith then begin
    let ac = (Env.get_params ~env).param_arith_ac in
    T.set_default_pp T.pp_arith;
    FOFormula.set_default_pp FOFormula.pp_arith;
    Lit.set_default_pp Lit.pp_arith;
    ArithElim.setup_env ~ac ~env;
    end;
  ()

(** Make an optional meta-prover and parse its KB *)
let mk_meta ~params =
  if params.param_theories then begin
    (* create meta *)
    let meta = MetaProverState.create () in
    (* handle KB *)
    MetaProverState.parse_kb_file meta params.param_kb;
    (* read some theory files *)
    List.iter
      (fun file -> MetaProverState.parse_theory_file meta file)
      params.param_kb_load;
    Some meta
  end else
    None

(** Load plugins *)
let load_plugins ~params =
  Util.list_flatmap
    (fun filename ->
      let n = String.length filename in
      let filename =  (* plugin name, or file? *)
        if n > 4 && String.sub filename (n-5) 5 = ".cmxs"
          then filename
        else
          let local_filename = Util.sprintf "plugins/std/ext_%s.cmxs" filename in
          try
            ignore (Unix.stat local_filename);
            local_filename
          with Unix.Unix_error _ ->
            let home_filename = Util.sprintf "plugins/ext_%s.cmxs" filename in
            Filename.concat Const.home home_filename
      in
      match Extensions.dyn_load filename with
      | Extensions.Ext_failure msg -> (* Could not load plugin *)
        []
      | Extensions.Ext_success ext ->
        Util.debug 0 "loaded extension %s" ext.Extensions.name;
        [ext])
    params.param_plugins

(* build initial env and clauses *)
let preprocess ?meta ~plugins ~params formulas =
  (* penv *)
  let penv = PEnv.create ?meta params in
  setup_penv ~penv ();
  let formulas = PEnv.process ~penv formulas in
  Util.debug 3 "formulas pre-processed into:\n  %a"
    (Util.pp_seq ~sep:"\n  " PF.pp) (PF.Set.to_seq formulas);
  (* now build a context *)
  let precedence = PEnv.mk_precedence ~penv formulas in
  Util.debug 1 "precedence: %a" Precedence.pp precedence;
  let ord = params.param_ord precedence in
  let select = Sel.selection_from_string ~ord params.param_select in
  Util.debug 1 "selection function: %s" params.param_select;
  let signature = PF.Set.signature ~signature:(PEnv.signature ~penv) formulas in
  let ctx = Ctx.create ~ord ~select ~signature () in
  (* build the env *)
  let env = Env.create ?meta ~ctx params signature in
  setup_env ~env;
  (* reduce to CNF *)
  Util.debug 1 "reduce to CNF...";
  let clauses = Env.cnf ~env formulas in
  Util.debug 3 "CNF:\n  %a" (Util.pp_seq ~sep:"\n  " C.pp) (C.CSet.to_seq clauses);
  (* return env + clauses *)
  env, clauses

(* pre-saturation *)
let presaturate_clauses ~env clauses =
  Util.debug 1 "presaturate initial clauses";
  Env.add_passive ~env clauses;
  let result, num = Sat.presaturate ~env in
  Util.debug 1 "initial presaturation in %d steps" num;
  (* pre-saturated set of clauses *)
  let clauses = Sequence.persistent (Env.get_active ~env) in
  (* remove clauses from [env] *)
  Env.remove_active ~env clauses;
  Env.remove_passive ~env clauses;
  result, clauses

(** Print some content of the state, based on environment variables *)
let print_dots ~env result =
  let params = Env.get_params ~env in
  (* see if we need to print proof state *)
  begin match params.param_dot_file, result with
  | Some dot_f, Sat.Unsat c ->
    let name = "unsat_graph" in
    (* print proof of false *)
    Proof.pp_dot_file ~name dot_f c.C.hcproof
  | Some dot_f, (Sat.Sat | Sat.Unknown) when params.param_dot_sat ->
    (* print saturated set *)
    let name = "sat_set" in
    let seq = Sequence.append (Env.get_active ~env) (Env.get_passive ~env) in
    let seq = Sequence.map C.get_proof seq in
    Proof.pp_dot_seq_file ~name dot_f seq
  | _ -> ()
  end;
  ()

let print_meta ~env =
  (* print theories *)
  match Env.get_meta ~env with
  | Some meta ->
    Util.debug 1 "meta-prover results (%d): %a"
      (Sequence.length (MetaProverState.results meta))
      (Util.pp_seq MetaProverState.pp_result) (MetaProverState.results meta);
    Util.debug 1 "datalog contains %d clauses"
      (MetaReasoner.size (MetaProverState.reasoner meta))
  | None -> ()

let print_szs_result ~file ~env result =
  let params = Env.get_params ~env in
  match result with
  | Sat.Unknown
  | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut for '%s'\n" file
  | Sat.Error s ->
    Printf.printf "%% SZS status InternalError for '%s'\n" file;
    Util.debug 1 "error is: %s" s
  | Sat.Sat ->
    if Ctx.is_completeness_preserved (Env.ctx env)
      then Printf.printf "%% SZS status CounterSatisfiable for '%s'\n" file
      else Printf.printf "%% SZS status GaveUp for '%s'\n" file;
    begin match params.param_proof with
      | "none" -> ()
      | "tstp" ->
        Util.debug 1 "saturated set:\n  %a\n"
          (Util.pp_seq ~sep:"\n  " C.pp_tstp_full) (Env.get_active ~env)
      | "debug" ->
        Util.debug 1 "saturated set:\n  %a\n"
          (Util.pp_seq ~sep:"\n  " C.pp) (Env.get_active ~env)
      | n -> failwith ("unknown proof format: " ^ n)
    end
  | Sat.Unsat c ->
    (* print status then proof *)
    Printf.printf "%% SZS status Theorem for '%s'\n" file;
    Util.printf "%% SZS output start Refutation\n";
    Util.printf "%a" (Proof.pp params.param_proof) c.C.hcproof;
    Printf.printf "%% SZS output end Refutation\n";
    ()

(** Process the given file (try to solve it) *)
let process_file ?meta ~plugins ~params file =
  Util.debug 1 "================ process file %s ===========" file;
  let steps = if params.param_steps = 0
    then None else (Util.debug 0 "run for %d steps" params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Util.debug 0 "run for %f s" params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Util.get_start_time () +. params.param_timeout -. 0.25))
  in
  (* parse formulas *)
  let decls = Util_tptp.parse_file ~recursive:true file in
  Util.debug 1 "parsed %d declarations" (Sequence.length decls);
  (* obtain a set of proved formulas *)
  let formulas = Util_tptp.sourced_formulas ~file decls in
  let formulas = Sequence.map PF.of_sourced formulas in
  let formulas = PF.Set.of_seq formulas in
  (* obtain clauses + env *)
  Util.debug 2 "input formulas:\n%%  %a" (Util.pp_seq ~sep:"\n%%  " PF.pp)
    (PF.Set.to_seq formulas);
  let env, clauses = preprocess ?meta ~plugins ~params formulas in
  (* pre-saturation *)
  let num_clauses = C.CSet.size clauses in
  let result, clauses = if params.param_presaturate
    then presaturate_clauses ~env (C.CSet.to_seq clauses)
    else Sat.Unknown, C.CSet.to_seq clauses
  in
  Util.debug 1 "signature: %a" Signature.pp_no_base (Env.signature env);
  Util.debug 2 "%d clauses processed into:\n%%  %a"
    num_clauses (Util.pp_seq ~sep:"\n%%  " C.pp) clauses;
  (* add clauses to passive set of [env] *)
  Env.add_passive ~env clauses;
  (* saturate *)
  let result, num = match result with
    | Sat.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ~env
  in
  Util.debug 1 "=================================================";
  Util.debug 1 "done %d iterations" num;
  Util.debug 1 "final precedence: %a" Precedence.pp (Env.precedence env);
  (* print some statistics *)
  if params.param_stats then begin
    print_stats ~env;
    print_json_stats ~env;
    Util.debug 1 "experts: %a" Experts.Set.pp (Env.get_experts ~env);
    end;
  print_dots ~env result;
  print_meta ~env;
  print_szs_result ~file ~env result;
  ()

(** Print the content of the KB, and exit *)
let print_kb ?meta =
  begin match meta with
  | None -> ()
  | Some meta ->
    let kb = MetaProverState.kb meta in
    Util.printf "%a\n" MetaKB.pp kb;
  end;
  exit 0

(** Clear the Knowledge Base and exit *)
let clear_kb params =
  let file = params.param_kb in
  Util.with_lock_file file
    (fun () ->  (* remove file *)
      Unix.unlink file);
  exit 0

let print_kb_where params = 
  Util.debug 0 "KB located at %s" params.param_kb;
  exit 0

let save_kb ?meta ~params =
  match meta with
  | None -> ()
  | Some meta ->
    let file = params.param_kb in
    Util.with_lock_file file
      (fun () -> MetaProverState.save_kb_file meta file)

let () =
  (* GC! increase max overhead because we want the GC to be faster, even if
      it implies more wasted memory. *)
  let gc = Gc.get () in
  Gc.set { gc with Gc.space_overhead=150; }

let () =
  (* parse arguments *)
  let params = Params.parse_args () in
  Random.init params.param_seed;
  print_version params;
  (* plugins *)
  let plugins = load_plugins ~params in
  (* meta *)
  let meta = mk_meta ~params in
  (* operations on knowledge base *)
  (if params.param_kb_where then print_kb_where params);
  (if params.param_kb_print then print_kb ?meta);
  (if params.param_kb_clear then clear_kb params);
  (* master process: process files *)
  Vector.iter params.param_files (process_file ?meta ~plugins ~params);
  (* save KB? *)
  save_kb ?meta ~params;
  ()

let _ =
  at_exit (fun () -> 
    Util.debug 1 "run time: %.3f" (Util.get_total_time ()))
