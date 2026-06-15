(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover} *)

open Logtk
open Libzipperposition

type filename = string
type 'a or_error = ('a, exn * Printexc.raw_backtrace) CCResult.t

(** {2 Phases} *)

type env_with_clauses =
  | Env_clauses : 'c Env.packed * 'c Clause.sets -> env_with_clauses

type env_with_result =
  | Env_result : 'c Env.packed * Saturate.szs_status -> env_with_result

type errcode = int
type prelude = UntypedAST.statement Iter.t

type phase =
  | Init
  | Setup_gc
  | Setup_signal
  | Parse_CLI
  | LoadExtensions
  | Parse_prelude
  | Start_file
  | Parse_file
  | Typing
  | CNF
  | Compute_prec
  | Compute_ord_select
  | MakeCtx
  | MakeEnv
  | Pre_saturate
  | Saturate
  | Print_result
  | Print_dot
  | Check_proof
  | Print_stats
  | Exit

module State = Flex_state

module Key = struct
  let cur_phase = State.create_key ()
end

let empty_state = State.empty |> State.add Key.cur_phase Init

(* Direct style: 'a t = State.t ref -> 'a *)
type 'a t = State.t ref -> 'a

let show_phase = function
  | Init -> "init"
  | Setup_gc -> "setup_gc"
  | Setup_signal -> "setup_signal"
  | Parse_CLI -> "parse_cli"
  | LoadExtensions -> "load_extensions"
  | Parse_prelude -> "parse_prelude"
  | Start_file -> "start_file"
  | Parse_file -> "parse_file"
  | Typing -> "typing"
  | CNF -> "cnf"
  | Compute_prec -> "compute_prec"
  | Compute_ord_select -> "compute_ord_select"
  | MakeCtx -> "make_ctx"
  | MakeEnv -> "make_env"
  | Pre_saturate -> "pre_saturate"
  | Saturate -> "saturate"
  | Print_result -> "print_result"
  | Print_dot -> "print_dot"
  | Check_proof -> "check_proof"
  | Print_stats -> "print_stats"
  | Exit -> "exit"

let failwith msg _st_ref = failwith msg

let with_span ~__FILE__ ~__LINE__ name (f : Trace.span -> 'a) : 'a =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ name in
  f _sp

let with_phase (st_ref : State.t ref) p (f : unit -> 'a) : 'a =
  let p_name = show_phase p in
  Util.debugf ~section:Const.section 2 "@{<yellow>start phase@} %s" (fun k ->
      k p_name);
  st_ref := State.add Key.cur_phase p !st_ref;
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "phases.phase" in
  Trace.add_data_to_span _sp [ "p", `String p_name ];
  let y = f () in
  Util.debugf ~section:Const.section 2 "@{<yellow>terminate phase@} %s"
    (fun k -> k p_name);
  y

let exit (st_ref : State.t ref) : unit = with_phase st_ref Exit (fun () -> ())

let get_key (k : 'a State.key) (st_ref : State.t ref) : 'a =
  State.get_exn k !st_ref

let set_key (k : 'a State.key) (v : 'a) (st_ref : State.t ref) : unit =
  st_ref := State.add k v !st_ref

let update ~(f : State.t -> State.t) (st_ref : State.t ref) : unit =
  st_ref := f !st_ref

let run_and_discard_l (l : int t list) (st_ref : State.t ref) : int =
  let saved = !st_ref in
  let rec aux = function
    | [] -> 0
    | [ a ] -> a st_ref
    | a :: tail ->
      let n = a st_ref in
      if n <> 0 then
        n
      else (
        st_ref := saved;
        aux tail
      )
  in
  aux l

let run_with (st : State.t) (m : 'a t) : (State.t * 'a) or_error =
  let st_ref = ref st in
  try
    let x = m st_ref in
    Ok (!st_ref, x)
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Error (e, bt)

let run (m : 'a t) : _ or_error =
  run_with (State.empty |> State.add Key.cur_phase Init) m
