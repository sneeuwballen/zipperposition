(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover} *)

open Logtk
open Libzipperposition
module E = CCResult

type filename = string
type 'a or_error = ('a, string) CCResult.t

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

(* A simple state monad *)
type 'a t = State.t -> (State.t * 'a) or_error

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
  | Print_stats -> "print_stats"
  | Print_dot -> "print_dot"
  | Check_proof -> "check_proof"
  | Exit -> "exit"

let return x st = E.return (st, x)

let return_result x st =
  match x with
  | E.Ok x -> E.Ok (st, x)
  | E.Error msg -> E.Error msg

let fail msg _ = E.Error msg

let bind x ~f st =
  match x st with
  | E.Ok (st, x) -> f x st
  | E.Error msg -> E.Error msg (*  cut evaluation *)

let with_span ~__FILE__ ~__LINE__ name x =
 fun st ->
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ name in
  x _sp st

let bind_err e ~f st =
  match e with
  | E.Ok x -> f x st
  | E.Error msg -> fail msg st (*  cut evaluation *)

let map x ~f st =
  match x st with
  | E.Error msg -> E.Error msg
  | E.Ok (st, x) -> E.Ok (st, f x)

module Syntax = struct
  let ( let* ) x f = bind x ~f
  let ( let+ ) x f = map x ~f
end

open Syntax

let rec fold_l ~f ~x = function
  | [] -> return x
  | y :: ys ->
    let* x' = f x y in
    fold_l ~f ~x:x' ys

let with_phase p (f : unit -> 'a t) : 'a t =
 fun st ->
  let p_name = show_phase p in
  Util.debugf ~section:Const.section 2 "@{<yellow>start phase@} %s" (fun k ->
      k p_name);
  let y =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "phases.phase" in
    Trace.add_data_to_span _sp [ "p", `String p_name ];
    f () st
  in

  Util.debugf ~section:Const.section 2 "@{<yellow>terminate phase@} %s"
    (fun k -> k p_name);
  y

let exit : unit t = with_phase Exit return
let get st = E.Ok (st, st)

let get_key k st =
  match Flex_state.get k st with
  | None -> E.Error "key not found"
  | Some v -> E.Ok (st, v)

let set new_st _st = E.Ok (new_st, ())

let set_key k v st =
  let st = Flex_state.add k v st in
  E.Ok (st, ())

let run_and_discard_l l =
  let rec aux = function
    | [] -> return 0
    | [ a ] -> a
    | a :: tail ->
      let* old_st = get in
      let* n = a in
      if n <> 0 then
        return n
      else
        (* restore old state *)
        let* () = set old_st in
        aux tail
  in
  aux l

let update ~f st =
  let st = f st in
  E.Ok (st, ())

let run_with st m =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "phases.run" in
  try m st
  with e ->
    let stack = Printexc.get_backtrace () in
    let msg = Printexc.to_string e in
    E.Error (msg ^ "\n" ^ stack)

let run m = run_with State.empty m
