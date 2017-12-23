
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover} *)

open Logtk
open Libzipperposition
module E = CCResult

type filename = string
type 'a or_error = ('a, string) CCResult.t

(** {2 Phases} *)

type env_with_clauses =
    Env_clauses : 'c Env.packed * 'c Clause.sets -> env_with_clauses

type env_with_result =
    Env_result : 'c Env.packed * Saturate.szs_status -> env_with_result

type errcode = int

type prelude = UntypedAST.statement Sequence.t

type ('ret, 'before, 'after) phase =
  | Init : (unit, _, [`Init]) phase (* global setup *)
  | Setup_gc : (unit, [`Init], [`Init]) phase
  | Setup_signal : (unit, [`Init], [`Init]) phase
  | Parse_CLI :
      (filename list * Params.t, [`Init], [`Parse_cli]) phase
  (* parse CLI options: get a list of files to process, and parameters *)
  | LoadExtensions : (Extensions.t list, [`Parse_cli], [`LoadExtensions]) phase
  | Parse_prelude : (prelude, [`LoadExtensions], [`Parse_prelude]) phase
  | Start_file :
      (filename, [`Parse_prelude], [`Start_file]) phase (* file to process *)
  | Parse_file :
      (Input_format.t * UntypedAST.statement Sequence.t,
       [`Start_file], [`Parse_file]) phase (* parse some file *)
  | Typing :
      (TypeInference.typed_statement CCVector.ro_vector, [`Parse_file], [`Typing]) phase
  | CNF :
      (Statement.clause_t CCVector.ro_vector, [`Typing], [`CNF]) phase
  | Compute_prec :
      (Precedence.t, [`CNF], [`Precedence]) phase
  | Compute_ord_select :
      (Ordering.t * Selection.t, [`Precedence], [`Compute_ord_select]) phase
  (* compute orderign and selection function *)

  | MakeCtx : ((module Ctx.S), [`Compute_ord_select], [`MakeCtx]) phase

  | MakeEnv : (env_with_clauses, [`MakeCtx], [`MakeEnv]) phase

  | Pre_saturate :
      ('c Env.packed * Saturate.szs_status * 'c Clause.sets,
       [`MakeEnv], [`Pre_saturate]) phase

  | Saturate :
      (env_with_result, [`Pre_saturate], [`Saturate]) phase

  | Print_result : (unit, [`Saturate], [`Print_result]) phase
  | Print_dot : (unit, [`Print_result], [`Print_dot]) phase
  | Check_proof : (errcode, [`Print_dot], [`Check_proof]) phase
  | Print_stats : (unit, [`Check_proof], [`Print_stats]) phase
  | Exit : (unit, _, [`Exit]) phase

type any_phase = Any_phase : (_, _, _) phase -> any_phase
(** A phase hidden in an existential type *)

module State = Flex_state

module Key = struct
  let cur_phase = State.create_key()
end

(* empty state: at Init *)
let empty_state = State.empty |> State.add Key.cur_phase (Any_phase Init)

(* A simple state monad *)
type (+'a, 'p1, 'p2) t = State.t -> (State.t * 'a) or_error

let string_of_phase : type a b c. (a,b,c) phase -> string
  = function
    | Init -> "init"
    | Setup_gc -> "setup_gc"
    | Setup_signal -> "setup_signal"
    | Parse_CLI  -> "parse_cli"
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
    | Pre_saturate  -> "pre_saturate"
    | Saturate -> "saturate"
    | Print_result -> "print_result"
    | Print_stats -> "print_stats"
    | Print_dot -> "print_dot"
    | Check_proof -> "check_proof"
    | Exit -> "exit"

let string_of_any_phase (Any_phase p) = string_of_phase p

let return x st = E.return (st, x)

let return_err x st = match x with
  | E.Ok x -> E.Ok (st, x)
  | E.Error msg -> E.Error msg

let fail msg _ = E.Error msg

let bind x ~f st =
  match x st with
    | E.Ok (st, x) -> f x st
    | E.Error msg -> E.Error msg  (*  cut evaluation *)

let bind_err e ~f st =
  match e with
    | E.Ok x -> f x st
    | E.Error msg -> fail msg st (*  cut evaluation *)

let map x ~f st = match x st with
  | E.Error msg -> E.Error msg
  | E.Ok (st, x) -> E.Ok (st, f x)

module Infix = struct
  let (>>=) x f = bind x ~f
  let (>>?=) x f = bind_err x ~f
  let (>|=) x f = map x ~f
end

include Infix

let rec fold_l ~f ~x = function
  | [] -> return x
  | y :: ys -> f x y >>= fun x' -> fold_l ~f ~x:x' ys

let current_phase st =
  try E.Ok (st, State.get_exn Key.cur_phase st)
  with Not_found ->
    let msg = "could not find current phase" in
    E.Error msg

let start_phase p st =
  Util.debugf ~section:Const.section 2 "@{<yellow>start phase@} %s" (fun k->k (string_of_phase p));
  let st = State.add Key.cur_phase (Any_phase p) st in
  E.Ok (st, ())

let return_phase_err x =
  current_phase >>= fun p ->
  Util.debugf ~section:Const.section 2 "@{<yellow>terminate phase@} %s"
    (fun k->k (string_of_any_phase p));
  return_err x

let return_phase x = return_phase_err (E.Ok x)

let with_phase1 p ~f x =
  start_phase p >>= fun () ->
  let y = f x in
  return_phase y

let with_phase p ~f = with_phase1 p ~f ()

let with_phase2 p ~f x y = with_phase1 p ~f:(f x) y

let exit =
  start_phase Exit >>= fun () ->
  return_phase ()

let get st = E.Ok (st, st)

let get_key k st =
  match Flex_state.get k st with
    | None -> E.Error "key not found"
    | Some v -> E.Ok (st, v)

let set new_st _st = E.Ok (new_st, ())

let set_key k v st =
  let st = Flex_state.add k v st in
  E.Ok (st, ())

let run_parallel l =
  let rec aux = function
    | [] -> return 0
    | [a] -> a
    | a :: tail ->
      get >>= fun old_st ->
      a >>= fun n ->
      if n<>0 then return n
      else (
        (* restore old state *)
        set old_st >>= fun () ->
        aux tail
      )
  in
  aux l

let update ~f st =
  let st = f st in
  E.Ok (st, ())

let run_with st m =
  try
    m st
  with e ->
    let stack = Printexc.get_backtrace () in
    let msg = Printexc.to_string e in
    E.Error (msg ^ "\n" ^ stack)

let run m = run_with State.empty m
