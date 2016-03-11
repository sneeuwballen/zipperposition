
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover} *)

type filename = string

(** {2 Phases} *)

type ('ret, 'before, 'after) phase =
  | Init : (unit, _, [`Init]) phase (* global setup *)
  | Parse_CLI : (unit, [`Init], [`Parse_cli]) phase (* parse CLI options *)
  | LoadExtensions : (Extensions.t list, [`Parse_cli], [`LoadExtensions]) phase
  | Start_file :
    (filename, [`LoadExtensions], [`Start_file]) phase (* file to process *)
  | Parse_file :
    (UntypedAST.statement Sequence.t, [`Start_file], [`Parse_file]) phase (* parse some file *)
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
  | MakeEnv : ((module Env.S), [`MakeCtx], [`MakeEnv]) phase

  | Extract_clauses :
    ((module Env.S with type C.t = 'c) * 'c CCVector.ro_vector, [`MakeEnv], [`Extract_clauses]) phase

  | Pre_saturate :
    ((module Env.S with type C.t = 'c) * 'c CCVector.ro_vector, [`Extract_clauses], [`Pre_saturate]) phase

  | Saturate :
    ((module Env.S with type C.t = 'c) * 'c Saturate.szs_status, [`Pre_saturate], [`Saturate]) phase

  | Print_stats : (unit, [`Saturate], [`Print_stats]) phase
  | Print_dot : (unit, [`Print_stats], [`Print_dot]) phase
  | Exit : (unit, _, [`Exit]) phase

type any_phase = Any_phase : (_, _, _) phase -> any_phase
(** A phase hidden in an existential type *)

module State = struct
  module M = CCHet.Map

  type 'a key = 'a CCHet.Key.t

  let create_key () = CCHet.Key.create()

  type t = M.t

  let add = M.add
  let get = M.find
  let get_exn = M.find_exn

  (* key used to store the current phase *)
  let cur_phase_key = create_key()

  (* empty state: at Init *)
  let empty = M.empty |> add cur_phase_key (Any_phase Init)
end

(* A simple state monad *)
type (+'a, 'p1, 'p2) t = State.t -> State.t * 'a

let string_of_phase : type a b c. (a,b,c) phase -> string
  = function
  | Init -> "init"
  | Parse_CLI  -> "parse_cli"
  | LoadExtensions -> "load_extensions"
  | Start_file -> "start_file"
  | Parse_file -> "parse_file"
  | Typing -> "typing"
  | CNF -> "cnf"
  | Compute_prec -> "compute_prec"
  | Compute_ord_select -> "compute_ord_select"
  | MakeCtx -> "make_ctx"
  | MakeEnv -> "make_env"
  | Extract_clauses -> "extract_clauses"
  | Pre_saturate  -> "pre_saturate"
  | Saturate -> "saturate"
  | Print_stats -> "print_stats"
  | Print_dot -> "print_dot"
  | Exit -> "exit"

let string_of_any_phase (Any_phase p) = string_of_phase p

let return x st = st, x

let bind x ~f st =
  let st, x = x st in
  f x st

let map x ~f st =
  let st, x = x st in
  st, f x

module Infix = struct
  let (>>=) x f = bind x ~f
  let (>|=) x f = map x ~f
end

include Infix

let current_phase st = st, State.get_exn State.cur_phase_key st

let start_phase p st =
  let st = State.add State.cur_phase_key (Any_phase p) st in
  st, ()

let return_phase x = return x

let with_phase p ~f =
  start_phase p >>= fun () ->
  let x = f () in
  return_phase x

let exit st =
  let st, () = start_phase Exit st in
  return_phase () st

let get st = st, st

let set new_st _st = new_st, ()

let update ~f st =
  let st = f st in
  st, ()

let run st m = m st

(* FIXME
type 'a cb = 'a -> unit
type 'a cbs = 'a cb list

type callbacks =
  pre_init : unit cbs;
  post_init : unit cbs;

val empty_callbacks : callbacks
*)



