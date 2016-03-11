
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Phases of the Prover}

    To process a file, the prover goes through a sequence of phases that
    are used to build values. This module reifies the phases. *)

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

(** {2 Main Type} *)

(** Monad type, representing an action starting at phase ['p1]
    and stopping at phase ['p2] *)
type (+'a, 'p1, 'p2) t

val string_of_phase : _ phase -> string

val string_of_any_phase : any_phase -> string

val return : 'a -> ('a, 'p, 'p) t
(** Return a value into the monad *)

val exit : (unit, _, [`Exit]) t
(** Exit *)

val start_phase : (_, 'p1, 'p2) phase -> (unit, 'p1, [`InPhase of 'p2]) t
(** Start the given phase *)

val return_phase : 'a -> ('a, [`InPhase of 'p2], 'p2) t
(** Finish the given phase *)

val current_phase : (any_phase, _, 'p2) t
(** Get the current phase *)

val with_phase : ('a, 'p1, 'p2) phase -> f:(unit -> 'a) -> ('a, 'p1, 'p2) t
(** Start phase, call [f ()] to get the result, return its result
    using {!return_phase} *)

val bind :
  ('a, 'p_before, 'p_middle) t ->
  f:('a -> ('b, 'p_middle, 'p_after) t) ->
  ('b, 'p_before, 'p_after) t
(** [bind state f] calls [f] to go one step further from [state] *)

val map : ('a, 'p1, 'p2) t -> f:('a -> 'b) -> ('b, 'p1, 'p2) t
(** Map the current value *)

module Infix : sig
  val (>>=) : ('a, 'p1, 'p2) t -> ('a -> ('b, 'p2, 'p3) t) -> ('b, 'p1, 'p3) t
  val (>|=) : ('a, 'p1, 'p2) t -> ('a -> 'b) -> ('b, 'p1, 'p2) t
end

include module type of Infix

(** {2 State that is threaded from phase to phase}  *)

module State : sig
  type t

  val empty : t

  type 'a key
  val create_key : unit -> 'a key

  val add : 'a key -> 'a -> t -> t

  val get : 'a key -> t -> 'a option

  val get_exn : 'a key -> t -> 'a
  (** @raise Not_found if the key is not present *)
end

val get : (State.t, 'a, 'a) t

val set : State.t -> (unit, 'a, 'a) t

val update : f:(State.t -> State.t) -> (unit, 'a, 'a) t

val run : State.t -> ('a, [`Init], [`Exit]) t -> State.t * 'a
(** [run state m] executes the actions in [m] starting with [state],
    returning some value and the final state. *)

(* FIXME
type callbacks

val empty_callbacks : callbacks

val register_pre : _ phase -> f:(State.t -> State.t) -> callbacks -> callbacks
(** [register_pre p f] adds [f] to the list of the functions to call
    before starting phase [p] *)

val register_post : ('p,_,_) phase -> ('p -> State.t -> State.t) -> callbacks -> callbacks
(** [register_post p f] adds [f] to the list of the functions to call
    with the result of phase [p], immediately after [p] terminates *)
*)
