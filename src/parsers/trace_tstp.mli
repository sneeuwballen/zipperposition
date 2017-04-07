
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Trace of a TSTP prover} *)

open Logtk

type id = Ast_tptp.name

type term = STerm.t
type form = STerm.t
type clause = term SLiteral.t list

type t =
  | Axiom of string * string (* filename, axiom name *)
  | Theory of string (* a theory used to do an inference *)
  | InferForm of form * step lazy_t
  | InferClause of clause * step lazy_t

and step = {
  id : id;
  rule : string;
  parents : t array;
  esa : bool; (** Equisatisfiable step? *)
}

val equal : t -> t -> bool
val compare : t -> t -> int

val mk_f_axiom : id:id -> form -> file:string -> name:string -> t
val mk_c_axiom : id:id -> clause -> file:string -> name:string -> t
val mk_f_step : ?esa:bool -> id:id -> form -> rule:string -> t list -> t
val mk_c_step : ?esa:bool -> id:id -> clause -> rule:string -> t list -> t

val is_axiom : t -> bool
val is_theory : t -> bool
val is_step : t -> bool
val is_proof_of_false : t -> bool

val get_id : t -> id
(** Obtain the ID of the proof step.
    @raise Invalid_argument if the step is Axiom or Theory *)

val force : t -> unit
(** Force the lazy proof step, if any *)

(** {3 Proof traversal} *)

module StepTbl : Hashtbl.S with type key = t

type proof_set = unit StepTbl.t

val is_dag : t -> bool
(** Is the proof a proper DAG? *)

val traverse : ?traversed:proof_set -> t -> (t -> unit) -> unit
(** Traverse the proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val to_seq : t -> t Sequence.t
(** Traversal of parent proofs *)

val depth : t -> int
(** Max depth of the proof *)

val size : t -> int
(** Number of nodes in the proof *)

(** {3 IO} *)

type 'a or_error = ('a, string) CCResult.t

val of_decls : form Ast_tptp.t Sequence.t -> t or_error
(** Try to extract a proof from a list of TSTP statements. *)

val parse : ?recursive:bool -> string -> t or_error
(** Try to parse a proof from a file. *)

include Interfaces.PRINT with type t := t
(** Debug printing, non recursive *)

val pp1 : t CCFormat.printer
(** Print proof step, and its parents *)

val pp_tstp : t CCFormat.printer
(** print the whole proofs *)
