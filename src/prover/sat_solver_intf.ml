
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module Lit = Bool_lit

type clause = Lit.t list

type result =
  | Sat
  | Unsat

module type S = sig
  val add_clause : ?tag:int -> Lit.t list -> unit

  val add_clauses : ?tag:int -> Lit.t list list -> unit

  val add_clause_seq : ?tag:int -> Lit.t list Sequence.t -> unit

  val check : unit -> result
  (** Is the current problem satisfiable? *)

  val valuation : Lit.t -> bool
  (** Assuming the last call to {!check} returned {!Sat}, get the boolean
      valuation for this (positive) literal in the current model.
      @raise Invalid_argument if [lit <= 0]
      @raise Failure if the last result wasn't {!Sat} *)

  val set_printer : Lit.t CCFormat.printer -> unit
  (** How to print literals? *)

  val unsat_core : int Sequence.t
  (** If [Some seq], [seq] is a sequence of integers
      that are the tags used to obtain [Unsat].
      @raise Invalid_argument if the last result isn't [Unsat] *)

  (** {6 Incrementality}
      We manage a stack for backtracking to older states *)

  (* TODO fix this *)

  type save_level

  val root_save_level : save_level

  val save : unit -> save_level
  (** Save current state on the stack *)

  val restore : save_level -> unit
  (** Restore to a level below in the stack *)
end
