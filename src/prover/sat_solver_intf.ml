
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type result =
  | Sat
  | Unsat

exception WrongState of string

module type S = sig
  module Lit : Bool_lit_intf.S

  type clause = Lit.t list

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

  val valuation_level : Lit.t -> bool * int
  (** Gives the value of a literal in the model, as well as its
      decision level. If decision level is 0, the literal has been proved,
      rather than decided/propagated *)

  val set_printer : Lit.t CCFormat.printer -> unit
  (** How to print literals? *)

  val unsat_core : int Sequence.t
  (** If [Some seq], [seq] is a sequence of integers
      that are the tags used to obtain [Unsat].
      @raise WrongState if the last result isn't [Unsat] *)

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
