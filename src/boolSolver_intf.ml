
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

type 'a printer = Format.formatter -> 'a -> unit

(** One instance of boolean solver. *)
module type SAT = sig
  type lit = int

  type result =
    | Sat
    | Unsat

  val add_clause : lit list -> unit
  val add_clauses : lit list list -> unit

  val check : unit -> result
  (** Is the current problem satisfiable? *)

  val valuation : lit -> bool
  (** Assuming the last call to {!check} returned {!Sat}, get the boolean
      valuation for this (positive) literal in the current model.
      @raise Invalid_argument if [lit <= 0]
      @raise Failure if the last result wasn't {!Sat} *)

  val set_printer : lit printer -> unit
  (** How to print literals? *)

  val name : string
  (** Name of the solver *)
end

module type QBF = sig
  include SAT

  type quantifier = Qbf.quantifier

  module LitSet : Set.S with type elt = lit
  (** Set of literals *)

  (** {2 Quantifier Stack}

  A stack of quantifiers is maintained, along with a set of literals
  at each stage. *)

  type level = private int
  (** Quantification level *)

  val level0 : level
  (** Level 0 is the outermost level, existentially quantified. *)

  val quant_at_level : level -> quantifier
  (** Which quantifier is it at this level? *)

  val lits_at_level : level -> LitSet.t
  (** Which literals are quantified at this level? *)

  val push : quantifier -> lit list -> level
  (** Push a new level on top of the others *)

  val quantify_lits : level -> lit list -> unit
  (** Add some literals at the given quantification level *)

  (** The functions from {!SAT}, such as {!SAT.check}, still work. They
      convert the current formulas to CNF and send the whole problem to
      the QBF solver. *)
end
