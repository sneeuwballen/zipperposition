
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

type 'a or_error = [`Ok of 'a | `Error of string]

type term = TypedSTerm.t

module type S = sig
  module E : Env.S
  module C = E.C

  type lemma = C.t (* a lemma *)
  type axiom = ID.t * term list
  type theory = ID.t * term list
  type rewrite = (FOTerm.t * FOTerm.t) list (** Rewrite system *)
  type pre_rewrite = (term * term) list

  (** {2 Result: Feedback from the meta-prover} *)

  module Result : sig
    type t

    val lemmas : t -> lemma list
    (** Discovered lemmas *)

    val theories : t -> theory list
    (** Detected theories *)

    val axioms : t -> axiom list
    (** Additional axioms *)

    val rewrite : t -> rewrite list
    (** List of term rewrite systems *)

    val pre_rewrite : t -> pre_rewrite list
    (** Pre-processing rules *)

    val print : Format.formatter -> t -> unit
  end

  (** {2 Interface to the Meta-prover} *)

  val results : unit -> Result.t
  (** Sum of all results obtained so far *)

  val pop_new_results : unit -> Result.t
  (** Obtain the difference between last call to [pop_new_results p]
      and [results p], and pop this difference.
      [ignore (pop_new_results p); pop_new_results p] always
      returns the empty results *)

  val theories : theory Sequence.t
  (** List of theories detected so far *)

  val reasoner : Libzipperposition_meta.Reasoner.t
  (** Meta-level reasoner (inference system) *)

  val prover : Libzipperposition_meta.Prover.t
  (** meta-prover  *)

  val on_theory : theory Signal.t
  val on_lemma : lemma Signal.t
  val on_axiom : axiom Signal.t
  val on_rewrite : rewrite Signal.t
  val on_pre_rewrite : pre_rewrite Signal.t

  val parse_theory_file : string -> Result.t or_error
  (** Update prover with the content of this file, returns the new results
      or an error *)

  val parse_theory_files : string list -> Result.t or_error
  (** Parse several files *)

  val scan_clause : C.t -> Result.t
  (** Scan a clause for axiom patterns, and save it *)

  (** {2 Inference System} *)

  val setup : unit -> unit
  (** [setup ()] registers some inference rules to [E] *)
end
