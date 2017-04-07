
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type profile =
  | P_default
  | P_bfs
  | P_almost_bfs
  | P_explore
  | P_ground
  | P_goal

(** {1 A priority queue of clauses, purely functional} *)
module type S = sig
  module C : Clause_intf.S

  (** {6 Weight functions} *)
  module WeightFun : sig
    type t = C.t -> int
    (** attribute a weight to a clause. The smaller, the better (lightweight
        clauses will be favored). A weight must always be positive;
        the weight of the empty clause should alwyays be 0. *)

    val default : t
    (** Use {!Literal.heuristic_weight} *)

    val penalty : t
    (** Returns the penalty of the clause *)

    val favor_all_neg : t
    (** Favor clauses with only negative ground lits *)

    val favor_non_all_neg : t
    (** Favor clauses that have at least one non-(ground negative) lit *)

    val favor_ground : t

    val favor_horn : t

    val favor_goal : t
    (** The closest a clause is from the initial goal, the lowest its weight.
        Some threshold is used for clauses that are too far away *)

    val combine : (t * int) list -> t
    (** Combine a list of pairs [w, coeff] where [w] is a weight function,
        and [coeff] a strictly positive number. This is a weighted sum
        of weights. *)
  end

  type t
  (** A priority queue. *)

  val add : t -> C.t -> unit
  (** Add a clause to the Queue *)

  val add_seq : t -> C.t Sequence.t -> unit
  (** Add clauses to the queue *)

  val length : t -> int
  (** Number of elements *)

  val is_empty : t -> bool
  (** check whether the queue is empty *)

  val take_first : t -> C.t
  (** Take first element of the queue, or raise Not_found *)

  val name : t -> string
  (** Name of the implementation/role of the queue *)

  (** {6 Available Queues} *)

  val make : ratio:int -> weight:(C.t -> int) -> string -> t
  (** Bring your own implementation of queue.
      @param ratio pick-given ratio. One in [ratio] calls to {!take_first},
        the returned clause comes from a FIFO; the other times it comes
        from a priority queue that uses [weight] to sort clauses
      @param name the name of this clause queue *)

  val bfs : unit -> t
  (** FIFO *)

  val almost_bfs : unit -> t
  (** Half FIFO, half default *)

  val explore : unit -> t
  (** Use heuristics for selecting "small" clauses *)

  val ground : unit -> t
  (** Favor positive unit clauses and ground clauses *)

  val goal_oriented : unit -> t
  (** custom weight function that favors clauses that are "close" to
      initial conjectures. It is fair.  *)

  val default : unit -> t
  (** Obtain the default queue *)

  val of_profile : profile -> t
  (** Select the queue corresponding to the given profile *)

  (** {6 IO} *)

  val pp : t CCFormat.printer
  val to_string : t -> string
end

