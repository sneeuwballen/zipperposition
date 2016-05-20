
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type profile =
  | P_default
  | P_bfs
  | P_explore
  | P_ground
  | P_goal
  | P_goal_bfs

type weight = int list

(** {1 A priority queue of clauses, purely functional} *)
module type S = sig
  module C : Clause.S

  (** {6 Weight functions} *)
  module WeightFun : sig
    type single = C.t -> int
    (** attribute a weight to a clause. The smaller, the better (lightweight
        clauses will be favored). A weight must always be positive;
        the weight of the empty clause should always be 0. *)

    type t = C.t -> weight
    (** A weight function is a lexicographically ordered sequence of weights *)

    val default : single
    (** Use {!Literal.heuristic_weight} *)

    val age : single
    (** Returns the age of the clause (or 0 for the empty clause) *)

    val favor_all_neg : single
    (** Favor clauses with only negative ground lits *)

    val favor_non_all_neg : single
    (** Favor clauses that have at least one non-(ground negative) lit *)

    val favor_ground : single

    val favor_horn : single

    val favor_goal : single
    (** The closest a clause is from the initial goal, the lowest its weight.
        Some threshold is used for clauses that are too far away *)

    val single : single -> t
    (** Single weight *)

    val combine_linear : (single * int) list -> t
    (** Combine a list of pairs [w, coeff] where [w] is a weight function,
        and [coeff] a strictly positive number. This is a weighted sum
        of weights. *)

    val combine_lexico : t -> t -> t
    (** [combine_lexico a b] puts the weight of [a] before the weight of [b],
        lexicographically. Only clauses for whch [a] gives the same weight
        are compared using [b] *)

    val combine_lexico_l : t list -> t
    (** @raise Invalid_argument if the list is empty *)
  end

  type t
  (** A priority queue. *)

  val add : t -> C.t -> t
  (** Add a clause to the Queue *)

  val adds : t -> C.t Sequence.t -> t
  (** Add clauses to the queue *)

  val is_empty : t -> bool
  (** check whether the queue is empty *)

  val take_first : t -> (t * C.t)
  (** Take first element of the queue, or raise Not_found *)

  val name : t -> string
  (** Name of the implementation/role of the queue *)

  (** {6 Available Queues} *)

  val make : weight:WeightFun.t -> string -> t
  (** Bring your own implementation of queue. The [weight] function
      should be fair, i.e. for any clause [c], the weight of [c] should
      eventually be the smallest one in the queue (e.g., using the age
      of clauses in the weight somewhere should be enough) *)

  val bfs : t
  (** Strong orientation toward FIFO *)

  val explore : t
  (** Use heuristics for selecting "small" clauses *)

  val ground : t
  (** Favor positive unit clauses and ground clauses *)

  val goal_oriented : t
  (** custom weight function that favors clauses that are "close" to
      initial conjectures. It is fair.  *)

  val default : t
  (** Obtain the default queue *)

  val of_profile : profile -> t
  (** Select the queue corresponding to the given profile *)

  (** {6 IO} *)

  val pp : t CCFormat.printer
  val to_string : t -> string
end

