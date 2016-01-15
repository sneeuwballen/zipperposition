
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type profile =
  | P_default
  | P_bfs
  | P_explore
  | P_ground
  | P_goal

(** {1 A priority queue of clauses, purely functional} *)
module type S = sig
  module C : Clause.S

  (** {6 Weight functions} *)
  module WeightFun : sig
    type t = C.t -> int
    (** attribute a weight to a clause. The smaller, the better (lightweight
        clauses will be favored). A weight must always be positive;
        the weight of the empty clause should alwyays be 0. *)

    val default : t
    (** Use {!Literal.heuristic_weight} *)

    val age : t
    (** Returns the age of the clause (or 0 for the empty clause) *)

    val favor_non_goal : t

    val favor_goal : t

    val favor_ground : t

    val favor_horn : t

    val favor_conjecture : t
    (** The closest a clause is from conjectures, the lowest its weight.
        Some threshold is used for clauses that are too far away *)

    val combine : (t * int) list -> t
    (** Combine a list of pairs [w, coeff] where [w] is a weight function,
        and [coeff] a strictly positive number. This is a weighted sum
        of weights. *)
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

  val make : weight:(C.t -> int) -> string -> t
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

