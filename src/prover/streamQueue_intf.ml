
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 A priority queue of streams} *)

module type S = sig
  module Stm : Stream_intf.S

  (** {5 Weight functions} *)
  module WeightFun : sig
    type t = Stm.t -> int
    (** attribute a weight to a stream. The smaller, the better (lightweight
        streams will be favored). A weight must always be positive. *)

    val penalty : t
    (** Returns the penalty of the stream *)

    val combine : (t * int) list -> t
    (** Combine a list of pairs [w, coeff] where [w] is a weight function,
        and [coeff] a strictly positive number. This is a weighted sum
        of weights. *)
  end

  type t
  (** A priority queue. *)

  val add : t -> Stm.t -> unit
  (** Add a stream to the Queue *)

  val add_lst : t -> Stm.t list -> unit
  (** Add a list of streams to the queue *)

  val length : t -> int
  (** Number of elements *)

  val is_empty : t -> bool
  (** check whether the queue is empty *)

  (* val take_first_when_available : t -> Stm.C.t option
   * (\** Attempts to take a clause out of the queue if available
   *     (meaning that the clause is searched for only when the
   *     time_before_drip counter reaches 0), or raise Not_found.
   *     Guarded recursion: can't loop forever
   *     @raise Not_found in the guard is reached *\) *)

  val take_first : t -> Stm.C.t option
  (** Attempts to take a clause out of the queue.
      Guarded recursion: can't loop forever
      @raise Not_found in the guard is reached *)

  val take_fair_anyway: t -> Stm.C.t option list
  (** Takes clauses from the queue in a fair manner.
      Unguarded recursion, may loop forever *)

  val take_stm_nb: t -> Stm.C.t option list
  (** Attempts to take as many clauses from the queue as there are streams in
      the queue. Calls take_first to do so and stops if its guard is reached *)

  val take_stm_nb_fix_stm: t -> Stm.C.t option list
  (** Attempts to take as many clauses from the queue as there are streams in
      the queue. Extract as many clauses as possible from first stream before
      moving to a new stream to find more clauses if necessary *)

  val name : t -> string
  (** Name of the implementation/role of the queue *)

  (** {5 Available Queues} *)

  val make : guard:int -> ratio:int -> weight:(Stm.t -> int) -> string -> t
  (** Creates a priority queue that uses [weight] to sort streams.
      @param ratio pick-given ratio. Only one in [ratio] truly returns
        a clause if there is one available in calls to {!take_first_when_available}
        and {!take_fair_anyway}.
      @param name the name of this stream queue *)

  val default : unit -> t
  (** Obtain the default queue *)

  (** {5 IO} *)

  val pp : t CCFormat.printer
  val to_string : t -> string
end

