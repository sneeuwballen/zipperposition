
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 A priority queue of streams} *)

module type S = sig
  module Stm : Stream_intf.S

  (** {6 Weight functions} *)
  module WeightFun : sig
    type t = Stm.t -> int
    (** attribute a weight to a clause. The smaller, the better (lightweight
        clauses will be favored). A weight must always be positive;
        the weight of the empty clause should alwyays be 0. *)
    (*TODO: is this still true for streams or only for clauses?*)

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

  val take_first_when_available : t -> Stm.t option
  (** Take first element of the queue if available, or raise Not_found *)

  val tak_first_anyway: t -> Stm.t
  (** Take first element of the queue, or raise Not_found *)

  val name : t -> string
  (** Name of the implementation/role of the queue *)

  (** {6 Available Queues} *)

  val make : ratio:int -> weight:(Stm.t -> int) -> string -> t
  (** Bring your own implementation of queue.
      @param ratio pick-given ratio. One in [ratio] calls to {!take_first},
        the returned stream comes from a FIFO; the other times it comes
        from a priority queue that uses [weight] to sort streams
      @param name the name of this stream queue *)

  val default : unit -> t
  (** Obtain the default queue *)

  (** {6 IO} *)

  val pp : t CCFormat.printer
  val to_string : t -> string
end

