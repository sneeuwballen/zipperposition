(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = sig
  type t = private {
    id: int;  (** unique ID of the stream *)
    parents: Clause.t list;
        (** parent clauses for inference generating this stream *)
    mutable penalty: int;  (** heuristic penalty *)
    mutable hits: int;  (** how many attemts to retrieve unifier were there *)
    mutable stm: Clause.t option OSeq.t;  (** the stream itself *)
  }

  exception Empty_Stream
  exception Drip_n_Unfinished of Clause.t option list * int * int

  (** {2 Basics} *)

  val make :
    ?penalty:int -> parents:Clause.t list -> Clause.t option OSeq.t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val id : t -> int
  val is_empty : t -> bool
  val penalty : t -> int

  val drip : t -> Clause.t option
  (** Remove the first element in the stream and return it.
      @raise Empty_Stream if the stream is empty *)

  val drip_n : t -> int -> int -> Clause.t option list
  (** Attempt to remove the n first elements in the stream and return them.
      Return less if the guard is reached.
      @raise Drip_n_Unfinished(cl,n)
        where cl is the list of elements already found and n the number of
        elements if the stream contains less than n elements *)

  (** {2 IO} *)

  val pp : t CCFormat.printer
end
