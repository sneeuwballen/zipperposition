
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Symbols}

Symbols are abstract, but must be constructible from strings, printable,
comparable and hashable.
*)

type t = private {
  mutable id : int; (* unique ID *)
  name : string;
}
(* TODO: any attribute / def / "needs @?" / Ind. Constructor *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

module Map : Sequence.Map.S with type key = t
module Set : Sequence.Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t

val of_string : string -> t

val gensym : ?prefix:string -> unit -> t
(** Fresh symbol (with unique name) *)

val is_distinct : t -> bool
(* constant with "" around *)

module Seq : sig
  val add_set : Set.t -> t Sequence.t -> Set.t
end

module TPTP : sig
  val i : t
  val o : t
  val int : t
  val rat : t
  val real : t

  (** Higher-order *)
  val forall_fun : t
  val exists_fun : t

  (** {3 Arith Symbols} *)

  module Arith : sig
    val floor : t
    val ceiling : t
    val truncate : t
    val round : t

    val prec : t
    val succ : t

    val sum : t
    val difference : t
    val uminus : t
    val product : t
    val quotient : t

    val quotient_e : t
    val quotient_t : t
    val quotient_f : t
    val remainder_e : t
    val remainder_t : t
    val remainder_f : t

    val is_int : t
    val is_rat : t

    val to_int : t
    val to_rat : t

    val less : t
    val lesseq : t
    val greater : t
    val greatereq : t

    val symbols : t Sequence.t

    val is_arith : t -> bool
    (** Is the symbol a TPTP arithmetic symbol? *)
  end
end
