
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Selection functions} *)

open Logtk

(** See "E: a brainiac theorem prover". A selection function
    returns a bitvector of selected literals.

    The [strict] parameter, if true, means only one negative
    literal is selected (at most);
    if [strict=false] then all positive literals are also selected.
*)

type t = Literal.t array -> CCBV.t

type parametrized = strict:bool -> ord:Ordering.t -> t

val no_select : t
(** Never select literals. *)

val max_goal : parametrized
(** Select a maximal negative literal, if any, or nothing *)

val except_RR_horn : parametrized -> parametrized
(** [except_RR_horn p] behaves like [p], except if the clause is
    a range-restricted Horn clause. In that case, we assume the clause
    is a (conditional) rewrite rule and we don't prevent using it
    as an active clause. *)

(** {2 Global selection Functions} *)

val default : ord:Ordering.t -> t
(** Default selection function *)

val from_string : ord:Ordering.t -> string -> t
(** selection function from string (may fail) *)

val all : unit -> string list
(** available names for selection functions *)
