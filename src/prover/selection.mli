
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

val e_sel  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectMaxLComplexAvoidPosPred  *)

val e_sel2  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectCQIPrecWNTNp  *)

val e_sel3  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectComplexG *)

val e_sel5  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectNDepth2OptimalLiteral *)

val e_sel6  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectLargestOrientable *)

val e_sel7  : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectComplexExceptRRHorn *)

val e_sel8 : ord:Ordering.t -> t
(** Selection function  identical to E's 
    SelectCQArNTNpEqFirst *)

val ho_sel  : ord:Ordering.t -> t
(** Selection function that tries to take 
    into account the number of nested applied variables.

    The assumption is that they are hard for unification.
*)

val from_string : ord:Ordering.t -> string -> t
(** selection function from string (may fail) *)

val all : unit -> string list
(** available names for selection functions *)
