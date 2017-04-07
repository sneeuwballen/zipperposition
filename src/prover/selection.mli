
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Selection functions} *)

open Logtk

(** See "E: a brainiac theorem prover". A selection function
    returns a bitvector of selected literals. *)

type t = Literal.t array -> CCBV.t

val no_select : t

val select_max_goal : strict:bool -> ord:Ordering.t -> t
(** Select a maximal negative literal, if any, or nothing *)

val select_diff_neg_lit : strict:bool -> ord:Ordering.t -> t
(** arbitrary negative literal with maximal weight difference between sides *)

val select_complex : strict:bool -> ord:Ordering.t -> t
(** x!=y, or ground negative lit, or like select_diff_neg_lit *)

val select_complex_except_RR_horn : strict:bool -> ord:Ordering.t -> t
(** if clause is a restricted range horn clause, then select nothing;
    otherwise, like select_complex *)

(** {2 Global selection Functions} *)

val default_selection : ord:Ordering.t -> t
(** Default selection function *)

val selection_from_string : ord:Ordering.t -> string -> t
(** selection function from string (may fail) *)

val available_selections : unit -> string list
(** available names for selection functions *)

val register : string -> (ord:Ordering.t -> t) -> unit
(** Register new selection function
    @raise Failure if the name is already used *)
