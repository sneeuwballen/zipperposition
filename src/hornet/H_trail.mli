
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

open Hornet_types

type t = bool_trail

val empty : t

val is_empty : t -> bool

val make : bool_lit lazy_t list -> t

val merge : t -> t -> t

val is_absurd : t -> bool

val subsumes : t -> t -> bool
(** [subsumes a b] means that [a] is a subset of [b] *)

val exists : (bool_lit -> bool) -> t -> bool

val to_list : t -> bool_lit list

val of_list : bool_lit list -> t

include Interfaces.PRINT with type t := t
include Interfaces.EQ with type t := t

val pp_opt : t CCFormat.printer

val bool_lits : t -> bool_lit Sequence.t
