
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

open Logtk

module Lit = BBox.Lit

type t

val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int

type bool_lit = Lit.t

val empty : t
val singleton : bool_lit -> t
val mem : bool_lit -> t -> bool
val add : bool_lit -> t -> t
val remove : bool_lit -> t -> t
val map : (bool_lit -> bool_lit) -> t -> t
val fold : ('a -> bool_lit -> 'a) -> 'a -> t -> 'a
val length : t -> int
val for_all : (bool_lit -> bool) -> t -> bool
val exists : (bool_lit -> bool) -> t -> bool
val of_list : bool_lit list -> t
val add_list : t -> bool_lit list -> t
val to_list : t -> bool_lit list
val to_seq : t -> bool_lit Sequence.t

val labels : t -> Index_intf.labels

val subsumes : t -> t -> bool
(** [subsumes a b] is true iff [a] is a subset of [b] *)

val is_empty : t -> bool
(** Empty trail? *)

val is_trivial : t -> bool
(** returns [true] iff the trail contains both [i] and [-i]. *)

val merge : t -> t -> t
(** Merge several trails (e.g. from different clauses) *)

val merge_l : t list -> t
(** Merge several trails (e.g. from different clauses) *)

val filter : (bool_lit -> bool) -> t -> t
(** Only keep a subset of boolean literals *)

type valuation = bool_lit -> bool
(** A boolean valuation *)

val is_active : t -> v:valuation -> bool
(** [Trail.is_active t ~v] is true iff all boolean literals
    in [t] are satisfied in the boolean valuation [v]. *)

val to_s_form : t -> TypedSTerm.Form.t
