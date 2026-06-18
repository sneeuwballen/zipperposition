(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Heuristics} *)

open Libzipperposition

val enable_depth_limit : int -> unit
(** Set a maximal depth for terms. Any clause with a term deeper than this limit
    will be dismissed.

    This breaks completeness in general, but can be very useful in practice. *)

val extension : Extensions.t
