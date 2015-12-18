
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interface to MSat} *)

include module type of Sat_solver_intf

module Make(X : sig end) : S
