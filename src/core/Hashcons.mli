(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsing} *)

module type HashedType = Logtk_hashcons.HashedType
module type S = Logtk_hashcons.S

module Make (X : HashedType) : S with type elt = X.t
