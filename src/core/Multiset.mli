
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Generic multisets} *)

module type S = Multiset_intf.S

module Make(X : Map.OrderedType) : S with type elt = X.t
