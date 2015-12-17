
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Several Multisets} *)

open Logtk

module MT = Multiset.Make(FOTerm)

module MMT = Multiset.Make(MT)
