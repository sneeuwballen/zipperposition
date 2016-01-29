
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

module type S = Trail_intf.S

module Make(L : Bool_lit_intf.S) : S with module Lit = L
