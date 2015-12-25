
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 BBox (Boolean Box)}

    This module defines a way to encapsulate clauses and some meta-level
    properties into boolean literals, and maintains a bijection between
    encapsulated values and boolean literals *)

val section : Libzipperposition.Util.Section.t

module type S = BBox_intf.S

module Make
    (I : BBox_intf.TERM)
    (Case : BBox_intf.TERM)
  : S
    with module I = I
     and module Case = Case
