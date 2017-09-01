
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Output Format} *)

type t =
  | O_none
  | O_normal
  | O_tptp
  | O_zf

let normal : t = O_normal
let tptp : t = O_tptp
let zf : t = O_zf
let none : t = O_none
let default : t = normal

let pp out (i:t) = match i with
  | O_tptp -> CCFormat.string out "tptp"
  | O_zf -> CCFormat.string out "zf"
  | O_none -> CCFormat.string out "none"
  | O_normal -> CCFormat.string out "normal"

(** Prefix to use for line comments *)
let comment_prefix = function
  | O_tptp -> "% "
  | O_normal
  | O_zf -> "# "
  | O_none -> ""
