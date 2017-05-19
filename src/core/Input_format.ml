
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Input Format} *)

type t =
  | I_tptp
  | I_zf
  | I_tip

let tptp : t = I_tptp
let tip : t = I_tip
let zf : t = I_zf
let default : t = zf

let pp out (i:t) = match i with
  | I_tptp -> CCFormat.string out "tptp"
  | I_zf -> CCFormat.string out "zf"
  | I_tip -> CCFormat.string out "tip"

(** What to do when we have an undefined ID in the corresponding format? *)
let on_undef_id (i:t) = match i with
  | I_tptp -> `Guess
  | I_tip
  | I_zf -> `Fail

(** what to do when we have a variable without a type declaration? *)
let on_var (i:t) = match i with
  | I_tptp -> `Default
  | I_tip
  | I_zf -> `Infer
