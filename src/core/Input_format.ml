
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Input Format} *)

let shadow_mode = ref (None : (bool option))

type t =
  | I_tptp
  | I_zf
  | I_tip
  | I_dk

let tptp : t = I_tptp
let tip : t = I_tip
let zf : t = I_zf
let dk : t = I_dk
let default : t = zf

let pp out (i:t) = match i with
  | I_tptp -> CCFormat.string out "tptp"
  | I_zf -> CCFormat.string out "zf"
  | I_tip -> CCFormat.string out "tip"
  | I_dk -> CCFormat.string out "dk"

(** What to do when we have an undefined ID in the corresponding format? *)
let on_undef_id (i:t) = match i with
  | I_tptp -> `Guess
  | I_dk
  | I_tip
  | I_zf -> `Fail

(** What to do when we have a shadowing decl? *)
let on_shadow (i:t) = 
  match !shadow_mode with 
  | None ->  (match i with
    | I_dk -> `Ignore
    | I_tptp
    | I_tip
    | I_zf -> `Warn)
  | Some b -> if b then `Warn else `Ignore

(** what to do when we have a variable without a type declaration? *)
let on_var (i:t) = match i with
  | I_tptp -> `Default
  | I_dk
  | I_tip
  | I_zf -> `Infer

(** Do we add implicit type parameters when '@' is not present? *)
let implicit_ty_args (i:t) : bool = match i with
  | I_tptp | I_dk -> false
  | I_tip | I_zf -> true
