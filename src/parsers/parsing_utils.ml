
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Parsing} *)

open Logtk
open CCResult.Infix

let parse_tptp file =
  Util_tptp.parse_file ~recursive:true file
  >|= Sequence.map Util_tptp.to_ast

let parse_tip file =
  Util_tip.parse_file file
  >|= Util_tip.convert_seq

type input =
  | I_tptp
  | I_zf
  | I_tip

let guess_input (file:string): input =
  if CCString.suffix ~suf:".p" file
  then I_tptp
  else if CCString.suffix ~suf:".smt2" file
  then I_tip
  else if CCString.suffix ~suf:".zf" file
  then I_zf
  else failwith ("unable to guess syntax for " ^ file)

(** Parse file using the input format chosen by the user *)
let input_of_file (file:string): input = match !Options.input with
  | Options.I_tptp -> I_tptp
  | Options.I_zf -> I_zf
  | Options.I_tip -> I_tip
  | Options.I_guess -> guess_input file

let parse_file (i:input) (file:string) = match i with
  | I_tptp -> parse_tptp file
  | I_zf -> Util_zf.parse_file file
  | I_tip -> parse_tip file

let on_undef_id (i:input) = match i with
  | I_tptp -> `Guess
  | I_tip -> `Fail
  | I_zf -> `Warn
