
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open CCResult.Infix

type 'a or_error = ('a, string) CCResult.t

let parse_tptp file : _ or_error =
  Util_tptp.parse_file ~recursive:true file
  >|= Sequence.map Util_tptp.to_ast

let parse_tip file =
  Util_tip.parse_file file
  >|= Util_tip.convert_seq

let parse_dk file = Util_dk.parse_file file

let guess_input (file:string): Input_format.t =
  if CCString.suffix ~suf:".p" file || CCString.suffix ~suf:".tptp" file
  then Input_format.tptp
  else if CCString.suffix ~suf:".smt2" file
  then Input_format.tip
  else if CCString.suffix ~suf:".zf" file
  then Input_format.zf
  else if CCString.suffix ~suf:".skp" file
  then Input_format.dk
  else (
    let res = Input_format.default in
    Util.warnf "unable to guess syntax for `%s`, use default syntax (%a)"
      file Input_format.pp res;
    res
  )

(** Parse file using the input format chosen by the user *)
let input_of_file (file:string): Input_format.t = match !Options.input with
  | Options.I_tptp -> Input_format.tptp
  | Options.I_zf -> Input_format.zf
  | Options.I_tip -> Input_format.tip
  | Options.I_dk -> Input_format.dk
  | Options.I_guess -> guess_input file

let parse_file (i:Input_format.t) (file:string) = match i with
  | Input_format.I_tptp -> parse_tptp file
  | Input_format.I_zf -> Util_zf.parse_file file
  | Input_format.I_tip -> parse_tip file
  | Input_format.I_dk -> parse_dk file
