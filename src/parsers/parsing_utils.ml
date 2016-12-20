
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Parsing} *)

open Libzipperposition
open CCError.Infix

let parse_tptp file =
  Util_tptp.parse_file ~recursive:true file
  >|= Sequence.map Util_tptp.to_ast

let parse_tip file =
  Util_tip.parse_file file
  >|= Util_tip.convert_seq

(** Parse file using the input format chosen by the user *)
let parse file = match !Options.input with
  | Options.I_tptp -> parse_tptp file
  | Options.I_zf -> Util_zf.parse_file file
  | Options.I_tip -> parse_tip file
  | Options.I_guess ->
      if CCString.suffix ~suf:".p" file
      then parse_tptp file
      else if CCString.suffix ~suf:".smt2" file
      then parse_tip file
      else if CCString.suffix ~suf:".zf" file
      then Util_zf.parse_file file
      else failwith ("unable to guess syntax for " ^ file)
