
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Parsing} *)

open Libzipperposition
open CCError.Infix

let parse_tptp file =
  Util_tptp.parse_file ~recursive:true file
  >|= Sequence.map Util_tptp.to_ast

(** Parse file using the input format chosen by the user *)
let parse file = match !Options.input with
  | Options.I_tptp -> parse_tptp file
  | Options.I_zf -> Util_zf.parse_file file
  | Options.I_guess ->
      if CCString.suffix ~suf:".p" file
      then parse_tptp file
      else Util_zf.parse_file file
