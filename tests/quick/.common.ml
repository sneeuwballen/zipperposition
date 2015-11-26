#use "topfind";;
#require "zarith";;
#require "sequence";;
#require "containers";;
#require "containers.data";;
#require "num";;
#require "unix";;
#require "bytes";;
#directory "_build/src/";;
#directory "_build/src/base";;
#directory "_build/src/extended";;
#directory "_build/src/parsers";;
#directory "_build/src/meta/";;
#directory "_build/src/arbitrary/";;
#directory "_build/tests/";;

#load "logtk.cma";;
open Logtk;;

module ST = ScopedTerm;;
module T = FOTerm;;
module PT = PrologTerm;;
module Sym = Symbol;;
let (~<) = Symbol.of_string;;
#install_printer Symbol.pp;;
#install_printer Type.pp;;
#install_printer FOTerm.pp;;
#install_printer PrologTerm.pp;;
#install_printer Formula.FO.pp;;
#install_printer HOTerm.pp;;
#install_printer Substs.pp;;
#install_printer Signature.pp;;
#install_printer ParseLocation.pp;;
#install_printer Precedence.pp;;
#install_printer Ordering.pp;;
open Sequence.Infix;;
open Logtk.Type;;
module F = Logtk.Formula;;
module HOT = Logtk.HOTerm;;

(* optional part: parser *)
#load "logtk_parsers.cma";;
open Logtk_parsers;;
#install_printer Ast_tptp.Untyped.pp;;
#install_printer Ast_tptp.Typed.pp;;
#install_printer Ast_tptp.pp_general;;
#install_printer Ast_tptp.pp_role;;
let pterm s =
    let t = Parse_tptp.parse_term Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create Signature.TPTP.base in
    let _, clos = TypeInference.FO.infer_exn tyctx t in
    clos tyctx
;;
let pform s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create Signature.TPTP.base in
    TypeInference.FO.convert_form_exn ~ctx:tyctx f
;;


let ok () =
  print_endline "... OK";
  exit 0;;

(* vim:syntax=ocaml
*)
