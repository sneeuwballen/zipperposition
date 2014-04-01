#use "topfind";;
#require "zarith";;
#require "num";;
#require "unix";;
#require "datalog";;
#require "str";;
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
#install_printer Symbol.fmt;;
#install_printer Type.fmt;;
#install_printer FOTerm.fmt;;
#install_printer PrologTerm.fmt;;
#install_printer Formula.FO.fmt;;
#install_printer HOTerm.fmt;;
#install_printer Substs.fmt;;
#install_printer Signature.fmt;;
#install_printer ParseLocation.fmt;;
#install_printer Precedence.fmt;;
#install_printer Ordering.fmt;;
open Logtk.Sequence.Infix;;
open Logtk.Type;;
module F = Logtk.Formula;;
module HOT = Logtk.HOTerm;;

(* optional part: parser *)
#load "logtk_parsers.cma";;
open Logtk_parsers;;
#install_printer Ast_tptp.Untyped.fmt;;
#install_printer Ast_tptp.Typed.fmt;;
#install_printer Ast_tptp.fmt_general;;
#install_printer Ast_tptp.fmt_role;;
let pterm s =
    let t = Parse_tptp.parse_term Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create Signature.TPTP.base in
    let _, clos = TypeInference.FO.infer tyctx t in
    clos tyctx
;;
let pform s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create Signature.TPTP.base in
    TypeInference.FO.convert_form ~ctx:tyctx f
;;


(* vim:syntax=ocaml
*)
