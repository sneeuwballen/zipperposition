#use "topfind";;
#require "zarith";;
#require "sequence";;
#require "containers";;
#require "containers.data";;
#require "oclock";;
#require "unix";;
#require "bytes";;
#directory "_build/src/";;
#directory "_build/src/core";;
#directory "_build/src/parsers";;
#directory "_build/src/prover";;
#directory "_build/src/meta/";;
#directory "_build/src/arbitrary/";;
#directory "_build/tests/";;

#load "libzipperposition.cma";;
open Libzipperposition;;

module T = FOTerm;;
module PT = STerm;;
module TT = TypedSTerm;;
module Sym = ID;;
#install_printer ID.pp;;
#install_printer Type.pp;;
#install_printer FOTerm.pp;;
#install_printer PT.pp;;
#install_printer TT.pp;;
#install_printer HOTerm.pp;;
#install_printer Substs.pp;;
#install_printer Signature.pp;;
#install_printer ParseLocation.pp;;
#install_printer Precedence.pp;;
#install_printer Ordering.pp;;
module F = TT.Form ;;
module HOT = HOTerm;;

(* optional part: parser *)
#load "libzipperposition_parsers.cma";;
open Libzipperposition_parsers;;
#install_printer Ast_tptp.pp;;
#install_printer Ast_tptp.pp_general;;
#install_printer Ast_tptp.pp_role;;
let pterm s =
    let t = Parse_tptp.parse_term Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create () in
    let t' = TypeInference.infer_exn tyctx t in
    t'
;;
let pform s =
    let f = Parse_tptp.parse_formula Lex_tptp.token (Lexing.from_string s) in
    let tyctx = TypeInference.Ctx.create () in
    TypeInference.infer_prop_exn tyctx f
;;

CCFormat.set_color_default true;;

let ok () =
  Format.printf "...\t @{<Green>OK@}@.";
  exit 0;;

let fail msg = Format.printf "@[<2>...\t @{<Red>Error@}:@ %s@]@." msg

let failf msg = CCFormat.ksprintf msg ~f:fail

(* vim:syntax=ocaml
*)
