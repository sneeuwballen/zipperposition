#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let s = Util_tptp.parse_file ~recursive:true "pelletier_problems/pb47.p";;
let signature, s' = Util_tptp.infer_types (`sign Signature.empty) s;;
let forms = Util_tptp.Typed.formulas s';;
assert (Sequence.for_all F.FO.is_closed forms);;

print_endline "... OK";;
