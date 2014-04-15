#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let ctx = TypeInference.Ctx.create
  (Signature.of_list
    [~< "f",
    Type.(TPTP.i <== [record ["x", TPTP.int; "y", TPTP.o] ~rest:(Some (var 1));
                      TPTP.int ])]);;

let t = PT.app
  (PT.const ~< "f")
    [ PT.record ["x", PT.of_int 42] ~rest:(Some (PT.var "R"));
      PT.var "X"];;

(* tough type inference *)
let t' = TypeInference.HO.convert ~ret:Type.TPTP.i ~generalize:false ~ctx t;;

(* tougher type inference *)
TypeInference.Ctx.clear ctx;;
let t'' = TypeInference.HO.convert ~ret:Type.TPTP.i ~generalize:true ~ctx t;;

ok ();;
