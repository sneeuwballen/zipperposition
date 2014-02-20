#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let ty1 = Type.(record ["x", TPTP.i] ~rest:(Some (record ["y", TPTP.o] ~rest:(Some (var 0))))) ;;
let ty2 = Type.(record ["x", TPTP.i; "y", TPTP.o] ~rest:(Some (var 0)));;

assert (Type.eq ty1 ty2);;

print_endline "... OK";;
