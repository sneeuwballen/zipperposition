#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let t = HOT.(at_list (var ~ty:Type.(TPTP.i <== [TPTP.i;TPTP.o]) 0) [var
~ty:Type.TPTP.i 1; const ~ty:Type.TPTP.o ~< "b"]);;


ok ();;
