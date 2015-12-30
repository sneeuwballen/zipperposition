#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let ty1 = Type.(
  record_flatten ["x", term]
  ~rest:(Some
    (record ["y", prop] ~rest:(Some (HVar.make ~ty:tType 0))))) ;;
let ty2 = Type.(
  record ["x", term; "y", prop]
  ~rest:(Some (HVar.make ~ty:tType 0)));;

assert (Type.equal ty1 ty2);;

ok ();;
