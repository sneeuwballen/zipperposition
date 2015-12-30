#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let t =
  let open HOT in
  app
    (var_of_int ~ty:Type.([term; prop] ==> term)  0)
    [ var_of_int ~ty:Type.term 1
    ; const ~ty:Type.prop (ID.make "b")]
;;

ok ();;
