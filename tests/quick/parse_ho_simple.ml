#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

open Logtk_parsers;;

let l = Lex_ho.decls_of_string
  "p X ?y <-
      axiom (foo (bar yolo) Y),
      hello [world].

   istrue (ilove Records) <-
      foo {x=yay, y=Y | Rest_here}.

   val yolo : swag.";;

let () =
  match l with
  | `Ok l -> assert (List.length l = 3)
  | `Error msg ->
      failwith (Printf.sprintf "error : %s\n" msg)
;;

ok ();;

