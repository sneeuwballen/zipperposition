
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Generation of Types} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

let mk_ gen = QCheck.make ~print:Type.to_string ~shrink:Type.Seq.sub gen

let base_g = QCheck.Gen.oneofl Type.([term; prop; int; rat])
let base = mk_ base_g

let const_ s = Type.const (ID.make s)

let a_ = const_ "a"
let b_ = const_ "b"

let list_ = ID.make "list"
let prod_ = ID.make "prod"

let ground_g =
  let open QCheck.Gen in
  let base = oneofl Type.([ term; int; a_; b_ ]) in
  let g = fix
      (fun self n ->
         let sub = self (n-1) in
         if n<=0 then base
         else frequency
             [ 1, map (Type.app list_) (list_repeat 1 sub)
             ; 1, map (Type.app prod_) (list_repeat 2 sub)
             ; 1, map2 Type.arrow (list_size (1--2) sub) sub
             ; 3, base
             ])
  in
  1 -- 4 >>= g

let ground = mk_ ground_g

let default_g =
  let open QCheck.Gen in
  let var = oneofl [Type.var_of_int 0; Type.var_of_int 1 ] in
  let base =
    oneof
      [ oneofl [ Type.term; Type.int; a_; b_ ]
      ; var ]
  in
  let g =
    fix
      (fun self n ->
         let sub = self (n-1) in
         if n<=0 then base
         else frequency
             [ 1, map (Type.app list_) (list_repeat 1 sub)
             ; 1, map (Type.app prod_) (list_repeat 2 sub)
             ; 1, map2 Type.arrow (list_size (1--2) sub) sub
             ; 3, base
             ])
  in
  1 -- 4 >>= g

let default = mk_ default_g
