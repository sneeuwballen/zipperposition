
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary generation of symbols} *)

open Libzipperposition
open QCheck

type 'a arbitrary = 'a QCheck.Arbitrary.t

let base =
  Arbitrary.(among Type.([term; prop; int; rat]))

let const_ s = Type.const (ID.make s)

let a_ = const_ "a"
let b_ = const_ "b"

let list_ = ID.make "list"
let prod_ = ID.make "prod"

let ground =
  Arbitrary.(
    let base = among Type.([ term; int; a_; b_ ]) in
    fix ~max:3 ~base
      (fun sub -> choose
        [ lift (Type.app list_) (list_repeat 1 sub)
        ; lift (Type.app prod_) (list_repeat 2 sub)
        ; lift2 Type.arrow (list sub) sub
        ]))

let default =
  Arbitrary.(
    let var = among [Type.var_of_int 0; Type.var_of_int 1 ] in
    let base =
      choose
      [ among [ Type.term; Type.int; a_; b_ ]
      ; var ]
    in
    fix ~max:4 ~base (fun sub -> choose
      [ lift (Type.app list_) (list_repeat 1 sub)
      ; lift (Type.app prod_) (list_repeat 2 sub)
      ; lift2 Type.arrow (list sub) sub
      ]))

