
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test typing *)

open Logtk
open Logtk_arbitrary

module PT = TypedSTerm

let check_cmp =
  let gen = QCheck.(pair ArType.default ArType.default) in
  let name = "type_cmp_compatible_eq" in
  (* comparison of two types is 0 iff they are equal *)
  let prop (ty1, ty2) =
    let c = Type.compare ty1 ty2 in
    (c = 0) = (Type.equal ty1 ty2)
  in
  QCheck.Test.make ~name gen prop

let props =
  [ check_cmp
  ]
