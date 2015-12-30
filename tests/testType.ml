
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test typing *)

open Libzipperposition
open Libzipperposition_arbitrary
open QCheck

module PT = TypedSTerm

let check_cmp =
  let gen = Arbitrary.(pair ArType.default ArType.default) in
  let name = "type_cmp_compatible_eq" in
  let pp = PP.(pair Type.to_string Type.to_string) in
  let size (ty1, ty2) = Type.size ty1 + Type.size ty2 in
  (* comparison of two types is 0 iff they are equal *)
  let prop (ty1, ty2) =
    let c = Type.compare ty1 ty2 in
    (c = 0) = (Type.equal ty1 ty2)
  in
  mk_test ~name ~pp ~size gen prop

let props =
  [ check_cmp
  ]
