
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unique Identifiers} *)

type t = {
  id: int;
  name: string;
}
type t_ = t

let make =
  let n = ref 0 in
  fun name ->
    let id = !n in
    incr n;
    {id; name; }

let copy t = make t.name

let hash t = t.id
let hash_fun t h = CCHash.int t.id h
let equal i1 i2 = i1.id = i2.id
let compare i1 i2 = Pervasives.compare i1.id i2.id

let pp out id = CCFormat.string out id.name
let to_string = CCFormat.to_string pp

let pp_full out id = Format.fprintf out "%s/%d" id.name id.id

let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name = if i=0
      then CCPrint.sprintf "'_%c" names.[j]
      else CCPrint.sprintf "'_%c%d" names.[j] i
    in
    incr r;
    name

module O_ = struct
  type t = t_
  let equal = equal
  let compare = compare
  let hash = hash
end

module Map = CCMap.Make(O_)
module Set = CCSet.Make(O_)
module Tbl = CCHashtbl.Make(O_)
