
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Literal} *)

type t = int
type lit = t

let hash i = i land max_int
let hash_fun i h = CCHash.int (hash i) h
let equal (i:t) j = i=j
let compare = CCInt.compare
let neg i = -i
let sign i = i>0
let abs = Pervasives.abs
let set_sign b i = if b then abs i else - (abs i)
let apply_sign b i = if b then i else neg i
let make i = assert (i<>0); i
let pp out i = Format.fprintf out "%s%d" (if sign i then "" else "¬") i

module AsKey = struct
  type t = lit
  let compare = compare
end

module Set = CCSet.Make(AsKey)

