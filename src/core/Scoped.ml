
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Value} *)

type scope = int
type +'a t = 'a * scope

let make x i = x, i

let get = fst
let scope = snd

let set (_,s) x = x, s

let same_scope v1 v2 = scope v1 = scope v2

let on f v = f (get v)
let on2 f v1 v2 = f (get v1) (get v2)

let map f (v,i) = (f v, i)

let equal eq v1 v2 = scope v1 = scope v2 && eq (get v1) (get v2)
let compare c v1 v2 =
  if scope v1 = scope v2 then c (get v1) (get v2)
  else Pervasives.compare (scope v1) (scope v2)

let hash f (v,sc) = Hash.combine2 (Hash.int sc) (f v)

let pp p out v =
  Format.fprintf out "@[%a[%d]@]" p (get v) (scope v)

let to_string p v = CCFormat.to_string (pp p) v

