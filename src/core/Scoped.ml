
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Value} *)

type scope = int
type +'a t = 'a * scope

let[@inline] make x i : _ t = x, i

let[@inline] get ((x,_): _ t) = x
let[@inline] scope ((_,sc):_ t) = sc

let[@inline] set (_,s) x : _ t = x, s

let[@inline] same_scope (v1:_ t) (v2:_ t) : bool = scope v1 = scope v2

let on f v = f (get v)
let on2 f v1 v2 = f (get v1) (get v2)
let map f (v,i) : _ t = (f v, i)

let[@inline] equal eq (v1:_ t) (v2:_ t) : bool = scope v1 = scope v2 && eq (get v1) (get v2)
let compare c v1 v2 =
  if scope v1 = scope v2 then c (get v1) (get v2)
  else Stdlib.compare (scope v1) (scope v2)

let hash f (v,sc) = Hash.combine2 (Hash.int sc) (f v)

let pp p out v =
  Format.fprintf out "@[%a[%d]@]" p (get v) (scope v)

let to_string p v = CCFormat.to_string (pp p) v

