
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Value} *)

type +'a t = {
  value: 'a;
  scope: int;
}

let make x i = { value=x; scope=i; }

let get v = v.value
let scope v = v.scope

let set v x = {v with value=x; }

let same_scope v1 v2 = v1.scope = v2.scope

let on f v = f v.value
let on2 f v1 v2 = f v1.value v2.value

let map f v = {v with value = f v.value; }

let equal eq v1 v2 = v1.scope = v2.scope && eq v1.value v2.value
let compare c v1 v2 =
  if v1.scope = v2.scope then c v1.value v2.value
  else Pervasives.compare v1.scope v2.scope

let hash_fun f v h = CCHash.int v.scope h |> f v.value
let hash f v = CCHash.apply (hash_fun f) v

let pp p out v =
  Format.fprintf out "@[%a[%d]@]" p v.value v.scope

let to_string p v = CCFormat.to_string (pp p) v

