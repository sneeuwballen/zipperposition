
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable} *)

type 'a t = {
  id: int;
  ty: 'a;
}
type 'a hvar = 'a t

let id t = t.id
let ty t = t.ty

let make ~ty i =
  if i < 0 then invalid_arg "HVar.make";
  { id=i; ty; }

let cast v ~ty = {v with ty; }

let compare a b = CCOrd.int_ a.id b.id
let equal a b = a.id = b.id
let hash_fun a = CCHash.int a.id
let hash = CCHash.apply hash_fun

let pp out v = Format.fprintf out "v%d" v.id
let to_string = CCFormat.to_string pp

