
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable} *)

type +'a t = {
  id: int;
  ty: 'a;
}
type 'a hvar = 'a t

let[@inline] id t = t.id
let[@inline] ty t = t.ty

let[@inline] make ~ty i =
  if i < 0 then invalid_arg "HVar.make";
  { id=i; ty; }

let[@inline] cast v ~ty = {v with ty; }
let[@inline] update_ty v ~f = {v with ty=f v.ty; }

let[@inline] compare cmp a b =
  let c = CCOrd.int a.id b.id  in
  if c<>0 then c else cmp a.ty b.ty
let[@inline] equal eq a b = a.id = b.id && eq a.ty b.ty
let[@inline] hash a = Hash.int a.id

let[@inline] max a b = if a.id < b.id then b else a
let[@inline] min a b = if a.id < b.id then a else b

let pp out v = Format.fprintf out "X%d" v.id
let pp_tstp = pp
let to_string v = CCFormat.to_string pp v
let to_string_tstp = to_string

let[@inline] make_unsafe ~ty id = {ty; id; }

let fresh_ = ref ~-1
let[@inline] fresh ~ty () =
  if !fresh_ > 0 then failwith "HVar.fresh_var: underflow";
  let v = make_unsafe ~ty !fresh_ in
  decr fresh_;
  v

let[@inline] is_fresh v = id v < 0
