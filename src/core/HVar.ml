
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsed Variable} *)

type t = int

type hvar = t

let id i = i
let make i =
  if i < 0 then invalid_arg "HVar.make";
  i

let compare = CCOrd.int_
let equal (i:t) j = i=j
let hash_fun = CCHash.int
let hash = CCHash.apply hash_fun

module As_key = struct type t = hvar let compare = compare end
module Set = CCSet.Make(As_key)
module Map = CCMap.Make(As_key)


