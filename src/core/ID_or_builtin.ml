
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 ID or Builtin} *)

type t =
  | I of ID.t
  | B of Builtin.t

let compare a b = match a,b with
  | I a, I b -> ID.compare a b
  | B a, B b -> Builtin.compare a b
  | I _, B _ -> 1 (* id > builtin *)
  | B _, I _ -> -1

let equal a b = compare a b = 0

let pp out = function
  | I id -> ID.pp out id
  | B b -> Builtin.pp out b

let to_string = CCFormat.to_string pp


module As_key = struct
  type t_ = t
  type t = t_
  let compare = compare
end

module Map = CCMap.Make(As_key)
