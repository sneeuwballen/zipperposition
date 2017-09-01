
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Immutable Arrays} *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int

type 'a t = 'a array

let of_list = Array.of_list

let to_list = Array.to_list

let to_array_copy = Array.copy

let to_array_unsafe a = a
let of_array_unsafe a = a (* bleh. *)

let empty = [| |]

let length = Array.length

let singleton x = [| x |]

let doubleton x y = [| x; y |]

let make n x = Array.make n x

let init n f = Array.init n f

let get = Array.get

let set a n x =
  let a' = Array.copy a in
  a'.(n) <- x;
  a'

let map = Array.map
let map_arr = Array.map

let mapi = Array.mapi
let mapi_arr = Array.mapi

let append a b =
  let na = length a in
  Array.init (na + length b)
    (fun i -> if i < na then a.(i) else b.(i-na))

let iter = Array.iter

let iteri = Array.iteri

let fold = Array.fold_left

let foldi f acc a =
  let n = ref 0 in
  Array.fold_left
    (fun acc x ->
       let acc = f acc !n x in
       incr n;
       acc)
    acc a

exception ExitNow

let for_all p a =
  try
    Array.iter (fun x -> if not (p x) then raise ExitNow) a;
    true
  with ExitNow -> false

let exists p a =
  try
    Array.iter (fun x -> if p x then raise ExitNow) a;
    false
  with ExitNow -> true

let equal eq a b =
  let rec aux i =
    if i = Array.length a then true
    else eq a.(i) b.(i) && aux (i+1)
  in
  Array.length a = Array.length b
  &&
  aux 0

let compare cmp a b =
  let rec aux i =
    if i = Array.length a
    then if i = Array.length b then 0 else -1
    else if i = Array.length b
    then 1
    else
      let c = cmp a.(i) b.(i) in
      if c = 0 then aux (i+1) else c
  in
  aux 0

let to_seq a k = iter k a
let to_seqi a k = iteri (fun i x -> k(i,x)) a

let of_seq s =
  let l = ref [] in
  s (fun x -> l := x :: !l);
  Array.of_list (List.rev !l)

let hash f a = Hash.array f a

let hash_comm f a =
  let arr = Array.init (Array.length a) (fun i -> f a.(i)) in
  Array.sort CCInt.compare arr; (* sort the hashes, so their order does not matter *)
  Hash.array (fun h->h) arr
