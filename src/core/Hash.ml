
(* This file is free software. See file "license" for more details. *)

type 'a t = 'a -> int

let bool b = if b then 1 else 2

let int i = i land max_int

let string (s:string) = Hashtbl.hash s

let combine f a b = Hashtbl.seeded_hash a (f b)

let combine2 a b = Hashtbl.seeded_hash a b

let combine3 a b c =
  combine2 a b
  |> combine2 c

let combine4 a b c d =
  combine2 a b
  |> combine2 c
  |> combine2 d

let combine5 a b c d e =
  combine2 a b
  |> combine2 c
  |> combine2 d
  |> combine2 e

let pair f g (x,y) = combine2 (f x) (g y)

let opt f = function
  | None -> 42
  | Some x -> combine2 43 (f x)

let list f l = List.fold_left (combine f) 0x42 l
let array f l = Array.fold_left (combine f) 0x42 l
let seq f seq =
  let h = ref 0x43 in
  seq (fun x -> h := combine f !h x);
  !h

let array_comm f a =
  let arr = Array.init (Array.length a) (fun i -> f a.(i)) in
  Array.sort CCInt.compare arr; (* sort the hashes, so their order does not matter *)
  array (fun h->h) arr

let list_comm f l =
  let a = Array.of_list l in
  array_comm f a

let poly x = Hashtbl.hash x
