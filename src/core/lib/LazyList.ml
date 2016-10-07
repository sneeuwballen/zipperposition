
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lazy List} *)

type 'a node =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a node Lazy.t

let nil = Lazy.from_val Nil
let cons a b = Lazy.from_val (Cons (a, b))

let of_fun f =
  let rec aux i = lazy (match f i with
    | None -> Nil
    | Some x -> Cons (x, aux (i+1)))
  in
  aux 0

let rec take n l =
  if n=0 then nil
  else lazy (
    match l with
      | lazy Nil -> Nil
      | lazy (Cons (x,tl)) -> Cons (x, take (n-1) tl)
  )

let rec to_list = function
  | lazy Nil -> []
  | lazy (Cons (x, f)) -> x :: to_list f

let rec to_seq res k = match res with
  | lazy Nil -> ()
  | lazy (Cons (s, f)) -> k s; to_seq f k

let rec fold f acc res = match res with
  | lazy Nil -> acc
  | lazy (Cons (s, cont)) -> fold f (f acc s) cont

