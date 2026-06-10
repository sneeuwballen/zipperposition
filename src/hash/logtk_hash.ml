(* This file is free software. See file "license" for more details. *)

(** {1 Hash combinators} *)

type hash = int
type 'a t = 'a -> hash
type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let[@inline] combine2 a b =
  Hash_impl.(finalize (combine_int (combine_int seed a) b))

let[@inline] combine f s x =
  Hash_impl.(finalize (combine_int (combine_int seed s) (f x)))

let combine3 a b c =
  Hash_impl.(
    let s = combine_int (combine_int seed a) b in
    finalize (combine_int s c))

let combine4 a b c d =
  Hash_impl.(
    let s = combine_int (combine_int seed a) b in
    let s = combine_int s c in
    finalize (combine_int s d))

let combine5 a b c d e =
  Hash_impl.(
    let s = combine_int (combine_int seed a) b in
    let s = combine_int s c in
    let s = combine_int s d in
    finalize (combine_int s e))

let combine6 a b c d e f =
  Hash_impl.(
    let s = combine_int (combine_int seed a) b in
    let s = combine_int s c in
    let s = combine_int s d in
    let s = combine_int s e in
    finalize (combine_int s f))

(** {2 Primitive hashers} *)

let const h _ = h
let const0 _ = 0
let int n = Hash_impl.(finalize (combine_int seed n))

let bool b =
  int
    (if b then
       1
     else
       2)

let char x = Hash_impl.(finalize (combine_char seed (Char.code x)))
let int64 (n : int64) : int = Hash_impl.(finalize (combine_i64 seed n))
let int32 (x : int32) : int = Hash_impl.(finalize (combine_i32 seed x))

let nativeint (x : nativeint) =
  Hash_impl.(finalize (combine_i64 seed (Int64.of_nativeint x)))

let bytes (x : bytes) =
  Hash_impl.(finalize (combine_string seed (Bytes.unsafe_to_string x)))

let string (x : string) = Hash_impl.(finalize (combine_string seed x))

let slice x i len =
  let j = i + len in
  let rec aux k s =
    if k = j then
      Hash_impl.finalize s
    else
      aux (k + 1) (Hash_impl.combine_char s (Char.code (String.unsafe_get x k)))
  in
  aux i Hash_impl.seed

let opt f = function
  | None -> 42
  | Some x -> Hash_impl.(finalize (combine_int (combine_int seed 43) (f x)))

let list f l =
  let s =
    List.fold_left (fun s x -> Hash_impl.combine_int s (f x)) Hash_impl.seed l
  in
  Hash_impl.finalize s

let array f a =
  let s =
    Array.fold_left (fun s x -> Hash_impl.combine_int s (f x)) Hash_impl.seed a
  in
  Hash_impl.finalize s

let pair f g (x, y) =
  Hash_impl.(finalize (combine_int (combine_int seed (f x)) (g y)))

let poly = Hashtbl.hash

let triple f g h (x, y, z) =
  Hash_impl.(
    let s = combine_int seed (f x) in
    let s = combine_int s (g y) in
    finalize (combine_int s (h z)))

let quad f g h i (x, y, z, w) =
  Hash_impl.(
    let s = combine_int seed (f x) in
    let s = combine_int s (g y) in
    let s = combine_int s (h z) in
    finalize (combine_int s (i w)))

let map f h x = h (f x)

let iter f seq =
  let s = ref Hash_impl.seed in
  seq (fun x -> s := Hash_impl.combine_int !s (f x));
  Hash_impl.finalize !s

let seq f sq = iter f sq

let gen f g =
  let rec aux s =
    match g () with
    | None -> Hash_impl.finalize s
    | Some x -> aux (Hash_impl.combine_int s (f x))
  in
  aux Hash_impl.seed
