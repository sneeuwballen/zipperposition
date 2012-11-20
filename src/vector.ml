(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Growable, mutable vector *)

(** a vector of 'a. *)
type 'a vector = {
  mutable size : int;
  mutable capacity : int;
  mutable vec : 'a array;
}

let create i =
  assert (i > 0);
  { size = 0;
    capacity = i;
    vec = Array.create i (Obj.magic None);
  }

(** resize the underlying array so that it can contains the
    given number of elements *)
let resize v newcapacity =
  assert (newcapacity > v.capacity);
  let new_vec = Array.create newcapacity (Obj.magic None) in
  Array.blit v.vec 0 new_vec 0 v.size;
  v.vec <- new_vec;
  v.capacity <- newcapacity

let clear v =
  v.size <- 0;
  if v.capacity > 1000  (* shrink if too large *)
    then (v.capacity <- 10;
          v.vec <- Array.create i (Obj.magic None))

let push v x =
  (if v.capacity = v.size
    then resize v (2 * v.capacity));
  v.vec.(v.size) <- x;
  v.size <- v.size + 1

let pop v =
  (if v.size = 0 then failwith "Vector.pop on empty vector");
  v.size <- v.size - 1;
  let x = v.vec.(v.size) in
  x

let shrink v n =
  if n <= v.size then () else v.size <- n

val iter v k =
  for i = 0 to v.size -1 do
    k v.vec.(i)
  done

val iteri v k =
  for i = 0 to v.size -1 do
    k i v.vec.(i)
  done

val map v f =
  let v' = create v.size in
  for i = 0 to v.size - 1 do
    push v' (f (get v i));
  done;
  v'

let fold v acc f =
  let acc = ref acc in
  for i = 0 to v.size - 1 do
    acc := f acc (get v i);
  done;
  !acc

let get v i =
  (if i < 0 || i >= v.size then failwith "wrong index for vector");
  v.vec.(i)

let set v i x =
  (if i < 0 || i >= v.size then failwith "wrong index for vector");
  v.vec.(i) <- x

let size v = v.size

let from_array a =
  let c = Array.length a in
  let v = create c in
  for i = 0 to c-1 do
    push v a.(i);
  done;
  v

let from_list l =
  let v = Array.create 10 in
  let rec add l = match l with
  | [] -> v
  | x::l' -> (push v x; add l')
  in add l

let to_array v =
  let a = Array.create v.size (Obj.magic None) in
  Array.blit v.vec 0 a 0 v.size;
  a

let to_list v =
  let l = ref [] in
  for i = 0 to v.size - 1 do
    l = get v i :: !l;
  done;
  List.rev !l
