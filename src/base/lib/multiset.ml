
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {6 Generic multisets} *)

type 'a t = 'a array
  (** We represent multisets as arrays. *)

let create = Array.of_list

let create_a a = a

let size a = Array.length a

let is_empty a = Array.length a = 0

let iter a f = Array.iter f a

(* returns a pair of bitvectors that are only true for elements of the
   corresponding multisets that have not been removed. An element is removed is
   it occurs also in the other multiset. *)
let remove_eq f m1 m2 =
  (* find an element of [m2] that can remove [i] from [bv1], On success
      remove elements from [m1] and [m2] using [bv1] and [bv2]. *)
  let rec find_mate f m1 m2 bv1 bv2 i j =
    if j = size m2
      then ()
    else if BV.get bv2 j && f m1.(i) m2.(j) = Comparison.Eq
      then (BV.reset bv1 i; BV.reset bv2 j)  (* remove both *)
    else find_mate f m1 m2 bv1 bv2 i (j+1)
  in
  let bv1 = BV.create ~size:(size m1) true in
  let bv2 = BV.create ~size:(size m2) true in
  for i = 0 to size m1 - 1 do
    if BV.get bv1 i then find_mate f m1 m2 bv1 bv2 i 0
  done;
  bv1, bv2

let eq f m1 m2 =
  size m1 = size m2 &&
  (* could be optimized by failing as soon as some element of [m1] has
     not been removed by [remove_eq]... *)
  let bv1, bv2 = remove_eq f m1 m2 in
  BV.is_empty bv1 && BV.is_empty bv2

let compare f m1 m2 =
  (* here, bitvectors are be used to determine whether
    an element can be maximal, ie if it dominates every element of the
    other set. *)
  let rec compare_all f m1 m2 bv1 bv2 i j =
    if i = size m1
      then ()
    else if j = size m2
      then compare_all f m1 m2 bv1 bv2 (i+1) 0
    else if BV.get bv1 i && BV.get bv2 j then
      (* compare m1(i) and m2(j). The loser, if any, is removed
         from the set of candidates for maximality *)
      let _ = match f m1.(i) m2.(j) with
      | Comparison.Eq -> assert false  (* remove equal failed?*)
      | Comparison.Incomparable -> ()
      | Comparison.Gt -> BV.reset bv2 j
      | Comparison.Lt -> BV.reset bv1 i
      in
      compare_all f m1 m2 bv1 bv2 i (j+1)
    else compare_all f m1 m2 bv1 bv2 i (j+1)
  in
  let bv1, bv2 = remove_eq f m1 m2 in
  if BV.is_empty bv1 && BV.is_empty bv2
    then Comparison.Eq
  else begin
    compare_all f m1 m2 bv1 bv2 0 0;
    match BV.cardinal bv1, BV.cardinal bv2 with
    | 0, 0 -> assert false    (* inconsistent: where is the dominating elemt? *)
    | 0, _ -> Comparison.Lt   (* m2 has at least one maximal element *)
    | _, 0 -> Comparison.Gt
    | _, _ -> Comparison.Incomparable  (* both have maximal elements *)
  end

let is_max f x m =
  Util.array_forall (fun y -> f x y <> Comparison.Lt) m

(* maximal elements *)
let max f m =
  (* at the beginning, all literals are potentially maximal *)
  let bv = BV.create ~size:(size m) true in
  for i = 0 to size m - 1 do
    if not (BV.get bv i)
      then ()
      else for j = i+1 to size m - 1 do
        if i = j || not (BV.get bv j)
          then ()
          else match f m.(i) m.(j) with
            | Comparison.Eq
            | Comparison.Incomparable -> ()
            | Comparison.Lt -> BV.reset bv i  (* i smaller, cannot be max *)
            | Comparison.Gt -> BV.reset bv j  (* j smaller, cannot be max *)
      done
  done;
  bv

let get m i = m.(i)

let to_array m = m

let to_list = Array.to_list
