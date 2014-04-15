
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

type 'a t = 'a IArray.t
  (** We represent multisets as arrays. *)

let of_list = IArray.of_list

let create a = a

let create_unsafe = IArray.of_array_unsafe

let size a = IArray.length a

let is_empty a = IArray.length a = 0

let iter a f = IArray.iter f a

let get = IArray.get

(* returns a pair of bitvectors that are only true for elements of the
   corresponding multisets that have not been removed. An element is removed is
   it occurs also in the other multiset. *)
let remove_eq f m1 m2 =
  (* find an element of [m2] that can remove [i] from [bv1], On success
      remove elements from [m1] and [m2] using [bv1] and [bv2]. *)
  let rec find_mate f m1 m2 bv1 bv2 i j =
    if j = size m2
      then ()
    else if BV.get bv2 j && f (IArray.get m1 i) (IArray.get m2 j) = Comparison.Eq
      then (BV.reset bv1 i; BV.reset bv2 j)  (* remove both *)
    else find_mate f m1 m2 bv1 bv2 i (j+1)
  in
  let bv1 = BV.create ~size:(size m1) true in
  let bv2 = BV.create ~size:(size m2) true in
  for i = 0 to size m1 - 1 do
    if BV.get bv1 i then find_mate f m1 m2 bv1 bv2 i 0
  done;
  bv1, bv2

let to_array m = m

let to_list = IArray.to_list

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
      let _ = match f (IArray.get m1 i) (IArray.get m2 j) with
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
  IArray.for_all (fun y -> f x y <> Comparison.Lt) m

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
          else match f (IArray.get m i) (IArray.get m j) with
            | Comparison.Eq
            | Comparison.Incomparable -> ()
            | Comparison.Lt -> BV.reset bv i  (* i smaller, cannot be max *)
            | Comparison.Gt -> BV.reset bv j  (* j smaller, cannot be max *)
      done
  done;
  bv

(* maximal elements of a list *)
let max_l f l =
  (* check whether [x] can be max.
     [ok]: some max terms; [x]: current element;
     [rest1]: to process but still candidates, [rest2]: remaining to compare.
     It always holds that elements of [ok] and
     [rest1 @ rest2] are incomparable or equal. *)
  let rec check_max ok x rest1 rest2 =
    match rest2 with
    | [] -> start (x::ok) rest1 (* [x] is maximal, next! *)
    | y :: rest2' ->
        begin match f x y with
        | Comparison.Gt -> check_max ok x rest1 rest2'  (* y can't be maximal *)
        | Comparison.Lt -> start ok (rest1 @ rest2)  (* drop x, can't be max *)
        | Comparison.Eq
        | Comparison.Incomparable ->
            (* keep [y] and [x], both can still be maximal *)
            check_max ok x (y::rest1) rest2'
        end
  (* add maximal elements among [rest] to [ok]. *)
  and start ok rest = match rest with
    | [] -> ok
    | x :: rest' -> check_max ok x [] rest'
  in
  start [] l

(* for now, simple solution. *)
let compare_l f l1 l2 =
  compare f (IArray.of_list l1) (IArray.of_list l2)
