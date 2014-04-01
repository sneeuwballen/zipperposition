
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

(** test partial order *)

open Logtk
open QCheck

module PO = PartialOrder.Make(struct
  type t = int
  let eq i j = i = j
  let hash i = i
end)

(* arbitrary po *)
let ar_po = Arbitrary.(
  list ~len:(1--10) small_int >>= fun l ->
  let po = PO.create l in
  list (pair (among l) (among l)) >>= fun pairs ->
  PO.enrich po
    (fun i j ->
      if List.mem (i,j) pairs then Comparison.Gt
      else if List.mem (j,i) pairs then Comparison.Lt
      else Comparison.Incomparable);
  return po)
    

(* check that complete gives a total order *)
let check_complete =
  let gen = ar_po in
  let prop po =
    PO.complete po (fun i j -> i-j);
    PO.is_total po
  in
  let pp = PP.(fun po -> list int (PO.elements po)) in
  let size = PO.size in
  let name = "partial_order_complete_gives_total_order" in
  mk_test ~size ~pp ~name gen prop

(* check that completion gives an order that is the same as function *)
let check_complete_same =
  let gen = ar_po in
  let cmp i j = i - j in
  let prop po =
    PO.complete po cmp;
    List.for_all2
      (fun x y -> match PO.compare po x y with
        | Comparison.Eq
        | Comparison.Incomparable -> cmp x y = 0
        | Comparison.Lt -> cmp x y < 0
        | Comparison.Gt -> cmp x y > 0)

      (PO.elements po) (PO.elements po)
  in
  let name = "partial_order_complete_gives_same_order" in
  mk_test ~name gen prop

(* check that extension gives same order on old elements *)
let check_extend =
  let gen = Arbitrary.(pair ar_po (list small_int)) in
  let prop (po, elts) =
    let po' = PO.extend po elts in
    List.for_all2
      (fun x y -> PO.compare po x y = PO.compare po' x y)
      (PO.elements po) (PO.elements po)
  in
  let name = "partial_order_extend_preserves_old" in
  mk_test ~name gen prop

(* check that after PO.complete called, elements is a sorted list *)
let check_elements_sorted =
  let gen = Arbitrary.(list small_int) in
  let cmp i j = i - j in
  let rec sorted l = match l with
    | [] | [_] -> true
    | x::((y::_) as l') -> x >= y && sorted l'
  in
  let prop l =
    let po = PO.create l in
    PO.complete po cmp;
    sorted (PO.elements po)
  in
  let pp = PP.(list int) in
  let size = List.length in
  let name = "partial_order_elements_sorted_after_completion" in
  mk_test ~pp ~size ~name gen prop
  
let props =
  [ check_complete
  ; check_complete_same
  ; check_extend
  ; check_elements_sorted
  ]

