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

(** {1 Partial Ordering on symbols} *)

module type S = sig
  type elt
    (** Elements that can be compared *)

  type t
    (** the partial order on elements of type [elt] *)

  val create : elt list -> t
    (** build an empty partial order for the list of elements *)

  val copy : t -> t
    (** Copy of the partial order *)

  val extend : t -> elt list -> t
    (** Add new elements to the ordering, creating a new ordering.
        They will not be ordered at all w.r.t previous elements. *)

  val is_total : t -> bool
    (** Is the ordering total (i.e. each pair of elements it contains
        is ordered)? *)

  val enrich : t -> (elt -> elt -> Comparison.t) -> unit
    (** Compare unordered pairs with the given partial order function.
        If the function returns {!Comparison.Eq} on two elements [x] and
        [y], then the ordering cannot be total anymore. *)

  val complete : t -> (elt -> elt -> int) -> unit
    (** [complete po f] completes [po] using the function [f]
        elements to compare still unordered pairs. If [f x y] returns 0
        then [x] and [y] are still incomparable in [po] afterwards.
        If the given comparison function is not total, the ordering may still
        not be complete. The comparison function [f] is assumed to be such
        that [transitive_closure f] is a partial order. *)

  val compare : t -> elt -> elt -> Comparison.t
    (** compare two elements in the ordering. *)

  val elements : t -> elt list
    (** Elements of the partial order. If the ordering is total,
        they will be sorted by decreasing order (maximum first) *)
end

(** {2 Helper: boolean matrix}

The matrix is *)

module BoolMatrix = struct
  type t = {
    bv : BV.t; (* bitvector *)
    line : int; (* number of lines *)
    column : int; (* number of columns *)
  }

  (* matrix of size (line,column) *)
  let create line column =
    let n = line * column in
    let bv = BV.create ~size:n false in
    { bv; line; column; }

  let copy m = { m with bv = BV.copy m.bv; }

  (* index of m[i,j]. [i] is the line number. *)
  let _idx m i j =
    m.column * i + j

  let set m i j =
    BV.set m.bv (_idx m i j)

  let get m i j =
    BV.get m.bv (_idx m i j)

  (* assuming the dimensions of m2 are >= those of m1, transfer content
      of m1 to m2 *)
  let transfer m1 m2 =
    assert (m1.line <= m2.line);
    assert (m1.column <= m2.column);
    for i = 0 to m1.line -1 do
      for j = 0 to m1.column -1 do
        if get m1 i j then set m2 i j
      done
    done
end

(** {2 Functor Implementation} *)

module type ELEMENT = sig
  type t

  val eq : t -> t -> bool
    (** Equality function on elements *)

  val hash : t -> int
    (** Hashing on elements *)
end

module Make(E : ELEMENT) = struct
  type elt = E.t

  (* hashtable on elements *)
  module H = Hashtbl.Make(struct
    type t = E.t
    let equal = E.eq
    let hash = E.hash
  end)

  (* the partial order is the adjacency matrix of a DAG *)
  type t = {
    tbl : int H.t; (* element -> index *)
    elements : elt array; (* index -> element *)
    mutable total : bool; (* is the order total? *)
    size : int; (* number of symbols in the table *)
    cmp : BoolMatrix.t; (* adjacency matrix *)
  }

  let create elements =
    let elements = Array.of_list elements in
    let tbl = H.create (Array.length elements) in
    Array.iteri (fun i e -> H.replace tbl e i) elements;
    (* there may be duplicates... *)
    let size = H.length tbl in
    let cmp = BoolMatrix.create size size in
    { elements; tbl; size; cmp; total=false; }

  (* most of the PO is immutable after construction, so only copy the
      mutable part *)
  let copy po =
    { po with cmp = BoolMatrix.copy po.cmp; }

  (* copy with more elements *)
  let extend po elements' =
    (* extend po.elements but keeping the same indexes *)
    let elements' = List.filter (fun x -> not (H.mem po.tbl x)) elements' in
    let elements = Array.of_list (Array.to_list po.elements @ elements') in
    (* same as {!create} *)
    let tbl = H.create (Array.length elements) in
    Array.iteri (fun i e -> H.replace tbl e i) elements;
    let size = H.length tbl in
    let cmp = BoolMatrix.create size size in
    (* transfer content of po.cmp to cmp *)
    BoolMatrix.transfer po.cmp cmp;
    { elements; tbl; size; cmp; total=false; }

  (* check whether the ordering is total *)
  let _check_is_total po =
    let n = po.size in
    try
      for i = 0 to n-1 do
        for j = i+1 to n-1 do
          if BoolMatrix.get po.cmp i j = BoolMatrix.get po.cmp j i
            then raise Exit
              (* pair of elements that are equal or incomparable *)
        done;
      done;
      true
    with Exit -> false

  let is_total po =
    po.total ||
    begin
      let res = _check_is_total po in
      if res then po.total <- true;
      res
    end

  (* update the transitive closure where i>j has just been added. *)
  let _propagate po i j =
    assert (i <> j);
    let n = po.size in
    let cmp = po.cmp in
    (* propagate recursively *)
    let rec propagate i j =
      if BoolMatrix.get cmp i j then () (* stop *)
      else begin
        BoolMatrix.set cmp i j;
        (* k > i and i > j -> k -> j *)
        for k = 0 to n-1 do
          if k <> i && BoolMatrix.get cmp k i && k <> j
            then propagate k j
        done;
        (* i > j and j > k -> i -> k *)
        for k = 0 to n-1 do
          if k <> j && BoolMatrix.get cmp j k && i <> k
            then propagate i k
        done;
      end
    in propagate i j

  (* enrich ordering with the given ordering function *)
  let enrich po cmp_fun =
    if po.total then ()
    else
      let n = po.size in
      let cmp = po.cmp in
      (* look for pairs that are not ordered *)
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          if not (BoolMatrix.get cmp i j) && not (BoolMatrix.get cmp j i) then
            (* elements i and j not ordered, order them by cmp_fun
               and then re-compute the transitive closure *)
            match cmp_fun po.elements.(i) po.elements.(j) with
            | Comparison.Incomparable -> ()
            | Comparison.Eq ->
              _propagate po i j;
              _propagate po j i
            | Comparison.Lt ->
              _propagate po j i
            | Comparison.Gt ->
              _propagate po i j
        done;
      done

  let complete po cmp_fun =
    enrich po
      (fun x y ->
        match cmp_fun x y with
        | 0 -> Comparison.Incomparable  (* see the .mli file *)
        | n when n < 0 -> Comparison.Lt
        | _ -> Comparison.Gt)

  (* compare two elements *)
  let compare po x y =
    let i = H.find po.tbl x in
    let j = H.find po.tbl y in
    match BoolMatrix.get po.cmp i j, BoolMatrix.get po.cmp j i with
    | false, false -> Comparison.Incomparable
    | true, true -> Comparison.Eq
    | true, false -> Comparison.Gt
    | false, true -> Comparison.Lt

  let elements po = Array.to_list po.elements
end
