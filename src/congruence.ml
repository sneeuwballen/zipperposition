
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

(** {1 Simple and Lightweight Congruence and order} *)

module type S = sig
  type term

  type t
    (** Represents a congruence *)

  val create : ?size:int -> unit -> t
    (** New congruence *)

  val find : t -> term -> term
    (** Current representative of this term *)

  val mk_eq : t -> term -> term -> unit
    (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
        the congruence *)

  val mk_less : t -> term -> term -> unit
    (** [mk_less congruence t1 t2] asserts that [t1 < t2] belongs to
        the congruence *)

  val is_eq : t -> term -> term -> bool
    (** Returns true if the two terms are equal in the congruence *)

  val is_less : t -> term -> term -> bool
    (** Returns true if the first term is lower than the second one in the
        congruence *)

  val no_cycles : t -> bool
    (** Checks whether there are cycles in inequalities.
        @return true if all calls to [mk_less] are compatible with
        irreflexivity and transitivity of less. *)
end

(** The graph used for the congruence *)

module type TERM = sig
  type t
  val eq : t -> t -> bool
  val hash : t -> int

  val subterms : t -> t list
    (** Subterms of the term, and possibly a head symbol *)

  val update_subterms : t -> t list -> t
    (** Replace immediate subterms by the given list.
        This is used to test for equality *)
end

module Make(T : TERM) = struct
  type term = T.t

  (** Definition of the congruence closure graph nodes *)

  type node = {
    term : term;
    mutable subnodes : node array;  (* subnodes *)
    mutable lower : node list; (* only for representative *)
    mutable next : node;  (* union-find representative *)
    mutable waiters : (int * int * node) list
      (* (arity,position, waiter) list, only for representative *)
  } (** node of the graph *)

  module H = Hashtbl.Make(struct
    type t = term
    let equal = T.eq
    let hash = T.hash
  end)

  type t = node H.t

  let create ?(size=17) () = H.create size

  (* find representative *)
  let rec _find node =
    if node.next == node
      then node
      else (* path compression *)
        let root = _find node.next in
        let _ = node.next <- root in
        root

  (* are two nodes, with their subterm lists, congruent? To
      check this, we compute the representative of subnodes
      and we check whether updated subterms are equal *)
  let _are_congruent n1 n2 =
    assert (Array.length n1.subnodes = Array.length n2.subnodes);
    let l1' = List.map (fun n -> (_find n).term) (Array.to_list n1.subnodes) in
    let l2' = List.map (fun n -> (_find n).term) (Array.to_list n2.subnodes) in
    let t1 = T.update_subterms n1.term l1' in
    let t2 = T.update_subterms n2.term l2' in
    T.eq t1 t2

  (* check whether all congruences of [node] are computed, by
      looking at equivalence classes of [subnodes] *)
  let rec _check_congruence node =
    let arity = Array.length node.subnodes in
    for i = 0 to arity - 1 do
      let subnode = _find node.subnodes.(i) in
      List.iter
        (fun (arity', i', node') ->
          if i = i' && arity = arity' && _are_congruent node node'
            then _merge node node')
        subnode.waiters
    done

  (* merge n1 and n2 equivalence classes *)
  and _merge n1 n2 =
    (* get representatives *)
    let n1 = _find n1 in
    let n2 = _find n2 in
    if n1 != n2 then begin
      n1.next <- n2;
      (* n1 now points to n2, put every class information in n2 *)
      n2.lower <- List.rev_append n1.lower n2.lower;
      n1.lower <- [];
      let left, right = n1.waiters, n2.waiters in
      n2.waiters <- List.rev_append n1.waiters n2.waiters;
      n1.waiters <- [];
      (* check congruence of waiters of n1 and n2 *)
      List.iter
        (fun (arity1, i1, n1') ->
          List.iter
            (fun (arity2, i2, n2') ->
              if arity1 = arity2 && i1 = i2 && _are_congruent n1' n2'
                then _merge n1' n2')
            right)
        left
    end

  (* obtain the node for this term. If not present, create it *)
  let rec _get h t =
    try H.find h t
    with Not_found ->
      let rec node = {
        term = t;
        subnodes = [| |];  (* updated later *)
        lower = [];
        next = node;
        waiters = [];
      } in
      H.add h t node;
      (* register the node to its subterms *)
      let subterms = T.subterms t in
      let arity = List.length subterms in
      (* obtain subnodes' current equiv classes *)
      let subnodes = List.map (_get h) subterms in
      let subnodes = Array.of_list subnodes in
      node.subnodes <- subnodes;
      (* possibly, merge with other nodes *)
      _check_congruence node;
      (* register to future merges of subnodes *)
      Array.iteri
        (fun i subnode ->
          let subnode = _find subnode in
          subnode.waiters <- (arity, i, node) :: subnode.waiters)
        subnodes;
      (* return node *)
      node

  let find h t =
    let n = _get h t in
    let n = _find n in
    n.term

  let mk_eq congruence t1 t2 =
    let n1 = _get congruence t1 in
    let n2 = _get congruence t2 in
    _merge n1 n2

  let mk_less congruence t1 t2 =
    let n1 = _find (_get congruence t1) in
    let n2 = _find (_get congruence t2) in
    if not (List.memq n1 n2.lower)
      then n2.lower <- n1 :: n2.lower;
    ()

  let is_eq congruence t1 t2 =
    let n1 = _find (_get congruence t1) in
    let n2 = _find (_get congruence t2) in
    n1 == n2

  let is_less congruence t1 t2 =
    let n1 = _find (_get congruence t1) in
    let n2 = _find (_get congruence t2) in
    (* follow [.lower] links, from [current], until we find [target].
        [explored] is used to break cycles, if any *)
    let rec search explored target current =
      if List.memq current explored
        then false
      else if target == current
        then true
      else
        let explored = current :: explored in
        List.exists
          (fun cur' -> search explored target (_find cur'))
          current.lower
    in
    (* search. target=n1, current=n2 *)
    search [] n1 n2

  let no_cycles h =
    failwith "no_cycles: not implemented"
end

module FO = Make(struct
  module T = FOTerm

  type t = T.t
  let eq = T.eq
  let hash = T.hash

  let subterms t = match t.T.term with
    | T.Var _
    | T.BoundVar _ -> []
    | T.Node (s, l) -> l

  let update_subterms t l = match t.T.term, l with
    | (T.Var _
    | T.BoundVar _), [] -> t
    | T.Node (s, l), l' when List.length l = List.length l' ->
      T.mk_node s l'
    | _ -> assert false
end)

module HO = Make(struct
  module T = HOTerm

  type t = T.t
  let eq = T.eq
  let hash = T.hash

  let subterms t = match t.T.term with
    | T.Const _
    | T.Var _
    | T.BoundVar _ -> []
    | T.At (t1, t2) -> [t1; t2]
    | T.Bind (_, t) -> [t]

  let update_subterms t l = match t.T.term, l with
    | (T.Const _
    | T.Var _
    | T.BoundVar _), [] -> t
    | T.At (t1, t2), [t1'; t2'] -> T.mk_at t1' t2'
    | T.Bind (s, _), [t'] -> T.mk_bind s t'
    | _ -> assert false
end)
