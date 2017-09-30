
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simple and Lightweight Congruence and order} *)

module type S = Congruence_intf.S

(** The graph used for the congruence *)

module type TERM = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val subterms : t -> t list
  (** Subterms of the term (possibly empty list) *)

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
    mutable next : node;  (* union-find representative *)
    mutable parents : (int * int * node) list
    (* (arity,position, waiter) list, only for representative *)
  } (** node of the graph *)

  module H = Hashtbl.Make(struct
      type t = term
      let equal = T.equal
      let hash = T.hash
    end)

  type t = {
    tbl : node H.t; (* table of nodes *)
    stack : (unit -> unit) CCVector.vector; (* backtrack stack. *)
  }

  let create ?(size=64) () =
    let cc = {
      tbl = H.create size;
      stack = CCVector.create ();
    } in
    cc

  let clear h =
    H.clear h.tbl;
    CCVector.clear h.stack;
    ()

  type level = int

  let save h = CCVector.length h.stack

  let restore h lev =
    while CCVector.length h.stack > lev do
      let f = CCVector.pop_exn h.stack in
      f ();
    done

  (* push an action on the undo stack *)
  let on_backtrack_ cc f = CCVector.push cc.stack f

  (* update [node.next] to be [next] *)
  let set_next_ cc node next =
    let old_next = node.next in
    on_backtrack_ cc (fun () -> node.next <- old_next);
    node.next <- next;
    ()

  (* update [node.parents] to be [parents] *)
  let set_parents_ cc node parents =
    let old_parents = node.parents in
    on_backtrack_ cc (fun () -> node.parents <- old_parents);
    node.parents <- parents;
    ()

  (* add a node to the table *)
  let add_node_ cc t node =
    on_backtrack_ cc (fun () -> H.remove cc.tbl t);
    H.add cc.tbl t node;
    ()

  (* find representative *)
  let rec find_ cc node =
    if node.next == node
    then node  (* root *)
    else (
      let root = find_ cc node.next in
      (* path compression *)
      if root != node.next then set_next_ cc node root;
      root
    )

  (* are two nodes, with their subterm lists, congruent? To
      check this, we compute the representative of subnodes
      and we check whether updated subterms are equal *)
  let are_congruent_ cc n1 n2 =
    assert (Array.length n1.subnodes = Array.length n2.subnodes);
    let l1' = List.map (fun n -> (find_ cc n).term) (Array.to_list n1.subnodes) in
    let l2' = List.map (fun n -> (find_ cc n).term) (Array.to_list n2.subnodes) in
    try
      let t1 = T.update_subterms n1.term l1' in
      let t2 = T.update_subterms n2.term l2' in
      T.equal t1 t2
    with Type.ApplyError _ ->
      false

  (* check whether all congruences of [node] are computed, by
      looking at equivalence classes of [subnodes] *)
  let rec check_parents_pairs_ cc node =
    let arity = Array.length node.subnodes in
    for i = 0 to arity - 1 do
      let subnode = find_ cc node.subnodes.(i) in
      List.iter
        (fun (arity', i', node') ->
           if i = i' && arity = arity' && are_congruent_ cc node node'
           then merge_ cc node node')
        subnode.parents
    done

  (* merge n1 and n2 equivalence classes *)
  and merge_ cc n1 n2 =
    (* get representatives *)
    let n1 = find_ cc n1 in
    let n2 = find_ cc n2 in
    if n1 != n2 then (
      set_next_ cc n1 n2;
      (* n1 now points to n2, put every class information in n2 *)
      let left, right = n1.parents, n2.parents in
      set_parents_ cc n2 (List.rev_append n1.parents n2.parents);
      set_parents_ cc n1 [];
      (* check congruence of parents of n1 and n2 *)
      List.iter
        (fun (arity1, i1, n1') ->
           List.iter
             (fun (arity2, i2, n2') ->
                if arity1 = arity2 && i1 = i2 && are_congruent_ cc n1' n2'
                then merge_ cc n1' n2')
             right)
        left
    )

  (* obtain the node for this term. If not present, create it *)
  let rec get_node_ cc t =
    try H.find cc.tbl t
    with Not_found ->
      let rec node = {
        term = t;
        subnodes = [| |];  (* updated later *)
        next = node;
        parents = [];
      } in
      add_node_ cc t node;
      (* register the node to its subterms *)
      let subterms = T.subterms t in
      let arity = List.length subterms in
      (* obtain subnodes' current equiv classes (no need to undo) *)
      let subnodes = List.map (get_node_ cc) subterms in
      let subnodes = Array.of_list subnodes in
      node.subnodes <- subnodes;
      (* possibly, merge with other nodes *)
      check_parents_pairs_ cc node;
      (* register to future merges of subnodes *)
      Array.iteri
        (fun i subnode ->
           let subnode = find_ cc subnode in
           set_parents_ cc subnode ((arity, i, node) :: subnode.parents))
        subnodes;
      (* return node *)
      node

  let find cc t =
    let n = get_node_ cc t in
    let n = find_ cc n in
    n.term

  let iter cc f =
    H.iter
      (fun mem node ->
         let repr = (find_ cc node).term in
         f ~mem ~repr)
      cc.tbl

  let iter_roots_ cc f =
    H.iter
      (fun _ node ->
         if node == node.next then f node)
      cc.tbl

  let iter_roots cc f = iter_roots_ cc (fun node -> f node.term)

  let mk_eq cc t1 t2 =
    let n1 = get_node_ cc t1 in
    let n2 = get_node_ cc t2 in
    merge_ cc n1 n2

  let is_eq cc t1 t2 =
    let n1 = find_ cc (get_node_ cc t1) in
    let n2 = find_ cc (get_node_ cc t2) in
    n1 == n2
end

module FO = Make(struct
    module T = Term

    type t = T.t
    let equal = T.equal
    let hash = T.hash

    let subterms t = match T.Classic.view t with
      | T.Classic.App (_, l) -> l
      | _ -> []

    let update_subterms t l = match T.view t, l with
      | T.App (hd, l), l' when List.length l = List.length l' ->
        T.app hd l'
      | _, [] -> t
      | _ -> assert false
  end)
