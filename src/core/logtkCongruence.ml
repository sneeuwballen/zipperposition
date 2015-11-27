
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
    (** New congruence.
        @param size a hint for the initial size of the hashtable. *)

  val clear : t -> unit
    (** Clear the content of the congruence. It is now equivalent to
        the empty congruence. *)

  val push : t -> unit
    (** Push a checkpoint on the stack of the congruence. An equivalent call
        to {!pop} will restore the congruence to its current state. *)

  val pop : t -> unit
    (** Restore to the previous checkpoint.
        @raise Invalid_argument if there is no checkpoint to restore to
          (ie if no call to {!push} matches this call to {!pop}) *)

  val stack_size : t -> int
    (** Number of calls to {!push} that lead to the current state of the
        congruence. Also, how many times {!pop} can be called. *)

  val find : t -> term -> term
    (** Current representative of this term *)

  val iter : t -> (mem:term -> repr:term -> unit) -> unit
    (** Iterate on terms that are explicitely present in the congruence.
        The callback is given [mem], the term itself, and [repr],
        the current representative of the term [mem].

        Invariant: when calling [iter cc f], if [f ~mem ~repr] is called,
        then [find cc mem == repr] holds.
    *)

  val iter_roots : t -> (term -> unit) -> unit
    (** Iterate on the congruence classes' representative elements.
        Exactly one term per congruence class will be passed to the
        function. *)

  val mk_eq : t -> term -> term -> unit
    (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
        the congruence *)

  val mk_less : t -> term -> term -> unit
    (** [mk_less congruence t1 t2] asserts that [t1 < t2] belongs to
        the congruence *)

  val is_eq : t -> term -> term -> bool
    (** Returns true if the two terms are equal in the congruence. This
        updates the congruence, because the two terms need to be added. *)

  val is_less : t -> term -> term -> bool
    (** Returns true if the first term is strictly lower than the second
        one in the congruence *)

  val cycles : t -> bool
    (** Checks whether there are cycles in inequalities.
        @return true if calls to [mk_eq] and [mk_less] entail a cycle in
        the ordering (hence contradicting irreflexivity/transitivity of less) *)
end

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
    mutable lower : node list; (* only for representative *)
    mutable next : node;  (* union-find representative *)
    mutable waiters : (int * int * node) list
      (* (arity,position, waiter) list, only for representative *)
  } (** node of the graph *)

  module H = Hashtbl.Make(struct
    type t = term
    let equal = T.equal
    let hash = T.hash
  end)

  type stack_cell =
    | Checkpoint
    | Undo of (unit -> unit)
    | Stop

  type t = {
    tbl : node H.t;               (* table of nodes *)
    stack : stack_cell Stack.t;   (* backtrack stack. Last element always=Stop *)
    mutable stack_size : int;     (* number of Checkpoint *)
  }

  let create ?(size=17) () =
    let cc = {
      tbl = H.create size;
      stack = Stack.create ();
      stack_size = 0;
    } in
    Stack.push Stop cc.stack;
    cc

  let clear h =
    H.clear h.tbl;
    Stack.clear h.stack;
    Stack.push Stop h.stack;
    h.stack_size <- 0;
    ()

  let push h =
    Stack.push Checkpoint h.stack;
    h.stack_size <- h.stack_size + 1;
    ()

  let rec pop h = match Stack.top h.stack with
    | Stop -> raise (Invalid_argument "Congruence.pop")
    | Checkpoint ->
      ignore (Stack.pop h.stack);
      h.stack_size <- h.stack_size - 1;
      assert (h.stack_size >= 0);
    | Undo f ->
      ignore (Stack.pop h.stack);
      f ();
      pop h

  let stack_size cc = cc.stack_size

  (* push an action on the undo stack *)
  let _push_undo cc f=
    Stack.push (Undo f) cc.stack

  (* update [node.next] to be [next] *)
  let _set_next cc node next =
    let old_next = node.next in
    _push_undo cc (fun () -> node.next <- old_next);
    node.next <- next;
    ()

  (* update [node.lower] to be [lower] *)
  let _set_lower cc node lower =
    let old_lower = node.lower in
    _push_undo cc (fun () -> node.lower <- old_lower);
    node.lower <- lower;
    ()

  (* update [node.waiters] to be [waiters] *)
  let _set_waiters cc node waiters =
    let old_waiters = node.waiters in
    _push_undo cc (fun () -> node.waiters <- old_waiters);
    node.waiters <- waiters;
    ()

  (* add a node to the table *)
  let _add_node cc t node =
    _push_undo cc (fun () -> H.remove cc.tbl t);
    H.add cc.tbl t node;
    ()

  (* find representative *)
  let rec _find cc node =
    if node.next == node
      then node  (* root *)
      else begin
        let root = _find cc node.next in
        (* path compression *)
        if root != node.next then _set_next cc node root;
        root
      end

  (* are two nodes, with their subterm lists, congruent? To
      check this, we compute the representative of subnodes
      and we check whether updated subterms are equal *)
  let _are_congruent cc n1 n2 =
    assert (Array.length n1.subnodes = Array.length n2.subnodes);
    let l1' = List.map (fun n -> (_find cc n).term) (Array.to_list n1.subnodes) in
    let l2' = List.map (fun n -> (_find cc n).term) (Array.to_list n2.subnodes) in
    try
      let t1 = T.update_subterms n1.term l1' in
      let t2 = T.update_subterms n2.term l2' in
      T.equal t1 t2
    with LogtkType.Error _ ->
      false

  (* check whether all congruences of [node] are computed, by
      looking at equivalence classes of [subnodes] *)
  let rec _check_congruence cc node =
    let arity = Array.length node.subnodes in
    for i = 0 to arity - 1 do
      let subnode = _find cc node.subnodes.(i) in
      List.iter
        (fun (arity', i', node') ->
          if i = i' && arity = arity' && _are_congruent cc node node'
            then _merge cc node node')
        subnode.waiters
    done

  (* merge n1 and n2 equivalence classes *)
  and _merge cc n1 n2 =
    (* get representatives *)
    let n1 = _find cc n1 in
    let n2 = _find cc n2 in
    if n1 != n2 then begin
      _set_next cc n1 n2;
      (* n1 now points to n2, put every class information in n2 *)
      _set_lower cc n2 (List.rev_append n1.lower n2.lower);
      _set_lower cc n1 [];
      let left, right = n1.waiters, n2.waiters in
      _set_waiters cc n2 (List.rev_append n1.waiters n2.waiters);
      _set_waiters cc n1 [];
      (* check congruence of waiters of n1 and n2 *)
      List.iter
        (fun (arity1, i1, n1') ->
          List.iter
            (fun (arity2, i2, n2') ->
              if arity1 = arity2 && i1 = i2 && _are_congruent cc n1' n2'
                then _merge cc n1' n2')
            right)
        left
    end

  (* obtain the node for this term. If not present, create it *)
  let rec _get cc t =
    try H.find cc.tbl t
    with Not_found ->
      let rec node = {
        term = t;
        subnodes = [| |];  (* updated later *)
        lower = [];
        next = node;
        waiters = [];
      } in
      _add_node cc t node;
      (* register the node to its subterms *)
      let subterms = T.subterms t in
      let arity = List.length subterms in
      (* obtain subnodes' current equiv classes (no need to undo) *)
      let subnodes = List.map (_get cc) subterms in
      let subnodes = Array.of_list subnodes in
      node.subnodes <- subnodes;
      (* possibly, merge with other nodes *)
      _check_congruence cc node;
      (* register to future merges of subnodes *)
      Array.iteri
        (fun i subnode ->
          let subnode = _find cc subnode in
          _set_waiters cc subnode ((arity, i, node) :: subnode.waiters))
        subnodes;
      (* return node *)
      node

  let find cc t =
    let n = _get cc t in
    let n = _find cc n in
    n.term

  let iter cc f =
    H.iter
      (fun mem node ->
        let repr = (_find cc node).term in
        f ~mem ~repr)
      cc.tbl

  let _iter_roots_node cc f =
    H.iter
      (fun _ node ->
        if node == node.next then f node)
      cc.tbl

  let iter_roots cc f = _iter_roots_node cc (fun node -> f node.term)

  let mk_eq cc t1 t2 =
    let n1 = _get cc t1 in
    let n2 = _get cc t2 in
    _merge cc n1 n2

  let mk_less cc t1 t2 =
    let n1 = _find cc (_get cc t1) in
    let n2 = _find cc (_get cc t2) in
    if not (List.memq n1 n2.lower)
      then _set_lower cc n2 (n1 :: n2.lower);
    ()

  let is_eq cc t1 t2 =
    let n1 = _find cc (_get cc t1) in
    let n2 = _find cc (_get cc t2) in
    n1 == n2

  let is_less cc t1 t2 =
    let n1 = _find cc (_get cc t1) in
    let n2 = _find cc (_get cc t2) in
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
          (fun cur' -> search explored target (_find cc cur'))
          current.lower
    in
    (* search. target=n1, current=n2 *)
    search [] n1 n2

  let cycles cc =
    try
      let explored = H.create 7 in
      (* starting from each root, explore "less" graph in DFS *)
      _iter_roots_node cc
        (fun root ->
          H.clear explored;
          let s = Stack.create () in
          (* initial step *)
          H.add explored root.term ();
          List.iter (fun node' -> Stack.push (node', [root]) s) root.lower;
          (* explore *)
          while not (Stack.is_empty s) do
            let node, path = Stack.pop s in
            if not (H.mem explored node.term) then begin
              H.add explored node.term ();
              (* explore children *)
              List.iter
                (fun node' ->
                  if List.memq node' path
                    then raise Exit (* found cycle *)
                    else Stack.push (node', node::path) s)
                node.lower
            end;
          done);
      false
    with Exit ->
      true
end

module FO = Make(struct
  module T = LogtkFOTerm

  type t = T.t
  let equal = T.equal
  let hash = T.hash

  let subterms t = match T.Classic.view t with
    | T.Classic.App (_, _, l) -> l
    | _ -> []

  let update_subterms t l = match T.view t, l with
    | T.App (hd, l), l' when List.length l = List.length l' ->
      T.app hd l'
    | _, [] -> t
    | _ -> assert false
end)

module HO = Make(struct
  module T = LogtkHOTerm

  type t = T.t
  let equal = T.equal
  let hash = T.hash

  let subterms t = match T.view t with
    | T.Const _
    | T.Var _
    | T.BVar _ -> []
    | T.Lambda (_,t') | T.Forall (_, t') | T.Exists (_, t') -> [t']
    | T.At (t1, t2) -> [t1;t2]
    | T.TyLift _ -> []
    | T.Multiset (_,l) -> l
    | T.Record _ -> assert false   (* TODO *)

  let update_subterms t l = match T.view t, l with
    | (T.Const _ | T.Var _ | T.BVar _), [] -> t
    | T.At _, [t1'; t2'] -> T.at t1' t2'
    | T.TyLift _, [] -> t
    | T.Lambda (varty, _), [t'] -> T.__mk_lambda ~varty t'
    | T.Forall (varty, _), [t'] -> T.__mk_forall ~varty t'
    | T.Exists (varty, _), [t'] -> T.__mk_exists ~varty t'
    | T.Multiset(ty,_), l -> T.multiset ~ty l
    | T.Record _, _ -> assert false (* TODO *)
    | _ -> assert false
end)
