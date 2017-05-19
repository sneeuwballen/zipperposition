
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Imperative Union-Find structure} *)

(** We need to be able to hash and compare keys, and values need to form
    a monoid *)
module type PAIR = sig
  type key
  type value

  val hash : key -> int
  val equal : key -> key -> bool

  val merge : value -> value -> value
  val zero : value
end

(** Build a union-find module from a key/value specification *)
module Make(P : PAIR) = struct
  type key = P.key
  (** Elements that can be compared *)

  type value = P.value
  (** Values associated with elements *)

  type node = {
    n_key: key;
    mutable n_repr : node option; (* representative *)
    mutable n_value : value; (* value (only up-to-date for representative) *)
  }

  module H = CCHashtbl.Make(struct include P type t = P.key end)

  (** The union-find imperative structure itself*)
  type t = node H.t

  let mk_node key = {
    n_repr = None;
    n_key=key;
    n_value = P.zero;
  }

  (** Elements that can be compared *)
  let create keys =
    let t = H.create 8 in
    (* add k -> zero for each key k *)
    List.iter (fun key -> H.replace t key (mk_node key)) keys;
    t

  let mem t key = H.mem t key

  let n_repr n = n.n_repr
  let n_key n = n.n_key
  let n_value n = n.n_value

  let rec find_root (node:node): node = match node.n_repr with
    | None -> node
    | Some node' ->
      (* path compression *)
      assert (node != node');
      let root = find_root node' in
      node.n_repr <- Some root;
      root

  let find_node t key = H.find t key

  let find t key = find_node t key |> find_root |> n_key

  (** Get value of the root for this key. *)
  let find_value t key = find_node t key |> find_root |> n_value

  let union_node n1 n2 =
    assert (n1.n_repr = None && n2.n_repr = None);
    if n1 != n2 then (
      (* k2 points to k1, and k1 points to the new value *)
      n1.n_value <- P.merge n1.n_value n2.n_value;
      n2.n_repr <- Some n1;
    )

  (** Merge two representatives *)
  let union t k1 k2 =
    let n1 = find_node t k1 |> find_root in
    let n2 = find_node t k2 |> find_root in
    union_node n1 n2

  (** Add the given value to the key (monoid) *)
  let add t key value =
    try
      let node = find_node t key |> find_root in
      node.n_value <- P.merge node.n_value value
    with Not_found ->
      let node = mk_node key in
      node.n_value <- value;
      H.add t key node

  (** Iterate on representative and their value *)
  let iter t f =
    H.iter
      (fun key node ->
         if P.equal key node.n_key && node.n_repr=None then f key node.n_value)
      t

  let to_seq t f =
    iter t (fun x y -> f (x,y))
end
