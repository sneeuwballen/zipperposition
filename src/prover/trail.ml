
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

module BLit = Bool_lit

type bool_lit = Bool_lit.t

type t = BLit.Set.t

let equal = BLit.Set.equal
let compare = BLit.Set.compare
let hash_fun trail h = CCHash.seq BLit.hash_fun (BLit.Set.to_seq trail) h
let hash = CCHash.apply hash_fun

let empty = BLit.Set.empty
let mem = BLit.Set.mem
let for_all = BLit.Set.for_all
let exists = BLit.Set.exists
let singleton = BLit.Set.singleton
let add = BLit.Set.add
let remove = BLit.Set.remove
let fold f acc t = BLit.Set.fold (fun x acc -> f acc x) t acc
let length = BLit.Set.cardinal
let map f set =
  BLit.Set.to_seq set
  |> Sequence.map f
  |> BLit.Set.of_seq
let of_list = BLit.Set.of_list
let to_list = BLit.Set.to_list
let is_empty = BLit.Set.is_empty

let subsumes t1 t2 = BLit.Set.subset t1 t2

let is_trivial trail =
  BLit.Set.exists
    (fun i -> BLit.Set.mem (BLit.neg i) trail)
    trail

let merge = BLit.Set.union

let merge_l = function
  | [] -> BLit.Set.empty
  | [t] -> t
  | [t1;t2] -> BLit.Set.union t1 t2
  | t::l -> List.fold_left BLit.Set.union t l

let filter = BLit.Set.filter

type valuation = BLit.t -> bool
(** A boolean valuation *)

let is_active trail ~v =
  BLit.Set.for_all
    (fun i ->
       let j = BLit.abs i in
       (BLit.sign i) = (v j))  (* valuation match sign *)
    trail

let to_seq = BLit.Set.to_seq
