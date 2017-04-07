
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Trail} *)

open Logtk

module Lit = BBox.Lit

type bool_lit = Lit.t

type t = Lit.Set.t

let equal = Lit.Set.equal
let compare = Lit.Set.compare
let hash trail = Hash.seq Lit.hash (Lit.Set.to_seq trail)

let empty = Lit.Set.empty
let mem = Lit.Set.mem
let for_all = Lit.Set.for_all
let exists = Lit.Set.exists
let singleton = Lit.Set.singleton
let add = Lit.Set.add
let remove = Lit.Set.remove
let fold f acc t = Lit.Set.fold (fun x acc -> f acc x) t acc
let length = Lit.Set.cardinal
let map f set =
  Lit.Set.to_seq set
  |> Sequence.map f
  |> Lit.Set.of_seq
let of_list = Lit.Set.of_list
let add_list = Lit.Set.add_list
let to_list = Lit.Set.to_list
let is_empty = Lit.Set.is_empty

let subsumes t1 t2 = Lit.Set.subset t1 t2

let is_trivial trail =
  Lit.Set.exists
    (fun i -> Lit.Set.mem (Lit.neg i) trail)
    trail

let merge = Lit.Set.union

let merge_l = function
  | [] -> Lit.Set.empty
  | [t] -> t
  | [t1;t2] -> Lit.Set.union t1 t2
  | t::l -> List.fold_left Lit.Set.union t l

let filter = Lit.Set.filter

type valuation = Lit.t -> bool
(** A boolean valuation *)

let is_active trail ~v =
  Lit.Set.for_all
    (fun i ->
       let j = Lit.abs i in
       (Lit.sign i) = (v j))  (* valuation match sign *)
    trail

let to_seq = Lit.Set.to_seq

let labels (t:t) =
  to_seq t
  |> Sequence.map BBox.Lit.to_int
  |> Util.Int_set.of_seq

let to_s_form (t:t) =
  let module F = TypedSTerm.Form in
  begin match to_list t |> List.map BBox.to_s_form with
    | [] -> F.true_
    | [f] -> f
    | l -> F.and_ l
  end
