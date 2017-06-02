
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Generic term indexing} *)

type term = Term.t
type subst = Subst.t

module T = Term

(** {2 Leaf} *)

(** A leaf maps terms to a set of elements *)

module type LEAF = Index_intf.LEAF

module MakeLeaf(X : Set.OrderedType) : LEAF with type elt = X.t = struct
  module S = Set.Make(X)

  type t = S.t T.Map.t

  type elt = X.t

  let empty = T.Map.empty

  let find_ leaf t =
    try T.Map.find t leaf with Not_found -> S.empty

  let add leaf t data =
    let set = find_ leaf t in
    let set = S.add data set in
    T.Map.add t set leaf

  let remove leaf t data =
    try
      let set = T.Map.find t leaf in
      let set = S.remove data set in
      if S.is_empty set
      then T.Map.remove t leaf
      else T.Map.add t set leaf
    with Not_found ->
      leaf

  let is_empty = T.Map.is_empty

  let iter leaf f =
    T.Map.iter (fun t set -> S.iter (fun elt -> f t elt) set) leaf

  let fold leaf acc f =
    T.Map.fold (fun t set acc -> S.fold (fun elt acc -> f acc t elt) set acc) leaf acc

  let size leaf =
    T.Map.fold (fun _ set acc -> S.cardinal set + acc) leaf 0

  let fold_unify ?(subst=Unif_subst.empty) (leaf,sc_l) t k =
    T.Map.iter
      (fun t' set ->
         try
           let subst = Unif.FO.unify_full ~subst (t',sc_l) t in
           S.iter (fun data -> k (t', data, subst)) set
         with Unif.Fail -> ())
      leaf

  let fold_match ?(subst=Subst.empty) (leaf,sc_l) t k =
    T.Map.iter
      (fun t' set ->
         try
           let subst = Unif.FO.matching ~subst ~pattern:(t',sc_l) t in
           S.iter
             (fun data -> k (t', data, subst))
             set
         with Unif.Fail -> ())
      leaf

  let fold_matched ?(subst=Subst.empty) (leaf,sc_l) t k =
    T.Map.iter
      (fun t' set ->
         try
           let subst = Unif.FO.matching ~subst ~pattern:t (t',sc_l) in
           S.iter
             (fun data -> k (t', data, subst))
             set
         with Unif.Fail -> ())
      leaf
end

(** {2 Term index} *)

module type TERM_IDX = Index_intf.TERM_IDX

(** {2 Subsumption Index} *)

module type CLAUSE = Index_intf.CLAUSE

(** A subsumption index (non perfect!) *)

module type SUBSUMPTION_IDX = Index_intf.SUBSUMPTION_IDX

(** {2 Specialized rewriting index} *)

module type EQUATION = Index_intf.EQUATION

module type UNIT_IDX = Index_intf.UNIT_IDX

module BasicEquation = struct
  type t = T.t * T.t
  type rhs = T.t
  let compare (l1,r1)(l2,r2) =
    let c = T.compare l1 l2 in
    if c <> 0 then c else T.compare r1 r2
  let extract (l,r) = l,r,true
  let priority _ = 1
end

