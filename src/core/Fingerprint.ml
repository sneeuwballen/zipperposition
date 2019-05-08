
(* This file is free software, part of Zipperposition. See file "license" for more details. *)


(** {1 Fingerprint term indexing} *)

module T = Term
module I = Index
module S = Subst

let prof_traverse = Util.mk_profiler "fingerprint.traverse"

(* a feature.
   A    = variable
   B    = below variable
   DB i = De-Bruijn index i 
   N    = invalid position
   S c  = symbol c
*)
type feature = A | B | DB of int | N | S of ID.t | Ignore

(* a fingerprint function, it computes several features of a term *)
type fingerprint_fun = T.t -> feature list

(* TODO: use a feature array, rather than a list *)

(* TODO: more efficient implem of traversal, only following branches that
   are useful instead of folding and filtering *)

(* compute a feature for a given position *)
let rec gfpf ?(depth=0) pos t =
  let pref_vars, body =  T.open_fun t in 
  match pos with 
  | [] -> gfpf_root ~depth body
  | i::is ->
      let hd, args = T.as_app body in
      if T.is_var hd then B
      else (
        if List.length args >= i then (
          gfpf ~depth:(depth + (List.length pref_vars)) is (List.nth args @@  i-1 )
        ) 
        else (
          let _, ret = Type.open_fun (Term.ty body) in
          if (Type.is_var ret) then B else N
        ) 
      )
and gfpf_root ~depth t =
  match T.view t with 
  | T.AppBuiltin(_, _) -> Ignore
  | T.DB i -> if (i < depth) then DB i else Ignore
  | T.Var _ -> A
  | T.Const c -> S c 
  | T.App (hd,_) -> (match T.view hd with
                         T.Var _ -> A 
                         | T.Const s -> S s
                         | T.DB i    -> if (i < depth) then DB i else Ignore
                         | _ -> Format.printf "%a\n" T.pp t; assert false)
  | T.Fun (_, _) -> assert false 

(* TODO more efficient way to compute a vector of features: if the fingerprint
   is in BFS, compute features during only one traversal of the term? *)

(** compute a feature vector for some positions *)
let fp positions =
  (* list of fingerprint feature functions *)
  let fpfs = List.map gfpf positions in
  fun t ->
    (* Format.printf "@[Fingerprinting: @[%a@].@]\n" T.pp t; *)
    List.map (fun fpf -> fpf t) fpfs

(** {2 Fingerprint functions} *)

let fp3d = fp [[]; [1]; [1;1]]
let fp3w = fp [[]; [1]; [2]]
let fp4d = fp [[]; [1]; [1;1;]; [1;1;1]]
let fp4m = fp [[]; [1]; [2]; [1;1]]
let fp4w = fp [[]; [1]; [2]; [3]]
let fp5m = fp [[]; [1]; [2]; [3]; [1;1]]
let fp6m = fp [[]; [1]; [2]; [3]; [1;1]; [1;2]]
let fp7  = fp [[]; [1]; [2]; [1;1]; [1;2]; [2;1] ; [2;2]]
let fp7m = fp [[]; [1]; [2]; [3]; [1;1]; [4]; [1;2]]
let fp16 = fp [[]; [1]; [2]; [3]; [4]; [1;1]; [1;2]; [1;3]; [2;1];
               [2;2]; [2;3]; [3;1]; [3;2]; [3;3]; [1;1;1]; [2;1;1]]

(** {2 Index construction} *)

let feat_to_int_ = function
  | A -> 0
  | B -> 1
  | S _ -> 2
  | N -> 3
  | DB _ -> 4
  | Ignore -> 5

let cmp_feature f1 f2 = match f1, f2 with
  | A, A
  | B, B
  | N, N
  | Ignore, Ignore
    -> 0
  | S s1, S s2 -> ID.compare s1 s2
  | DB i, DB j -> compare i j
  | _ -> feat_to_int_ f1 - feat_to_int_ f2

(** check whether two features are compatible for unification. *)
let compatible_features_unif f1 f2 =
  match f1 with
  | S s1 -> (match f2 with
             | S s2 -> ID.equal s1 s2 
             | A | B | Ignore -> true
             | N | DB _ -> false)
  | Ignore 
  | B    -> true
  | A    -> (match f2 with
             | DB _ | N  -> false
             | Ignore | S _ | A | B  -> true)
  | DB i -> (match f2 with 
             | DB j -> i = j
             | B | Ignore -> true
             | A | S _ | N -> false)
  | N ->    (match f2 with 
             | N | B | Ignore -> true
             | A | DB _ | S _ -> false)

(** check whether two features are compatible for matching. *)
let compatible_features_match f1 f2 =
  match f1 with
  | S s1 -> (match f2 with
             | S s2 -> ID.equal s1 s2
             | Ignore -> true 
             | _ -> false)
  | Ignore 
  | B    -> true
  | A    -> (match f2 with
             | A | S _ | Ignore -> true
             | _ -> false)
  | DB i -> (match f2 with 
             | DB j -> i = j
             | Ignore -> true
             | _ -> false)
  | N ->    (match f2 with 
             | N | Ignore -> true
             | _ -> false)


(** Map whose keys are features *)
module FeatureMap = Map.Make(struct
    type t = feature
    let compare = cmp_feature
  end)

module Make(X : Set.OrderedType) = struct
  type elt = X.t

  module Leaf = Index.MakeLeaf(X)

  type t = {
    trie : trie;
    fp : fingerprint_fun;
  }
  and trie =
    | Empty
    | Node of trie FeatureMap.t
    | Leaf of Leaf.t
    (** The index *)

  let default_fp = fp7m

  let empty () = {
    trie = Empty;
    fp = default_fp;
  }

  let empty_with fp = {
    trie = Empty;
    fp;
  }

  let get_fingerprint idx = idx.fp

  let name = "fingerprint_idx"

  let is_empty idx =
    let rec is_empty trie =
      match trie with
        | Empty -> true
        | Leaf l -> Leaf.is_empty l
        | Node map -> FeatureMap.for_all (fun _ trie' -> is_empty trie') map
    in is_empty idx.trie

  (** add t -> data to the trie *)
  let add idx t data =
    (* recursive insertion *)
    let rec recurse trie features =
      match trie, features with
        | Empty, [] ->
          let leaf = Leaf.empty in
          let leaf = Leaf.add leaf t data in
          Leaf leaf (* creation of new leaf *)
        | Empty, f::features' ->
          let subtrie = recurse Empty features' in
          let map = FeatureMap.add f subtrie FeatureMap.empty in
          Node map  (* index new subtrie by feature *)
        | Node map, f::features' ->
          let subtrie =
            try FeatureMap.find f map
            with Not_found -> Empty in
          (* insert in subtrie *)
          let subtrie = recurse subtrie features' in
          let map = FeatureMap.add f subtrie map in
          Node map  (* point to new subtrie *)
        | Leaf leaf, [] ->
          let leaf = Leaf.add leaf t data in
          Leaf leaf (* addition to set *)
        | Node _, [] | Leaf _, _::_ ->
          failwith "different feature length in fingerprint trie"
    in
    let features = idx.fp (t) in  (* features of term *)
    { idx with trie = recurse idx.trie features; }

  let add_ trie = CCFun.uncurry (add trie)
  let add_seq = Iter.fold add_
  let add_list = List.fold_left add_

  (** remove t -> data from the trie *)
  let remove idx t data =
    (* recursive deletion *)
    let rec recurse trie features =
      match trie, features with
        | Empty, [] | Empty, _::_ ->
          Empty (* keep it empty *)
        | Node map, f::features' ->
          let map =
            (* delete from subtrie, if there is a subtrie *)
            try
              let subtrie = FeatureMap.find f map in
              let subtrie = recurse subtrie features' in
              if subtrie = Empty
              then FeatureMap.remove f map
              else FeatureMap.add f subtrie map
            with Not_found -> map
          in
          (* if the map is empty, use Empty *)
          if FeatureMap.is_empty map
          then Empty
          else Node map
        | Leaf leaf, [] ->
          let leaf = Leaf.remove leaf t data in
          if Leaf.is_empty leaf
          then Empty
          else Leaf leaf
        | Node _, [] | Leaf _, _::_ ->
          failwith "different feature length in fingerprint trie"
    in
    let features = idx.fp (t) in  (* features of term *)
    { idx with trie = recurse idx.trie features; }

  let remove_ trie = CCFun.uncurry (remove trie)
  let remove_seq dt seq = Iter.fold remove_ dt seq
  let remove_list dt seq = List.fold_left remove_ dt seq

  let iter idx f =
    let rec iter trie f = match trie with
      | Empty -> ()
      | Node map -> FeatureMap.iter (fun _ subtrie -> iter subtrie f) map
      | Leaf leaf -> Leaf.iter leaf f
    in
    iter idx.trie f

  let fold idx f acc =
    let rec fold trie f acc = match trie with
      | Empty -> acc
      | Node map -> FeatureMap.fold (fun _ subtrie acc -> fold subtrie f acc) map acc
      | Leaf leaf -> Leaf.fold leaf acc f
    in
    fold idx.trie f acc

  (** number of indexed terms *)
  let size idx =
    let n = ref 0 in
    iter idx (fun _ _ -> incr n);
    !n

  (** fold on parts of the trie that are compatible with features *)
  let traverse ~compatible idx features k =
    Util.enter_prof prof_traverse;
    (* fold on the trie *)
    let rec recurse trie features =
      match trie, features with
        | Empty, _ -> ()
        | Leaf leaf, [] ->
          k leaf  (* give the leaf to [k] *)
        | Node map, f::features' ->
          (* fold on any subtrie that is compatible with current feature *)
          FeatureMap.iter
            (fun f' subtrie ->
               if compatible f f' then recurse subtrie features')
            map
        | Node _, [] | Leaf _, _::_ ->
          failwith "different feature length in fingerprint trie"
    in
    try
      recurse idx.trie features;
      Util.exit_prof prof_traverse;
    with e ->
      Util.exit_prof prof_traverse;
      raise e

  let retrieve_unifiables_aux fold_unify (idx,sc_idx) t k =
    let features = idx.fp (fst t) in
    let compatible = compatible_features_unif in
    traverse ~compatible idx features
      (fun leaf -> fold_unify (leaf,sc_idx) t k)

  let retrieve_unifiables = retrieve_unifiables_aux Leaf.fold_unify
  
  let retrieve_unifiables_complete ?(unif_alg=JP_unif.unify_scoped) = 
    retrieve_unifiables_aux (Leaf.fold_unify_complete ~unif_alg)

  let retrieve_generalizations ?(subst=S.empty) (idx,sc_idx) t k =
    let features = idx.fp (fst t) in
    (* compatible t1 t2 if t2 can match t1 *)
    let compatible f1 f2 = compatible_features_match f2 f1 in
    traverse ~compatible idx features
      (fun leaf -> Leaf.fold_match ~subst (leaf,sc_idx) t k)

  let retrieve_specializations ?(subst=S.empty) (idx,sc_idx) t k =
    let features = idx.fp (fst t) in
    let compatible = compatible_features_match in
    traverse ~compatible idx features
      (fun leaf -> Leaf.fold_matched ~subst (leaf,sc_idx) t k)

  let to_dot _ _ =
    failwith "Fingerprint: to_dot not implemented"
end
