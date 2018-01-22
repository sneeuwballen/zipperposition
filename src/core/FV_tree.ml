
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

module T = Term
module Fmt = CCFormat

type labels = Index_intf.labels

type feature =
  | N of int
  | S of ID.Set.t
  | M of int ID.Map.t
  | L of labels

type feature_vector = feature IArray.t
(** a vector of feature *)

let mk_n i = N i
let mk_s s = S s
let mk_m m = M m
let mk_l s = L s

module Feature : sig
  type t = feature
  include Interfaces.ORD with type t := t
  include Interfaces.EQ with type t := t
  include Interfaces.PRINT with type t := t
  val add : t -> t -> t
  val max : t -> t -> t
  val leq : t -> t -> bool
end = struct
  type t = feature

  let pp out (f:t): unit = match f with
    | N i -> Fmt.int out i
    | S s -> Fmt.fprintf out "(@[set@ %a@])" (Fmt.seq ID.pp) (ID.Set.to_seq s)
    | M m ->
      Fmt.fprintf out "(@[mset@ %a@])"
        Fmt.(seq (pair ~sep:silent ID.pp int)) (ID.Map.to_seq m)
    | L l ->
      Fmt.fprintf out "(@[labels@ %a@])" Fmt.(seq int) (Util.Int_set.to_seq l)

  let to_string = Fmt.to_string pp

  let compare f1 f2 : int =
    let to_int = function N _ -> 0 | S _ -> 1 | M _ -> 2 | L _ -> 3 in
    match f1, f2 with
      | N i1, N i2 -> CCInt.compare i1 i2
      | S s1, S s2 -> ID.Set.compare s1 s2
      | M m1, M m2 -> ID.Map.compare CCInt.compare m1 m2
      | L m1, L m2 -> Util.Int_set.compare m1 m2
      | N _, _
      | S _, _
      | M _, _
      | L _, _
        -> CCInt.compare (to_int f1)(to_int f2)

  let equal f1 f2 = compare f1 f2 = 0

  (* combination of features *)
  let combine_ ~default ~op f1 f2: feature = match f1, f2 with
    | N i1, N i2 -> mk_n (i1+i2)
    | S s1, S s2 -> mk_s (ID.Set.union s1 s2)
    | M m1, M m2 ->
      ID.Map.merge
        (fun _ o1 o2 ->
           Some (op (CCOpt.get_or ~default o1) (CCOpt.get_or ~default o2)))
        m1 m2
      |> mk_m
    | L s1, L s2 -> Util.Int_set.union s1 s2 |> mk_l
    | N _, _
    | S _, _
    | M _, _
    | L _, _
      -> assert false

  let add = combine_ ~default:0 ~op:(+)
  let max = combine_ ~default:0 ~op:max

  let leq (f1:t) (f2:t): bool = match f1, f2 with
    | N i1, N i2 -> i1 <= i2
    | S s1, S s2 -> ID.Set.subset s1 s2
    | M m1, M m2 ->
      ID.Map.for_all
        (fun k i1 -> i1 <= ID.Map.get_or ~default:~-1 k m2)
        m1
    | L s1, L s2 -> Util.Int_set.subset s1 s2
    | N _, _
    | S _, _
    | M _, _
    | L _, _
      -> assert false
end

(** {2 Features} *)

module Make(C: Index_intf.CLAUSE) = struct
  module C = C

  module Feature_fun = struct
    type t = {
      name : string;
      f : Index_intf.lits -> Index_intf.labels -> feature;
    } (** a function that computes a given feature on clauses *)

    let compute f c = f.f (C.to_lits c) (C.labels c)

    let name f = f.name

    let pp out f = Fmt.string out f.name
    let to_string = Fmt.to_string pp

    let make name f = {name; f}

    let size_plus =
      make "size+"
        (fun lits _ ->
           Sequence.filter SLiteral.is_pos lits |> Sequence.length |> mk_n)

    let size_minus =
      make "size-"
        (fun lits _ ->
           Sequence.filter SLiteral.is_neg lits |> Sequence.length |> mk_n)

    let weight_lit lit =
      SLiteral.to_seq lit |> Sequence.map T.weight |> Sequence.fold (+) 0

    let weight_ name filter =
      make name
        (fun lits _ ->
           Sequence.filter filter lits
           |> Sequence.map weight_lit
           |> Sequence.fold (+) 0
           |> mk_n)

    let weight_plus = weight_ "weight+" SLiteral.is_pos
    let weight_minus = weight_ "weight-" SLiteral.is_neg

    let labels = make "labels" (fun _ labels -> mk_l labels)

    (* sequence of symbols of clause, of given sign *)
    let symbols_ filter lits : ID.t Sequence.t =
      lits
      |> Sequence.filter filter
      |> Sequence.flat_map SLiteral.to_seq
      |> Sequence.flat_map T.Seq.symbols

    let set_sym_ filter lits _ =
      symbols_ filter lits
      |> ID.Set.of_seq
      |> mk_s

    let set_sym_plus =
      make "set_symb+" (set_sym_ SLiteral.is_pos)

    let set_sym_minus =
      make "set_symb-" (set_sym_ SLiteral.is_neg)

    let multiset_sym_ filter lits _ =
      symbols_ filter lits
      |> Sequence.fold
        (fun m id ->
           let n = ID.Map.get_or ~default:0 id m in
           ID.Map.add id (n+1) m)
        ID.Map.empty
      |> mk_m

    let multiset_sym_plus =
      make "mset_symb+" (multiset_sym_ SLiteral.is_pos)

    let multiset_sym_minus =
      make "mset_symb-" (multiset_sym_ SLiteral.is_neg)

    (* sequence of symbols of clause with their depth *)
    let symbols_depth_ filter lits : (ID.t * int) Sequence.t =
      lits
      |> Sequence.filter filter
      |> Sequence.flat_map SLiteral.to_seq
      |> Sequence.flat_map T.Seq.subterms_depth
      |> Sequence.filter_map
        (fun (t,d) -> match T.view t with
           | T.Const id -> Some (id,d)
           | _ -> None)

    let depth_sym_ filter lits _ =
      symbols_depth_ filter lits
      |> Sequence.fold
        (fun m (id, d) ->
           let d' = ID.Map.get_or ~default:0 id m in
           ID.Map.add id (max d d') m)
        ID.Map.empty
      |> mk_m

    let depth_sym_plus =
      make "depth_sym+" (depth_sym_ SLiteral.is_pos)

    let depth_sym_minus =
      make "depth_sym-" (depth_sym_ SLiteral.is_neg)
  end

  type feature_funs = Feature_fun.t IArray.t

  let compute_fv funs lits labels : feature_vector =
    IArray.map (fun feat -> feat.Feature_fun.f lits labels) funs

  let compute_fv_c funs c : feature_vector = compute_fv funs (C.to_lits c) (C.labels c)

  (** {2 Feature Trie} *)

  module Feat_map = CCMap.Make(Feature)

  module C_set = Set.Make(C)

  (* TODO: replace intmap by RAL? or simply a list? or dynamic array *)
  type trie =
    | TrieNode of trie Feat_map.t (** map feature -> trie *)
    | TrieLeaf of C_set.t (** leaf with a set of clauses *)

  let empty_trie n = match n with
    | TrieNode m when Feat_map.is_empty m -> true
    | TrieLeaf set when C_set.is_empty set -> true
    | _ -> false

  (* get/add/remove the leaf for the given list of ints. The
     continuation k takes the leaf, and returns a leaf
     that replaces the old leaf.
     This function returns the new trie. *)
  let goto_leaf (trie:trie) (fv:feature_vector) k =
    (* the root of the tree *)
    let root = trie in
    (* function to go to the given leaf, building it if needed *)
    let rec goto trie i rebuild =
      if i = IArray.length fv
      then match trie with
        | (TrieLeaf set) as leaf -> (* found leaf *)
          begin match k set with
            | new_leaf when leaf == new_leaf -> root (* no change, return same tree *)
            | new_leaf -> rebuild new_leaf (* replace by new leaf *)
          end
        | TrieNode _ -> assert false
      else match trie, IArray.get fv i with
        | TrieNode m, c ->
          begin
            try  (* insert in subtrie *)
              let subtrie = Feat_map.find c m in
              let rebuild' subtrie = match subtrie with
                | _ when empty_trie subtrie -> rebuild (TrieNode (Feat_map.remove c m))
                | _ -> rebuild (TrieNode (Feat_map.add c subtrie m))
              in
              goto subtrie (i+1) rebuild'
            with Not_found -> (* no subtrie found *)
              let subtrie =
                if i+1 = IArray.length fv
                then TrieLeaf C_set.empty
                else TrieNode Feat_map.empty
              and rebuild' subtrie = match subtrie with
                | _ when empty_trie subtrie -> rebuild (TrieNode (Feat_map.remove c m))
                | _ -> rebuild (TrieNode (Feat_map.add c subtrie m))
              in
              goto subtrie (i+1) rebuild'
          end
        | TrieLeaf _, _ -> assert false  (* wrong arity *)
    in
    goto trie 0 (fun t -> t)

  (** {2 Subsumption Index} *)

  type t = {
    trie : trie;
    funs : feature_funs;
  }

  let empty_with funs = {
    trie = TrieNode Feat_map.empty;
    funs;
  }

  let name = "feature_vector_idx"

  let feature_funs idx = idx.funs

  (* NOTE: order of features matter. Put first
     the ones with coarse discrimination and cheap features (integers),
     to prune early and to ensure some sharing. Very accurate features
     should come last, where most instances have been pruned already. *)
  let default_feature_funs : feature_funs =
    IArray.of_list
      [ Feature_fun.size_plus;
        Feature_fun.size_minus;
        Feature_fun.weight_plus;
        Feature_fun.weight_minus;
        Feature_fun.labels;
        Feature_fun.set_sym_plus;
        Feature_fun.set_sym_minus;
        Feature_fun.depth_sym_plus;
        Feature_fun.depth_sym_minus;
        Feature_fun.multiset_sym_plus;
        Feature_fun.multiset_sym_minus;
      ]

  let empty () = empty_with default_feature_funs

  let add idx c =
    (* feature vector of [c] *)
    let fv = compute_fv_c idx.funs c in
    (* insertion *)
    let k set = TrieLeaf (C_set.add c set) in
    let trie' = goto_leaf idx.trie fv k in
    { idx with trie=trie'; }

  let add_seq = Sequence.fold add
  let add_list = List.fold_left add

  let remove idx c =
    let fv = compute_fv_c idx.funs c in
    (* remove [c] from the trie *)
    let k set = TrieLeaf (C_set.remove c set) in
    let trie' = goto_leaf idx.trie fv k in
    { idx with trie=trie'; }

  let remove_seq idx seq = Sequence.fold remove idx seq

  (* retrieve all clauses which have a feature vector [fv] such that
     [check (compute c).(i) fv.(i) = true] *)
  let retrieve_ ~check idx lits labels f: unit =
    (* feature vector of the hc *)
    let fv = compute_fv idx.funs lits labels in
    let rec fold_higher i node =
      if i = IArray.length fv
      then match node with
        | TrieLeaf set -> C_set.iter f set
        | TrieNode _ -> assert false
      else match node, IArray.get fv i with
        | TrieNode map, feat ->
          Feat_map.iter
            (fun feat' subnode ->
               if check ~feat_query:feat ~feat_tree:feat'
               then fold_higher (i+1) subnode  (* go in the branch *)
               else ())
            map
        | TrieLeaf _, _ -> assert false
    in
    fold_higher 0 idx.trie

  (* clauses that subsume (potentially) the given clause *)
  let retrieve_subsuming idx lits labels f: unit =
    retrieve_ idx lits labels f
      ~check:(fun ~feat_query ~feat_tree ->
        Feature.leq feat_tree feat_query)

  (* clauses that are subsumed (potentially) by the given clause *)
  let retrieve_subsumed idx lits labels f: unit =
    retrieve_ idx lits labels f
      ~check:(fun ~feat_query ~feat_tree ->
        Feature.leq feat_query feat_tree)

  (* clauses that are potentially alpha-equivalent to the given clause*)
  let retrieve_alpha_equiv idx lits labels f: unit =
    retrieve_ idx lits labels f
      ~check:(fun ~feat_query ~feat_tree ->
        Feature.equal feat_query feat_tree)

  let retrieve_subsuming_c idx c f =
    retrieve_subsuming idx (C.to_lits c) (C.labels c) f

  let retrieve_subsumed_c idx c f =
    retrieve_subsumed idx (C.to_lits c) (C.labels c) f

  let retrieve_alpha_equiv_c idx c f =
    retrieve_alpha_equiv idx (C.to_lits c) (C.labels c) f

  let iter idx f =
    let rec iter = function
      | TrieLeaf set -> C_set.iter f set
      | TrieNode map -> Feat_map.iter (fun _ t' -> iter t') map
    in
    iter idx.trie

  let fold f acc idx =
    let rec fold acc = function
      | TrieLeaf set -> C_set.fold (fun x acc -> f acc x) set acc
      | TrieNode map -> Feat_map.fold (fun _ t' acc -> fold acc t') map acc
    in
    fold acc idx.trie
end
