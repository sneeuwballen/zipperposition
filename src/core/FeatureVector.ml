
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

module T = Term

type lits = Index_intf.lits

module Make(C : Index.CLAUSE) = struct
  module C = C

  (* TODO use array? *)
  type feature_vector = int list
  (** a vector of feature *)

  (** {2 Features} *)

  module Feature = struct
    type t = {
      name : string;
      f : lits -> int;
    } (** a function that computes a given feature on clauses *)

    let compute f c = f.f c

    let name f = f.name

    let pp out f = CCFormat.string out f.name
    let to_string = CCFormat.to_string pp

    let size_plus =
      { name = "size+";
        f =
          (fun lits ->
             Sequence.filter SLiteral.is_pos lits |> Sequence.length);
      }

    let size_minus =
      { name = "size-";
        f =
          (fun lits ->
             Sequence.filter SLiteral.is_neg lits |> Sequence.length);
      }

    let rec _depth_term depth t = match T.view t with
      | T.Var _
      | T.Const _
      | T.DB _ -> 0
      | T.Fun (_,u) -> _depth_term (depth+1) u
      | T.AppBuiltin (_, l)
      | T.App (_, l) ->
        let depth' = depth + 1 in
        List.fold_left (fun acc t' -> acc + _depth_term depth' t') depth l

    (* sum of depths at which symbols occur. Eg f(a, g(b)) will yield 4 (f
       is at depth 0) *)
    let sum_of_depths =
      { name = "sum_of_depths";
        f = (fun lits ->
          Sequence.fold
            (fun acc lit ->
               SLiteral.fold (fun acc t -> acc + _depth_term 0 t) acc lit
            ) 0 lits);
      }

    let _select_sign ~sign lits =
      lits |> Sequence.filter (fun l -> SLiteral.sign l = sign)

    (* sequence of symbols of clause, of given sign *)
    let _symbols ~sign lits =
      _select_sign ~sign lits
      |> Sequence.flat_map SLiteral.to_seq
      |> Sequence.flat_map T.Seq.symbols

    let count_symb_plus symb =
      { name = CCFormat.sprintf "count+(%a)" ID.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:true lits));
      }

    let count_symb_minus symb =
      { name = CCFormat.sprintf "count-(%a)" ID.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:false lits));
      }

    (* max depth of the symbol in the term, or -1 *)
    let max_depth_term symb t =
      let symbs_depths =
        T.Seq.subterms_depth t
        |> Sequence.filter_map
          (fun (t,depth) -> match T.Classic.view t with
             | T.Classic.App (s, _) when ID.equal s symb -> Some depth
             | _ -> None)
      in
      match Sequence.max symbs_depths with
        | None -> 0
        | Some m -> m

    let _max_depth_lits ~sign symb lits =
      Sequence.fold
        (fun depth lit  ->
           if sign = SLiteral.sign lit
           then
             SLiteral.fold
               (fun depth t -> max depth (max_depth_term symb t))
               depth lit
           else depth
        )
        0 lits

    let max_depth_plus symb =
      { name = CCFormat.sprintf "max_depth+(%a)" ID.pp symb;
        f = (_max_depth_lits ~sign:true symb);
      }

    let max_depth_minus symb =
      { name = CCFormat.sprintf "max_depth-(%a)" ID.pp symb;
        f = (_max_depth_lits ~sign:false symb);
      }
  end

  let compute_fv features lits =
    List.map (fun feat -> feat.Feature.f lits) features

  (** {2 Feature Trie} *)

  module IntMap = Map.Make(CCInt)

  module CSet = Set.Make(struct
      type t = C.t
      let compare = C.compare
    end)

  (* TODO: replace intmap by RAL? or simply a list? or dynamic array *)
  type trie =
    | TrieNode of trie IntMap.t   (** map feature -> trie *)
    | TrieLeaf of CSet.t          (** leaf with a set of clauses *)

  let empty_trie n = match n with
    | TrieNode m when IntMap.is_empty m -> true
    | TrieLeaf set when CSet.is_empty set -> true
    | _ -> false

  (** get/add/remove the leaf for the given list of ints. The
      continuation k takes the leaf, and returns a leaf
      that replaces the old leaf.
      This function returns the new trie. *)
  let goto_leaf trie t k =
    (* the root of the tree *)
    let root = trie in
    (* function to go to the given leaf, building it if needed *)
    let rec goto trie t rebuild =
      match trie, t with
        | (TrieLeaf set) as leaf, [] -> (* found leaf *)
          (match k set with
            | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
            | new_leaf -> rebuild new_leaf)           (* replace by new leaf *)
        | TrieNode m, c::t' ->
          (try  (* insert in subtrie *)
             let subtrie = IntMap.find c m in
             let rebuild' subtrie = match subtrie with
               | _ when empty_trie subtrie -> rebuild (TrieNode (IntMap.remove c m))
               | _ -> rebuild (TrieNode (IntMap.add c subtrie m))
             in
             goto subtrie t' rebuild'
           with Not_found -> (* no subtrie found *)
             let subtrie = if t' = []
               then TrieLeaf CSet.empty
               else TrieNode IntMap.empty
             and rebuild' subtrie = match subtrie with
               | _ when empty_trie subtrie -> rebuild (TrieNode (IntMap.remove c m))
               | _ -> rebuild (TrieNode (IntMap.add c subtrie m))
             in
             goto subtrie t' rebuild')
        | TrieNode _, [] -> assert false (* ill-formed term *)
        | TrieLeaf _, _ -> assert false  (* wrong arity *)
    in
    goto trie t (fun t -> t)

  (** {2 Subsumption Index} *)

  type t = {
    trie : trie;
    features : Feature.t list;
  }

  let empty_with features = {
    trie = TrieNode IntMap.empty;
    features;
  }

  let name = "feature_vector_idx"

  let features idx = idx.features

  let default_features =
    [ Feature.size_plus; Feature.size_minus;
      Feature.sum_of_depths ]

  let empty () = empty_with default_features

  (** maximam number of features in addition to basic ones *)
  let max_features = 25

  let features_of_signature ?(ignore=fun _ -> false) sigma =
    (* list of (salience: float, feature) *)
    let features = ref [] in
    (* create features for the symbols *)
    Signature.iter sigma
      (fun s ty ->
         if ignore s
         then ()  (* base symbols don't count *)
         else
           let arity = match Type.arity ty with
             | Type.NoArity -> 0
             | Type.Arity (_, i) -> i
           in
           if Type.equal ty Type.TPTP.o
           then features := [1 + arity, Feature.count_symb_plus s;
                             1 + arity, Feature.count_symb_minus s]
               @ !features
           else
             features := [0, Feature.max_depth_plus s;
                          0, Feature.max_depth_minus s;
                          1 + arity, Feature.count_symb_plus s;
                          1 + arity, Feature.count_symb_minus s]
               @ !features);
    (* only take a limited number of features *)
    let features = List.sort (fun (s1,_) (s2,_) -> s2 - s1) !features in
    let features = CCList.take max_features features in
    let features = List.map (fun (_, f) -> f) features in
    let features = default_features @ features in
    Util.debugf 2 "FV features: [%a]" (fun k->k (CCFormat.list Feature.pp) features);
    features

  let of_signature signature =
    let features = features_of_signature signature in
    empty_with features

  let add idx c =
    (* feature vector of [c] *)
    let fv = compute_fv idx.features (C.to_lits c) in
    (* insertion *)
    let k set = TrieLeaf (CSet.add c set) in
    let trie' = goto_leaf idx.trie fv k in
    { idx with trie=trie'; }

  let add_seq = Sequence.fold add
  let add_list = List.fold_left add

  let remove idx c =
    let fv = compute_fv idx.features (C.to_lits c) in
    (* remove [c] from the trie *)
    let k set = TrieLeaf (CSet.remove c set) in
    let trie' = goto_leaf idx.trie fv k in
    { idx with trie=trie'; }

  let remove_seq idx seq = Sequence.fold remove idx seq

  (* clauses that subsume (potentially) the given clause *)
  let retrieve_subsuming idx lits _ f =
    (* feature vector of [c] *)
    let fv = compute_fv idx.features lits in
    let rec fold_lower fv node = match fv, node with
      | [], TrieLeaf set -> CSet.iter f set
      | i::fv', TrieNode map ->
        IntMap.iter
          (fun j subnode -> if j <= i
            then fold_lower fv' subnode  (* go in the branch *)
            else ())
          map
      | _ -> failwith "number of features in feature vector changed"
    in
    fold_lower fv idx.trie

  (** clauses that are subsumed (potentially) by the given clause *)
  let retrieve_subsumed idx lits _ f =
    (* feature vector of the hc *)
    let fv = compute_fv idx.features lits in
    let rec fold_higher fv node = match fv, node with
      | [], TrieLeaf set -> CSet.iter f set
      | i::fv', TrieNode map ->
        IntMap.iter
          (fun j subnode -> if j >= i
            then fold_higher fv' subnode  (* go in the branch *)
            else ())
          map
      | _ -> failwith "number of features in feature vector changed"
    in
    fold_higher fv idx.trie

  (** clauses that are potentially alpha-equivalent to the given clause*)
  let retrieve_alpha_equiv idx lits _ f =
    (* feature vector of the hc *)
    let fv = compute_fv idx.features lits in
    let rec fold_higher fv node = match fv, node with
      | [], TrieLeaf set -> CSet.iter f set
      | i::fv', TrieNode map ->
        IntMap.iter
          (fun j subnode -> if j = i
            then fold_higher fv' subnode  (* go in the branch *)
            else ())
          map
      | _ -> failwith "number of features in feature vector changed"
    in
    fold_higher fv idx.trie

  let retrieve_subsuming_c idx c f =
    retrieve_subsuming idx (C.to_lits c) (C.labels c) f

  let retrieve_subsumed_c idx c f =
    retrieve_subsumed idx (C.to_lits c) (C.labels c) f

  let retrieve_alpha_equiv_c idx c f =
    retrieve_alpha_equiv idx (C.to_lits c) (C.labels c) f

  let iter idx f =
    let rec iter = function
      | TrieLeaf set -> CSet.iter f set
      | TrieNode map -> IntMap.iter (fun _ t' -> iter t') map
    in
    iter idx.trie

  let fold f acc idx =
    let rec fold acc = function
      | TrieLeaf set -> CSet.fold (fun x acc -> f acc x) set acc
      | TrieNode map -> IntMap.fold (fun _ t' acc -> fold acc t') map acc
    in
    fold acc idx.trie
end
