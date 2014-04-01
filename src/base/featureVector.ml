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


(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

module ST = ScopedTerm
module T = FOTerm
module STbl = Symbol.Tbl
module SMap = Symbol.Map

module Make(C : Index.CLAUSE) = struct
  module C = C

  type feature_vector = int list
    (** a vector of feature *)

  (** {2 Features} *)

  module Feature = struct
    type t = {
      name : string;
      f : Index.lits -> int;
    } (** a function that computes a given feature on clauses *)

    let compute f c = f.f c

    let name f = f.name

    let pp buf f = Buffer.add_string buf f.name
    let fmt fmt f = Format.pp_print_string fmt f.name

    let size_plus =
      { name = "size+";
        f = (fun lits ->
          let cnt = ref 0 in
          Sequence.iter (fun (_,_,sign) -> if sign then incr cnt) lits;
          !cnt);
      }

    let size_minus =
      { name = "size-";
        f = (fun lits ->
          let cnt = ref 0 in
          Sequence.iter (fun (_,_,sign) -> if not sign then incr cnt) lits;
          !cnt);
      }

    let rec _depth_term depth t = match ST.view t with
      | ST.Var _
      | ST.RigidVar _
      | ST.Const _
      | ST.BVar _ -> 0
      | ST.Bind (_, _, t') -> _depth_term (depth+1) t'
      | ST.At (t1, t2) ->
          1 + max (_depth_term depth t1) (_depth_term depth t2)
      | ST.SimpleApp (_, l)
      | ST.Multiset l
      | ST.App (_, l) ->
        let depth' = depth + 1 in
        List.fold_left (fun acc t' -> acc + _depth_term depth' t') depth l
      | _ -> assert false

    (* sum of depths at which symbols occur. Eg f(a, g(b)) will yield 4 (f
       is at depth 0) *)
    let sum_of_depths =
      { name = "sum_of_depths";
        f = (fun lits -> 
          let n = ref 0 in
          Sequence.iter
            (fun (l,r,_) ->
              n := !n + _depth_term 0 (l:T.t:>ST.t) + _depth_term 0 (r:T.t:>ST.t)
            ) lits;
          !n);
      }

    let _select_sign ~sign lits =
      lits |> Sequence.filter (fun (_,_,sign') -> sign = sign')

    let _terms_of_lit (l,r,_) k = k l; k r

    (* sequence of symbols of clause, of given sign *)
    let _symbols ~sign lits =
      _select_sign ~sign lits
        |> Sequence.flatMap _terms_of_lit
        |> Sequence.flatMap T.Seq.symbols

    let count_symb_plus symb =
      { name = Util.sprintf "count+(%a)" Symbol.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:true lits));
      }

    let count_symb_minus symb =
      { name = Util.sprintf "count-(%a)" Symbol.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:false lits));
      }

    (* max depth of the symbol in the term, or -1 *)
    let max_depth_term symb t =
      let symbs_depths = T.Seq.subterms_depth t
        |> Sequence.fmap
          (fun (t,depth) -> match T.Classic.view t with
            | T.Classic.App (s, _, _) when Symbol.eq s symb -> Some depth
            | _ -> None)
      in
      Sequence.max symbs_depths 0

    let _max_depth_lits ~sign symb lits =
      let depth = ref 0 in
      Sequence.iter
        (fun (l,r,sign') -> if sign = sign'
          then depth := max !depth
            (max (max_depth_term symb l) (max_depth_term symb r)))
        lits;
      !depth

    let max_depth_plus symb =
      { name = Util.sprintf "max_depth+(%a)" Symbol.pp symb;
        f = (_max_depth_lits ~sign:true symb);
      }

    let max_depth_minus symb =
      { name = Util.sprintf "max_depth-(%a)" Symbol.pp symb;
        f = (_max_depth_lits ~sign:false symb);
      }
  end

  let compute_fv features lits =
    List.map (fun feat -> feat.Feature.f lits) features

  (** {2 Feature Trie} *)

  module IntMap = Map.Make(struct
    type t = int
    let compare i j = i - j
  end)

  module CSet = Set.Make(struct
    type t = C.t
    let compare = C.cmp
  end)

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

  let features_of_signature ?(ignore=Symbol.TPTP.is_connective) signature =
    (* list of (salience: float, feature) *)
    let features = ref [] in
    (* create features for the symbols *)
    Signature.iter signature
      (fun s ty ->
        if ignore s
          then ()  (* base symbols don't count *)
        else
          let arity = match Type.arity ty with
          | Type.NoArity -> 0
          | Type.Arity (_, i) -> i
          in
          if Type.eq ty Type.TPTP.o
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
    let features = Util.list_take max_features features in
    let features = List.map (fun (_, f) -> f) features in
    let features = default_features @ features in
    Util.debug 2 "FV features: [%a]" (Util.pp_list Feature.pp) features;
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

  let add_seq idx seq = Sequence.fold add idx seq

  let remove idx c =
    let fv = compute_fv idx.features (C.to_lits c) in
    (* remove [c] from the trie *)
    let k set = TrieLeaf (CSet.remove c set) in
    let trie' = goto_leaf idx.trie fv k in
    { idx with trie=trie'; }

  let remove_seq idx seq = Sequence.fold remove idx seq

  (* clauses that subsume (potentially) the given clause *)
  let retrieve_subsuming idx lits acc f =
    (* feature vector of [c] *)
    let fv = compute_fv idx.features lits in
    let rec fold_lower acc fv node = match fv, node with
    | [], TrieLeaf set -> CSet.fold (fun c acc -> f acc c) set acc
    | i::fv', TrieNode map ->
      IntMap.fold
        (fun j subnode acc -> if j <= i
          then fold_lower acc fv' subnode  (* go in the branch *)
          else acc) 
        map acc
    | _ -> failwith "number of features in feature vector changed"
    in
    fold_lower acc fv idx.trie

  (** clauses that are subsumed (potentially) by the given clause *)
  let retrieve_subsumed idx lits acc f =
    (* feature vector of the hc *)
    let fv = compute_fv idx.features lits in
    let rec fold_higher acc fv node = match fv, node with
    | [], TrieLeaf set -> CSet.fold (fun c acc -> f acc c) set acc
    | i::fv', TrieNode map ->
      IntMap.fold
        (fun j subnode acc -> if j >= i
          then fold_higher acc fv' subnode  (* go in the branch *)
          else acc)
        map acc
    | _ -> failwith "number of features in feature vector changed"
    in
    fold_higher acc fv idx.trie

  let retrieve_subsuming_c idx c acc f =
    retrieve_subsuming idx (C.to_lits c) acc f

  let retrieve_subsumed_c idx c acc f =
    retrieve_subsumed idx (C.to_lits c) acc f
end
