(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

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

    let rec _depth_term depth t = match t.T.term with
      | T.Var _
      | T.BoundVar _ -> 0
      | T.Node (_, _, l) ->
        let depth' = depth + 1 in
        List.fold_left (fun acc t' -> acc + _depth_term depth' t') depth l

    (* sum of depths at which symbols occur. Eg f(a, g(b)) will yield 4 (f
       is at depth 0) *)
    let sum_of_depths =
      { name = "sum_of_depths";
        f = (fun lits -> 
          let n = ref 0 in
          Sequence.iter (fun (l,r,_) -> n := !n + _depth_term 0 l + _depth_term 0 r) lits;
          !n);
      }

    (** Count the number of distinct split symbols *)
    let count_split_symb =
      { name = "count_split_symb";
        f = (fun lits ->
          let table = STbl.create 3 in
          let rec gather t = match t.T.term with
          | T.Node (s, _, l) ->
            (if Symbol.has_flag Symbol.flag_split s
              then STbl.replace table s ());
            List.iter gather l
          | T.BoundVar _ | T.Var _ -> ()
          in
          Sequence.iter (fun (l,r,_) -> gather l; gather r) lits;
          STbl.length table);
      }

    (** Count the number of distinct skolem symbols *)
    let count_skolem_symb =
      { name = "count_skolem_symb";
        f = (fun lits ->
          let table = STbl.create 3 in
          let rec gather t = match t.T.term with
          | T.Node (s, _, l) ->
            (if Symbol.has_flag Symbol.flag_skolem s
              then STbl.replace table s ());
            List.iter gather l
          | T.BoundVar _ | T.Var _ -> ()
          in
          Sequence.iter (fun (l,r,_) -> gather l; gather r) lits;
          STbl.length table);
      }

    (* iterate on symbols of a term *)
    let rec _iter_symb t k = match t.T.term with
      | T.Var _ | T.BoundVar _ -> ()
      | T.Node (s, _, l) -> k s; List.iter (fun t -> _iter_symb t k) l

    (* sequence of symbols of clause, of given sign *)
    let _symbols ~sign lits =
      Sequence.from_iter
        (fun k -> Sequence.iter
            (fun (l,r,sign') -> if sign = sign' then _iter_symb l k; _iter_symb r k)
            lits)

    let count_symb_plus symb =
      { name = Util.sprintf "count+(%a)" Symbol.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:true lits));
      }

    let count_symb_minus symb =
      { name = Util.sprintf "count-(%a)" Symbol.pp symb;
        f = (fun lits -> Sequence.length (_symbols ~sign:false lits));
      }

    (* max depth of the symbol in the term, or -1 *)
    let rec max_depth_term symb t depth =
      match t.T.term with
      | T.Var _ | T.BoundVar _ -> -1
      | T.Node (s, _, l) ->
        let cur_depth = if Symbol.eq s symb then depth else -1 in
        List.fold_left
          (fun maxdepth subterm -> max maxdepth (max_depth_term symb subterm (depth+1)))
          cur_depth l

    let max_depth_plus symb =
      { name = Util.sprintf "max_depth+(%a)" Symbol.pp symb;
        f = (fun lits ->
          let depth = ref 0 in
          Sequence.iter
            (fun (l,r,sign) -> if sign
              then depth := max !depth
                (max (max_depth_term symb l 0) (max_depth_term symb r 0)))
            lits;
          !depth);
      }

    let max_depth_minus symb =
      { name = Util.sprintf "max_depth-(%a)" Symbol.pp symb;
        f = (fun lits ->
          let depth = ref 0 in
          Sequence.iter
            (fun (l,r,sign) -> if not sign
              then depth := max !depth
                (max (max_depth_term symb l 0) (max_depth_term symb r 0)))
            lits;
          !depth);
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
    [ Feature.size_plus; Feature.size_minus; Feature.count_skolem_symb;
      Feature.count_split_symb; Feature.sum_of_depths ]

  let empty () = empty_with default_features

  (** maximam number of features in addition to basic ones *)
  let max_features = 25

  let features_of_signature ?(ignore=Symbol.is_connective) signature =
    (* list of (salience: float, feature) *)
    let features = ref [] in
    (* create features for the symbols *)
    Signature.iter signature
      (fun _ s ->
        let ty = Symbol.ty s in
        let _, arity = Type.arity ty in
        if ignore s
          then ()  (* base symbols don't count *)
        else if Type.eq ty Type.o
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
