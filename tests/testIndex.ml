
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Test indexing structures} *)

open Logtk
open Logtk_arbitrary

module T = Term

(* a simple instance of equation *)
module E : Index.EQUATION with type rhs = int and type t = T.t * int = struct
  type t = T.t * int
  type rhs = int
  let compare (t1,i1) (t2,i2) = if t1 == t2 then i1-i2 else T.compare t1 t2
  let extract (t,i) = t, i, true
  let priority _ = 1
end

module type UnitIndex = sig
  include Index.UNIT_IDX with module E = E
  val name : string
end

(* lists of unique terms *)
let gen low high =
  QCheck.Gen.(
    let t = (1 -- 7) >>= ArTerm.default_ho_fuel in
    list_size (low -- high) t >>= fun l ->
    let seq = List.map Lambda.snf l |> T.Set.of_list |> T.Set.to_seq in
    let seq = Sequence.mapi (fun i t -> t, i) seq in
    return (Sequence.to_list seq)
  )

let pp l =
  let pp out (t,i) = Format.fprintf out "@[<2>`@[<2>%a@]`@ -> %d@]" T.pp t i in
  CCFormat.to_string (CCFormat.list pp) l

let arb i j =
  let shrink_t = ArTerm.shrink in
  let shrink = QCheck.Shrink.(list ~shrink:(pair shrink_t int)) in
  QCheck.make ~shrink ~print:pp (gen i j)

let long_factor = 10

(* test unit index *)
module TestUnit(I : UnitIndex) = struct
  (* check that the size of index is correct *)
  let check_size_add =
    let prop l =
      let idx = I.add_list (I.empty ()) l in
      List.length l = I.size idx
    in
    let name = CCFormat.sprintf "index(%s)_size_after_add" I.name in
    QCheck.Test.make ~name (arb 30 100) prop

  (* list of (term,int) that generalize [t] *)
  let find_all idx t =
    I.retrieve ~sign:true (idx,1) (t,0)
    |> Sequence.map (fun (t',i,_,_) -> t', i)
    |> Sequence.to_rev_list

  (* check that at least the terms are retrieved *)
  let check_gen_retrieved_member =
    let prop l =
      let idx = I.add_list (I.empty ()) l in
      List.for_all
        (fun (t,i) ->
          let retrieved = find_all idx t in
          (* [i] must occur in the list *)
          List.exists (fun (_, i') -> i=i') retrieved)
      l
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_member" I.name in
    QCheck.Test.make ~long_factor ~name (arb 30 100) prop

  (* check that the retrieved terms match the query *)
  let check_gen_retrieved_match =
    let prop l =
      let idx = I.add_list (I.empty ()) l in
      List.for_all
        (fun (t,_) ->
          let retrieved = find_all idx t in
          (* all terms must match [t] *)
          List.for_all
            (fun (t',_) -> Unif.FO.matches ~pattern:t' t)
            retrieved)
        l
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_match" I.name in
    QCheck.Test.make ~long_factor ~name (arb 50 150) prop

  (* check that all matching terms are retrieved *)
  let check_all_matching_are_retrieved =
    let prop l =
      let idx = I.add_list (I.empty ()) l in
      List.for_all
        (fun (t,_) ->
          let retrieved = find_all idx t in
          List.for_all
            (fun (t',_) ->
               if Unif.FO.matches ~pattern:t' t
               then
                 List.exists
                   (fun (t'',_) -> T.equal t' t'')
                   retrieved
               else true)
            l)
        l
    in
    let name = CCFormat.sprintf "index(%s)_all_matching_are_retrieved" I.name in
    QCheck.Test.make ~long_factor ~name (arb 50 150) prop

  (* check the matching of generalization *)
  let props =
    [ check_size_add
    ; check_gen_retrieved_member
    ; check_gen_retrieved_match
    ; check_all_matching_are_retrieved
    ]
end

(* test term index *)

module type TermIndex = sig
  include Index.TERM_IDX with type elt = int
  val name : string
end

module TestTerm(I : TermIndex) = struct
  (* check that the size of index is correct *)
  let check_size_add =
    let prop l =
      let idx = I.add_list (I.empty()) l in
      List.length l = I.size idx
    in
    let name = CCFormat.sprintf "index(%s)_size_after_add" I.name in
    QCheck.Test.make ~name (arb 10 100) prop

  (* list of (term,int) that can be retrieved using [retrieve] in [t] *)
  let find_all retrieve idx s_idx t s_t =
    retrieve ?subst:None (idx,s_idx) (t,s_t)
    |> Sequence.fold
      (fun acc (t',i,_) -> (t', i) :: acc)
      []

  (* check that at least the terms are retrieved *)
  let check_gen_retrieved_member =
    let prop l =
      let idx = I.add_list (I.empty()) l in
      List.for_all
        (fun (t,i) ->
          let retrieved = find_all I.retrieve_unifiables idx 0 t 1 in
          (* [i] must occur in the list *)
          List.exists (fun (_, i') -> i=i') retrieved)
      l
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_member" I.name in
    QCheck.Test.make ~name (arb 10 100) prop

  (* check that the retrieved terms satisfy the given properry w.r.t the query *)
  let _check_all_retrieved_satisfy retrieve check l =
    let idx = I.add_list (I.empty()) l in
    List.for_all
      (fun (t,_) ->
        let retrieved = find_all retrieve idx 1 t 0 in
        (* all terms must match [t] *)
        List.for_all
          (fun (t',_) ->
            try ignore (check (t,0) (t',1)); true
            with Unif.Fail ->
              Util.debugf 1 "problem with %a and %a" (fun k->k T.pp t T.pp t');
              false)
          retrieved)
    l

  (* check that all terms that satisfy the relation with query are retrieved *)
  let _check_all_satisfying_are_retrieved retrieve check l =
    let idx = I.add_list (I.empty()) l in
    List.for_all
      (fun (t,_) ->
        let retrieved = find_all retrieve idx 1 t 0 in
        List.for_all
          (fun (t',i') ->
            try
              let _ = check (t,0) (t',1) in
              List.exists
                (fun (_,i'') -> i' = i'')
                retrieved
            with Unif.Fail -> true)
          l)
      l

  let _match_flip ?subst t1 t2 =
    Unif.FO.matching ?subst ~pattern:t2 t1

  let size = Sequence.length
  let pp l =
    CCFormat.to_string
      (fun out l ->
        Format.fprintf out "@[<hv>%a@]"
          (CCFormat.seq
            (fun out (t,i) ->
              Format.fprintf out "@[<1>@[%a@] ->@ %d@]" T.pp t i))
          l)
      l

  let _count = 100
  let _limit = _count + 100

  let arb_low_ = 10
  let arb_high_ = 100

  let check_retrieved_unify =
    let prop = _check_all_retrieved_satisfy I.retrieve_unifiables Unif.FO.unify_syn in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_unify" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  let check_retrieved_specializations =
    let prop = _check_all_retrieved_satisfy I.retrieve_specializations
      (fun t1 t2 -> Unif.FO.matching ~pattern:t1 t2) in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_specializations" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  let check_retrieved_generalizations =
    let prop = _check_all_retrieved_satisfy I.retrieve_generalizations _match_flip in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_generalizations" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  let check_retrieve_all_unify =
    let prop = _check_all_satisfying_are_retrieved I.retrieve_unifiables Unif.FO.unify_syn in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_unify" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  let check_retrieve_all_specializations =
    let prop = _check_all_satisfying_are_retrieved I.retrieve_specializations
      (fun t1 t2 -> Unif.FO.matching ~pattern:t1 t2) in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_specializations" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  let check_retrieve_all_generalizations =
    let prop = _check_all_satisfying_are_retrieved I.retrieve_generalizations _match_flip in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_generalizations" I.name in
    QCheck.Test.make ~name ~count:_count ~max_gen:_limit (arb arb_low_ arb_high_) prop

  (* check the matching of generalization *)
  let props =
    [ check_size_add
    ; check_retrieved_unify
    ; check_retrieved_generalizations
    ; check_retrieved_specializations
    ; check_retrieve_all_unify
    ; check_retrieve_all_generalizations
    ; check_retrieve_all_specializations
    ]
end

module OrderedInt = struct type t = int let compare i j = i-j end

(** {2 Properties} *)

let props =
  let module DT = struct include Dtree.Make(E) let name = "dtree" end in
  let module TestDtree = TestUnit(DT) in
  let module NPDT = struct include NPDtree.Make(E) let name = "npdtree" end in
  let module TestNPDtree = TestUnit(NPDT) in
  let module IntFingerprint = Fingerprint.Make(OrderedInt) in
  let module TestFingerprint = TestTerm(IntFingerprint) in
  let module IntNPDtree = NPDtree.MakeTerm(OrderedInt) in
  let module TestIntNPDTree = TestTerm(IntNPDtree) in
  List.flatten
    [ TestDtree.props
    ; TestNPDtree.props
    ; TestFingerprint.props
    ; TestIntNPDTree.props
    ]
