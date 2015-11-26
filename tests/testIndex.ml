
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

(** {1 Test indexing structures} *)

open Logtk
open Logtk_arbitrary
open QCheck

module T = FOTerm

(* a simple instance of equation *)
module E : Index.EQUATION with type rhs = int and type t = T.t * int = struct
  type t = T.t * int
  type rhs = int
  let compare (t1,i1) (t2,i2) = if t1 == t2 then i1-i2 else T.cmp t1 t2
  let extract (t,i) = t, i, true
  let priority _ = 1
end

module type UnitIndex = sig
  include Index.UNIT_IDX with module E = E
  val name : string
end

(* test unit index *)
module TestUnit(I : UnitIndex) = struct
  (* lists of unique terms *)
  let gen low high = Arbitrary.(
    list ~len:(low -- high) ArTerm.default >>= fun l ->
    let seq = T.Set.of_list l |> T.Set.to_seq in
    let seq = Sequence.mapi (fun i t -> t, i) seq in
    return (Sequence.persistent seq))

  let pp seq =
    let pp out (t,i) = Format.fprintf out "%a -> %d" T.pp t i in
    CCFormat.to_string (CCFormat.seq pp) seq

  (* check that the size of index is correct *)
  let check_size_add =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.length seq = I.size idx
    in
    let name = CCFormat.sprintf "index(%s)_size_after_add" I.name in
    mk_test ~name (gen 30 100) prop

  (* list of (term,int) that generalize [t] *)
  let find_all idx t =
    I.retrieve ~sign:true idx 1 t 0 []
      (fun acc t' i _ _ -> (t', i) :: acc)

  (* check that at least the terms are retrieved *)
  let check_gen_retrieved_member =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.for_all
        (fun (t,i) ->
          let retrieved = find_all idx t in
          (* [i] must occur in the list *)
          List.exists (fun (_, i') -> i=i') retrieved)
      seq
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_member" I.name in
    mk_test ~name (gen 30 100) prop

  (* check that the retrieved terms match the query *)
  let check_gen_retrieved_match =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.for_all
        (fun (t,_) ->
          let retrieved = find_all idx t in
          (* all terms must match [t] *)
          List.for_all
            (fun (t',_) ->
              try ignore (Unif.FO.matching ~pattern:t' 0 t 1); true
              with Unif.Fail -> false)
            retrieved)
      seq
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_match" I.name in
    mk_test ~name (gen 50 150) prop

  (* check that all matching terms are retrieved *)
  let check_all_retrieved =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.for_all
        (fun (t,_) ->
          let retrieved = find_all idx t in
          Sequence.for_all
            (fun (t',_) ->
              try
                let _ = Unif.FO.matching ~pattern:t' 1 t 0 in
                List.exists
                  (fun (t'',_) -> T.eq t' t'')
                  retrieved
              with Unif.Fail -> true)
            seq)
        seq
    in
    let name = CCFormat.sprintf "index(%s)_all_retrieved" I.name in
    mk_test ~name (gen 50 150) prop

  (* check the matching of generalization *)
  let props =
    [ check_size_add
    ; check_gen_retrieved_member
    ; check_gen_retrieved_match
    ; check_all_retrieved
    ]
end

(* test term index *)

module type TermIndex = sig
  include Index.TERM_IDX with type elt = int
  val name : string
end

module TestTerm(I : TermIndex) = struct
  (* lists of unique terms *)
  let gen low high = Arbitrary.(
    list ~len:(low -- high) ArTerm.default >>= fun l ->
    let seq = T.Set.of_list l |> T.Set.to_seq in
    let seq = Sequence.mapi (fun i t -> t, i) seq in
    return (Sequence.persistent seq))

  (* check that the size of index is correct *)
  let check_size_add =
    let prop seq =
      let idx = Sequence.fold (fun idx (t,i) -> I.add idx t i) (I.empty ()) seq in
      Sequence.length seq = I.size idx
    in
    let name = CCFormat.sprintf "index(%s)_size_after_add" I.name in
    mk_test ~name (gen 10 100) prop

  (* list of (term,int) that can be retrieved using [retrieve] in [t] *)
  let find_all retrieve idx s_idx t s_t =
    retrieve ?subst:None idx s_idx t s_t []
      (fun acc t' i _ -> (t', i) :: acc)

  (* check that at least the terms are retrieved *)
  let check_gen_retrieved_member =
    let prop seq =
      let idx = Sequence.fold (fun idx (t,i) -> I.add idx t i) (I.empty ()) seq in
      Sequence.for_all
        (fun (t,i) ->
          let retrieved = find_all I.retrieve_unifiables idx 0 t 1 in
          (* [i] must occur in the list *)
          List.exists (fun (_, i') -> i=i') retrieved)
      seq
    in
    let name = CCFormat.sprintf "index(%s)_gen_retrieved_member" I.name in
    mk_test ~name (gen 10 100) prop

  (* check that the retrieved terms satisfy the given properry w.r.t the query *)
  let _check_all_retrieved_satisfy retrieve check seq =
    let idx = Sequence.fold (fun idx (t,i) -> I.add idx t i) (I.empty ()) seq in
    Sequence.for_all
      (fun (t,_) ->
        let retrieved = find_all retrieve idx 1 t 0 in
        (* all terms must match [t] *)
        List.for_all
          (fun (t',_) ->
            try ignore (check t 0 t' 1); true
            with Unif.Fail ->
              Util.debug 1 "problem with %a and %a" (fun k->k T.pp t T.pp t');
              false)
          retrieved)
    seq

  (* check that all terms that satisfy the relation with query are retrieved *)
  let _check_all_satisfying_are_retrieved retrieve check seq =
    let idx = Sequence.fold (fun idx (t,i) -> I.add idx t i) (I.empty ()) seq in
    Sequence.for_all
      (fun (t,_) ->
        let retrieved = find_all retrieve idx 1 t 0 in
        Sequence.for_all
          (fun (t',i') ->
            try
              let _ = check t 0 t' 1 in
              List.exists
                (fun (_,i'') -> i' = i'')
                retrieved
            with Unif.Fail -> true)
          seq)
      seq

  let _match_flip ?subst t1 s_1 t2 s_2 =
    Unif.FO.matching ?subst ~pattern:t2 s_2 t1 s_1

  let size = Sequence.length
  let pp l =
    CCFormat.to_string
      (fun out l -> CCFormat.seq
        (fun out (t,i) -> Format.fprintf out "%a -> %d" T.pp t i) out l) l

  let _limit = 0

  let check_retrieved_unify =
    let prop = _check_all_retrieved_satisfy I.retrieve_unifiables Unif.FO.unification in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_unify" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

  let check_retrieved_specializations =
    let prop = _check_all_retrieved_satisfy (I.retrieve_specializations ~allow_open:false)
      (fun t1 s1 t2 s2 -> Unif.FO.matching ~pattern:t1 s1 t2 s2) in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_specializations" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

  let check_retrieved_generalizations =
    let prop = _check_all_retrieved_satisfy (I.retrieve_generalizations ~allow_open:false) _match_flip in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_generalizations" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

  let check_retrieve_all_unify =
    let prop = _check_all_satisfying_are_retrieved I.retrieve_unifiables Unif.FO.unification in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_unify" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

  let check_retrieve_all_specializations =
    let prop = _check_all_satisfying_are_retrieved (I.retrieve_specializations ~allow_open:false)
      (fun t1 s1 t2 s2 -> Unif.FO.matching ~pattern:t1 s1 t2 s2) in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_specializations" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

  let check_retrieve_all_generalizations =
    let prop = _check_all_satisfying_are_retrieved (I.retrieve_generalizations ~allow_open:false) _match_flip in
    let name = CCFormat.sprintf "index(%s)_retrieve_imply_generalizations" I.name in
    mk_test ~name ~limit:_limit ~size ~pp (gen 10 150) prop

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
  let module IntFastFingerprint = FastFingerprint.Make(OrderedInt) in
  let module TestFastFingerprint = TestTerm(IntFastFingerprint) in
  let module IntNPDtree = NPDtree.MakeTerm(OrderedInt) in
  let module TestIntNPDTree = TestTerm(IntNPDtree) in
  QCheck.flatten
    [ TestDtree.props
    ; TestNPDtree.props
    ; TestFingerprint.props
    ; TestFastFingerprint.props
    ; TestIntNPDTree.props
    ]
