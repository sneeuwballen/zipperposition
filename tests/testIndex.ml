
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
  let compare (t1,i1) (t2,i2) = if t1 == t2 then i1-i2 else T.compare t1 t2
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
    let set = T.Tbl.from_list l in
    let seq = T.Tbl.to_seq set in
    let seq = Sequence.mapi (fun i t -> t, i) seq in
    return (Sequence.persistent seq))

  let pp seq =
    let pp buf (t,i) = Printf.bprintf buf "%a -> %d" T.pp t i in
    Util.on_buffer (Util.pp_seq pp) seq

  (* check that the size of index is correct *)
  let check_size_add =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.length seq = I.size idx
    in
    let name = Util.sprintf "index(%s)_size_after_add" I.name in
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
    let name = Util.sprintf "index(%s)_gen_retrieved_member" I.name in
    mk_test ~name (gen 30 100) prop

  (* check that the retrieved terms match the query *)
  let check_gen_retrieved_match =
    let prop seq =
      let idx = I.add_seq (I.empty ()) seq in
      Sequence.for_all
        (fun (t,i) ->
          let retrieved = find_all idx t in
          (* all terms must match [t] *)
          List.for_all
            (fun (t',_) ->
              try ignore (FOUnif.matching t' 0 t 1); true
              with FOUnif.Fail -> false)
            retrieved)
      seq
    in
    let name = Util.sprintf "index(%s)_gen_retrieved_match" I.name in
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
                let _ = FOUnif.matching t' 1 t 0 in
                List.exists
                  (fun (t'',_) -> T.eq t' t'')
                  retrieved
              with FOUnif.Fail -> true)
            seq)
        seq
    in
    let name = Util.sprintf "index(%s)_all_retrieved" I.name in
    mk_test ~name (gen 50 150) prop

  (* check the matching of generalization *)
  let props =
    [ check_size_add
    ; check_gen_retrieved_member
    ; check_gen_retrieved_match
    ; check_all_retrieved
    ]
end

let props =
  let module DT = struct include Dtree.Make(E) let name = "dtree" end in
  let module TestDtree = TestUnit(DT) in
  let module NPDT = struct include NPDtree.Make(E) let name = "npdtree" end in
  let module TestNPDtree = TestUnit(NPDT) in
  QCheck.flatten
    [ TestDtree.props
    ; TestNPDtree.props
    ]
