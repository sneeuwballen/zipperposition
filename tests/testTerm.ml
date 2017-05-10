
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test terms *)

open Logtk
open Logtk_arbitrary

module T = Term
module H = Helpers
module S = Substs

let (==>) = QCheck.(==>)

(* unit tests *)

let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"

let ty = Type.term

(** Properties *)

(* subterm is smaller than term *)
let check_size_subterm =
  (* choose a subterm of t *)
  let gen = QCheck.Gen.(
    ArTerm.default_g >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, T.Pos.at t pos))
  in
  let pp = QCheck.Print.(pair T.to_string T.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t1, t2) =
    T.subterm ~sub:t2 t1 &&
    T.size t1 >= T.size t2
  in
  QCheck.Test.make ~name:"term_size_subterm" gen prop

(* replace subterm by itself yields same term *)
let check_replace_id =
  let gen = QCheck.Gen.(
    ArTerm.default_g >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, pos))
  in
  let pp = QCheck.Print.(pair T.to_string Position.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t, pos) =
    let sub = T.Pos.at t pos in
    InnerTerm.DB.closed (sub : T.t :> InnerTerm.t)
    ==>
      let t' = T.Pos.replace t pos ~by:sub in
      T.equal t t'
  in
  QCheck.Test.make ~name:"term_replace_same_subterm" gen prop

let check_ground_novar =
  let gen = ArTerm.default in
  let prop t =
    not (T.is_ground t) || Sequence.is_empty (T.Seq.vars t)  (* ground => no vars *)
  in
  QCheck.Test.make ~count:1000 ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = ArTerm.default in
  let prop t =
    let vars = T.vars t in
    T.VarSet.is_empty vars || (T.min_var vars <= T.max_var vars)
  in
  QCheck.Test.make ~count:1000 ~name:"term_min_max_var" gen prop

let check_hash_mod_alpha =
  let gen = QCheck.pair ArTerm.default ArTerm.default in
  let prop (t1,t2) =
    if not (T.equal t1 t2) && Unif.FO.are_variant t1 t2
    then
      T.hash_mod_alpha t1 = T.hash_mod_alpha t2
    else QCheck.assume_fail ()
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:20
    ~name:"variant_have_same_hash_mod_alpha"

(* TODO: write a term arbitrary instance for DB terms (lambda terms?)
   and check that a shifted/unshifted closed term remains closed *)

let props =
  [ check_size_subterm
  ; check_replace_id
  ; check_ground_novar
  ; check_hash_mod_alpha
  ; check_min_max_vars
  ]
