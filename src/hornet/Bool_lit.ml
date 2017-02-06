
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module FI = Msat.Formula_intf
module Fmt = CCFormat
module C = Clause
module CG = Clause.General

module Int_map = Util.Int_map

module type S = Bool_lit_intf.S

module type PROOF = sig
  type t
end

module Make(Proof:PROOF)(X : sig end) : S with type proof = Proof.t = struct
  type proof = Proof.t

  type unique_id = int
  type atom =
    | A_fresh of unique_id
    | A_box_clause of Clause.t * unique_id
    | A_select of CG.t * CG.idx * unique_id
    | A_ground of Lit.t
    | A_depth_limit of int

  type t = {
    atom: atom;
    sign: bool;
  }

  let make_ sign atom : t = {atom;sign}

  let sign t = t.sign

  let fresh_atom_id_ : unit -> int =
    let n = ref 0 in fun () -> incr n; !n

  let fresh () = make_ true (A_fresh (fresh_atom_id_()))

  let dummy = fresh()

  let select_lit =
    (* structural map [(clause,idx) -> atom] *)
    let tbl : atom Int_map.t C.Tbl_mod_alpha.t =
      C.Tbl_mod_alpha.create 64
    in
    fun c i ->
      let cg = (c : CG.t :> C.t) in
      let ig = (i : CG.idx :> int) in
      let atom =
        let map = try C.Tbl_mod_alpha.find tbl cg with Not_found -> Int_map.empty in
        try Int_map.find ig map
        with Not_found ->
          let a = A_select (c,i,fresh_atom_id_ ()) in
          C.Tbl_mod_alpha.replace tbl cg (Int_map.add ig a map);
          a
      in
      make_ true atom

  let ground (l:Lit.t): t =
    assert (Lit.is_ground l);
    make_ true (A_ground l)

  let box_clause =
    (* structural map [clause -> atom] *)
    let tbl = C.Tbl_mod_alpha.create 64 in
    fun c ->
      if C.is_unit_ground c then ground (IArray.get (C.lits c) 0)
      else (
        (* make a [Clause c] literal *)
        let a =
          try C.Tbl_mod_alpha.find tbl c
          with Not_found ->
            let a = A_box_clause (c, fresh_atom_id_()) in
            C.Tbl_mod_alpha.add tbl c a;
            a
        in
        make_ true a
      )

  let depth_limit i = make_ true (A_depth_limit i)

  let neg t = {t with sign=not t.sign}

  let norm (t:t): t * FI.negated =
    if t.sign
    then t, FI.Same_sign
    else neg t, FI.Negated

  let equal a b : bool =
    a.sign = b.sign
    &&
    begin match a.atom, b.atom with
      | A_fresh i, A_fresh j ->  i=j
      | A_depth_limit i, A_depth_limit j -> i=j
      | A_box_clause (_,i1), A_box_clause (_,i2) -> i1=i2
      | A_ground l1, A_ground l2 -> Lit.equal l1 l2
      | A_select (_,_,id1), A_select (_,_,id2) -> id1=id2
      | A_fresh _, _
      | A_box_clause _, _
      | A_ground _, _
      | A_select _, _
      | A_depth_limit _, _
        -> false
    end

  let hash a : int = match a.atom with
    | A_fresh i -> Hash.combine3 10 (Hash.bool a.sign) (Hash.int i)
    | A_box_clause (_,i) -> Hash.combine2 15 (Hash.int i)
    | A_select (_,_,i) ->
      Hash.combine3 20 (Hash.bool a.sign) (Hash.int i)
    | A_depth_limit i ->
      Hash.combine2 30 (Hash.int i)
    | A_ground lit -> Hash.combine2 50 (Lit.hash lit)

  let print out l =
    let pp_atom out = function
      | A_fresh i -> Fmt.fprintf out "fresh_%d" i
      | A_box_clause (c,i) -> Fmt.fprintf out "%a/%d" Clause.pp c i
      | A_select (c,i,id) ->
        Fmt.fprintf out "@[select@ :idx %d@ :id %d :clause %a@]" (i:>int) id CG.pp c
      | A_ground lit -> Lit.pp out lit
      | A_depth_limit i ->
        Fmt.fprintf out "[depth@<1>≤%d]" i
    in
    if l.sign
    then Fmt.within "(" ")" pp_atom out l.atom
    else Fmt.fprintf out "(¬%a)" pp_atom l.atom

  let pp_clause out l =
    Fmt.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ⊔ " print) l

  type view =
    | Fresh of int
    | Box_clause of Clause.t
    | Select_lit of Clause.General.t * Clause.General.idx
    | Ground_lit of Lit.t (* must be ground *)
    | Depth_limit of int

  let view (t:t): view = match t.atom with
    | A_fresh i -> Fresh i
    | A_box_clause (c,_) -> Box_clause c
    | A_select (c,i,_) -> Select_lit(c,i)
    | A_ground lit -> Ground_lit lit
    | A_depth_limit d -> Depth_limit d
end
