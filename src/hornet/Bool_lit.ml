
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module FI = Msat.Formula_intf
module Fmt = CCFormat
module C = Clause

module Int_map = Util.Int_map

module type S = Bool_lit_intf.S

type lit = Hornet_types.lit
type clause = Hornet_types.clause
type clause_idx = Hornet_types.clause_idx
type proof = Hornet_types.proof

type view = Bool_lit_intf.view =
  | Fresh of int
  | Box_clause of clause
  | Select_lit of clause * clause_idx
  | Ground_lit of lit (* must be ground *)

type unique_id = Hornet_types.bool_unique_id
type atom = Hornet_types.bool_atom =
  | A_fresh of unique_id
  | A_box_clause of clause * unique_id
  | A_select of clause * clause_idx * unique_id
  | A_ground of lit

module Make(X : sig end) : S = struct
  type proof = Hornet_types.proof

  type t = Hornet_types.bool_lit = {
    bl_atom: atom;
    bl_sign: bool;
  }

  let make_ bl_sign bl_atom : t = {bl_atom; bl_sign}

  let sign t = t.bl_sign

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
      let cg = (c : clause :> C.t) in
      let ig = (i : clause_idx :> int) in
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

  let neg t = {t with bl_sign=not t.bl_sign}

  let norm (t:t): t * FI.negated =
    if t.bl_sign
    then t, FI.Same_sign
    else neg t, FI.Negated

  let equal = Hornet_types_util.equal_bool_lit
  let hash = Hornet_types_util.hash_bool_lit
  let pp = Hornet_types_util.pp_bool_lit
  let print = pp

  let pp_clause out l =
    Fmt.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ⊔ " print) l

  let view (t:t): view = match t.bl_atom with
    | A_fresh i -> Fresh i
    | A_box_clause (c,_) -> Box_clause c
    | A_select (c,i,_) -> Select_lit(c,i)
    | A_ground lit -> Ground_lit lit
end
