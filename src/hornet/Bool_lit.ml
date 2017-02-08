
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
open Hornet_types

module FI = Msat.Formula_intf
module Fmt = CCFormat
module C = Clause

module Int_map = Util.Int_map

type lit = Hornet_types.lit
type clause = Hornet_types.clause
type clause_idx = Hornet_types.clause_idx
type proof = Hornet_types.proof

type unique_id = Hornet_types.bool_unique_id

(** {2 Basics} *)

type atom = Hornet_types.bool_atom

type t = Hornet_types.bool_lit

let atom t = t.bl_atom
let sign t = t.bl_sign
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

type view =
  | Fresh of int
  | Box_clause of clause
  | Select_lit of clause * clause_idx
  | Ground_lit of lit (* must be ground *)

let view (t:t): view = match t.bl_atom with
  | A_fresh i -> Fresh i
  | A_box_clause (c,_) -> Box_clause c
  | A_select (c,i,_) -> Select_lit(c,i)
  | A_ground lit -> Ground_lit lit

(** {2 Constructors} *)

type state = {
  mutable count: int; (* for fresh counters *)
  select_tbl : atom Int_map.t C.Tbl_mod_alpha.t;
  (* structural map [(clause,idx) -> atom] *)
  box_tbl : atom C.Tbl_mod_alpha.t;
  (* map [clause -> atom] modulo alpha, for components *)
}

let create_state() : state = {
  count=1;
  select_tbl=C.Tbl_mod_alpha.create 64;
  box_tbl=C.Tbl_mod_alpha.create 64;
}

let make_ bl_sign bl_atom : t = {bl_atom; bl_sign}

(* stateless *)
let dummy = make_ true (A_fresh 0)

let of_atom ?(sign=true) a = make_ sign a

let fresh_atom_id_ state: int =
  let n = state.count in
  state.count <- n+1;
  n

let fresh state = make_ true (A_fresh (fresh_atom_id_ state))

let select_lit state c i =
  let atom =
    let map =
      try C.Tbl_mod_alpha.find state.select_tbl c
      with Not_found -> Int_map.empty
    in
    try Int_map.find i map
    with Not_found ->
      let a = A_select (c,i,fresh_atom_id_ state) in
      C.Tbl_mod_alpha.replace state.select_tbl c (Int_map.add i a map);
      a
  in
  make_ true atom

(* unit ground literal.
   invariant: move [lit]'s sign to the boolean literal sign *)
let ground (lit:Lit.t): t =
  assert (Lit.is_ground lit);
  if Lit.sign lit
  then make_ true (A_ground lit)
  else make_ false (A_ground (Lit.neg lit))

let box_clause state c =
  if C.is_unit_ground c
  then ground (IArray.get (C.lits c) 0)
  else (
    (* make a [Clause c] literal *)
    let a =
      try C.Tbl_mod_alpha.find state.box_tbl c
      with Not_found ->
        let a = A_box_clause (c, fresh_atom_id_ state) in
        C.Tbl_mod_alpha.add state.box_tbl c a;
        a
    in
    make_ true a
  )
