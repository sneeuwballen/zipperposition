
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
open Hornet_types

module FI = Msat.Formula_intf
module Fmt = CCFormat
module C = Clause

module Int_map = Util.Int_map

(** {2 Basics} *)

type atom = Hornet_types.bool_atom
type proof = Hornet_types.proof
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
let to_string = Fmt.to_string pp
let print = pp

type view =
  | Fresh of int
  | Box_clause of clause * bool_box_clause
  | Select_lit of clause * clause_idx * bool_select
  | Ground_lit of lit * bool_ground (* must be ground and positive *)

let view (t:t): view = match t.bl_atom.a_view with
  | A_fresh i -> Fresh i
  | A_box_clause r -> Box_clause (r.bool_box_clause,r)
  | A_select r -> Select_lit (r.bool_select_clause, r.bool_select_idx, r)
  | A_ground r -> Ground_lit (r.bool_ground_lit, r)

(** {2 Constructors} *)

type state = {
  mutable count: int; (* for fresh counters *)
  select_tbl : atom Int_map.t C.Tbl_mod_alpha.t;
  (* structural map [(clause,idx) -> atom] *)
  box_tbl : atom C.Tbl_mod_alpha.t;
  (* map [clause -> atom] modulo alpha, for components *)
  ground_tbl: atom Lit.Tbl.t;
  (* map [ground positive lit -> atom] *)
}

let create_state() : state = {
  count=1;
  select_tbl=C.Tbl_mod_alpha.create 64;
  box_tbl=C.Tbl_mod_alpha.create 64;
  ground_tbl=Lit.Tbl.create 64;
}

let make_ bl_sign bl_atom : t = {bl_atom; bl_sign}

(* stateless *)
let dummy = make_ true {a_view=A_fresh 0; a_dependent=[]}

let of_atom ?(sign=true) a = make_ sign a

let fresh_atom_id_ state: int =
  let n = state.count in
  state.count <- n+1;
  n

let atom_of_view a = {a_view=a; a_dependent=[]}

let fresh state =
  make_ true (atom_of_view (A_fresh (fresh_atom_id_ state)))

let select_lit state c i =
  let atom =
    let map =
      try C.Tbl_mod_alpha.find state.select_tbl c
      with Not_found -> Int_map.empty
    in
    try Int_map.find i map
    with Not_found ->
      let a =
        A_select {
          bool_select_idx=i;
          bool_select_lit=IArray.get c.c_lits i;
          bool_select_clause=c;
          bool_select_id=fresh_atom_id_ state;
        } |> atom_of_view
      in
      C.Tbl_mod_alpha.replace state.select_tbl c (Int_map.add i a map);
      a
  in
  make_ true atom

(* unit ground literal.
   invariant: move [lit]'s sign to the boolean literal sign *)
let ground state (lit:Lit.t): t =
  assert (Lit.is_ground lit);
  let lit, sign = if Lit.sign lit then lit, true else Lit.neg lit, false in
  let atom =
    try Lit.Tbl.find state.ground_tbl lit
    with Not_found ->
      let atom =
        A_ground {
          bool_ground_lit=lit;
          bool_ground_id=fresh_atom_id_ state;
          bool_ground_instance_of=[];
        } |> atom_of_view
      in
      Lit.Tbl.add state.ground_tbl lit atom;
      atom
  in
  make_ sign atom

let box_clause state c =
  if C.is_unit_ground c
  then ground state (IArray.get (C.lits c) 0)
  else (
    (* make a [Clause c] literal *)
    let a =
      try C.Tbl_mod_alpha.find state.box_tbl c
      with Not_found ->
        let a = A_box_clause {
            bool_box_clause=c;
            bool_box_id=fresh_atom_id_ state;
          } |> atom_of_view
        in
        C.Tbl_mod_alpha.add state.box_tbl c a;
        a
    in
    make_ true a
  )

(** {2 Boolean Clauses} *)

type bool_clause = t list

let pp_clause = Hornet_types_util.pp_bool_clause

(** {2 Boolean Trails} *)

type bool_trail = Hornet_types.bool_trail

let pp_trail = Hornet_types_util.pp_bool_trail

(** {2 Containers} *)

module As_key = struct
  type t = bool_lit
  let equal = equal
  let hash = hash
end
module Tbl = CCHashtbl.Make(As_key)
