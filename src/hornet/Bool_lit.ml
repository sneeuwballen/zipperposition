
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
type view = Hornet_types.bool_atom

let atom t = t.bl_atom
let sign t = t.bl_sign
let view t = t.bl_atom
let neg = Hornet_types_util.neg_bool_lit

let norm (t:t): t * FI.negated =
  if t.bl_sign
  then t, FI.Same_sign
  else neg t, FI.Negated

let equal = Hornet_types_util.equal_bool_lit
let hash = Hornet_types_util.hash_bool_lit
let compare = Hornet_types_util.compare_bool_lit
let pp = Hornet_types_util.pp_bool_lit
let to_string = Fmt.to_string pp
let print = pp

(** {2 Constructors} *)

type state = {
  mutable count: int; (* for fresh counters *)
  box_tbl : atom C.Tbl_mod_alpha.t;
  (* map [clause -> atom] modulo alpha, for components *)
  ground_tbl: atom Lit.Tbl.t;
  (* map [ground positive lit -> atom] *)
}

let create_state() : state = {
  count=1;
  box_tbl=C.Tbl_mod_alpha.create 64;
  ground_tbl=Lit.Tbl.create 64;
}

let make_ bl_sign bl_atom : t = {bl_atom; bl_sign}

(* stateless *)
let dummy = make_ true (A_fresh 0)

let of_atom ?(sign=true) a = make_ sign a

let fresh_atom_id_ state: int =
  let n = state.count in
  state.count <- n+1;
  n

let fresh state =
  make_ true (A_fresh (fresh_atom_id_ state))

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
        }
      in
      Lit.Tbl.add state.ground_tbl lit atom;
      atom
  in
  make_ sign atom

let box_clause state c =
  (* make a [Clause c] literal *)
  let a =
    try C.Tbl_mod_alpha.find state.box_tbl c
    with Not_found ->
      let a = A_box_clause {
          bool_box_clause=c;
          bool_box_id=fresh_atom_id_ state;
          bool_box_depends=[];
        }
      in
      C.Tbl_mod_alpha.add state.box_tbl c a;
      a
  in
  make_ true a

(** {2 Boolean Clauses} *)

type bool_clause = t list

let equal_clause = CCList.equal equal
let hash_clause = Hash.list hash
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
