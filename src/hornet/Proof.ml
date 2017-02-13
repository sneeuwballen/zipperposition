
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 CProofs} *)

open Hornet_types

module Fmt = CCFormat

type t = proof

let trivial = P_trivial
let from_stmt st = P_from_stmt st
let instance c subst = P_instance (c,subst)
let avatar_split c = P_avatar_split c
let split c = P_split c

let bool_tauto = P_bool_tauto
let bool_res a c1 p1 c2 p2 =
  let s = {
    bool_res_atom=a;
    bool_res_c1=c1;
    bool_res_p1=p1;
    bool_res_c2=c2;
    bool_res_p2=p2;
  } in
  P_bool_res s

let hc_sup x = P_hc_superposition x
let hc_simplify c = P_hc_simplify c

let pp = Hornet_types_util.pp_proof
let to_string = Fmt.to_string pp

let parents (p:t): proof_with_res list = match p with
  | P_trivial
  | P_from_stmt _
  | P_bool_tauto
    -> []
  | P_avatar_split c
  | P_split c
  | P_instance (c,_) -> [c.c_proof, PR_clause c]
  | P_bool_res r ->
    [ r.bool_res_p1, PR_bool_clause r.bool_res_c1;
      r.bool_res_p2, PR_bool_clause r.bool_res_c2;
    ]
  | P_hc_superposition r ->
    let c1, _ = r.hc_sup_active in
    let c2, _ = r.hc_sup_passive in
    [ c1.hc_proof, PR_horn_clause c1;
      c2.hc_proof, PR_horn_clause c2;
    ]
  | P_hc_simplify c ->
    [ c.hc_proof, PR_horn_clause c]
