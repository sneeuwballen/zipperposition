
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 CProofs} *)

open Hornet_types

module Fmt = CCFormat

type t = proof

let from_stmt st = P_from_stmt st
let instance c subst = P_instance (c,subst)
let avatar_split c = P_avatar_split c
let split c = P_split c

let bool_tauto = P_bool_tauto
let bool_res c1 p1 c2 p2 =
  let s = {
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

(** {2 Print Proof DAG} *)

(* TODO: traverse recursively, with a table to preserve DAG *)
let pp_dag _ _ = assert false

(* TODO: DOT output *)

(* TODO: a type ['a with_proof] to always store have the conclusion
   of a proof with it *)
