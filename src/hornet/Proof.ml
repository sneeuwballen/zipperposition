
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 CProofs} *)

open Libzipperposition
open Hornet_types

module U = Hornet_types_util
module Stmt = Statement
module Fmt = CCFormat

type t = proof
type formula = TypedSTerm.t

let trivial = P_trivial
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

let name (p:t): string = match p with
  | P_trivial -> "trivial"
  | P_from_input _ -> "from_input"
  | P_from_file _ -> "from_file"
  | P_cnf_neg _ -> "cnf_neg"
  | P_cnf _ -> "cnf"
  | P_bool_tauto -> "bool_tauto"
  | P_avatar_split _ -> "avatar_split"
  | P_split _ -> "split"
  | P_instance (_,subst) -> Fmt.sprintf "instance(@[%a@])" Subst.pp subst
  | P_bool_res r -> Fmt.sprintf "bool_res(%a)" U.pp_bool_lit r.bool_res_atom
  | P_hc_superposition _ -> "hc_sup"
  | P_hc_simplify _ -> "hc_simpl"

let parents (p:t): proof_with_res list = match p with
  | P_trivial
  | P_bool_tauto
  | P_from_file _
  | P_from_input _
    -> []
  | P_cnf_neg r
  | P_cnf r -> [r]
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



module Src_tbl = CCHashtbl.Make(struct
    type t = Stmt.source
    let equal = Stmt.Src.equal
    let hash = Stmt.Src.hash
  end)

(* used to share the same clauses in the proof *)
let input_proof_tbl_ : t Src_tbl.t = Src_tbl.create 32

let rec proof_of_stmt src : t =
  try Src_tbl.find input_proof_tbl_ src
  with Not_found ->
    let p = match Stmt.Src.view src with
      | Stmt.Input (_, r) -> P_from_input r
      | Stmt.From_file (f, r) -> P_from_file (f,r)
      | Stmt.Internal _ -> trivial
      | Stmt.Neg srcd -> P_cnf_neg (proof_of_sourced srcd)
      | Stmt.CNF srcd -> P_cnf (proof_of_sourced srcd)
    in
    Src_tbl.add input_proof_tbl_ src p;
    p

and proof_of_sourced (x:Stmt.sourced_t) : proof_with_res =
  let module F = TypedSTerm.Form in
  let r, src = x in
  let p = proof_of_stmt src in
  let res = match r with
    | Stmt.Sourced_input f -> PR_formula f
    | Stmt.Sourced_clause _ -> assert false (* TODO: how? dep cycles… *)
  in
  p, res

(* conversion from statement *)
let from_stmt (st:(_,_,_)Stmt.t): t = proof_of_stmt (Stmt.src st)

