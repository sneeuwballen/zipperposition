
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Result} *)

(** The various results of a proof *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module HC = Horn_clause

type t = Hornet_types.proof_res

let equal (a:t)(b:t): bool = match a, b with
  | PR_horn_clause hc1, PR_horn_clause hc2 -> HC.equal hc1 hc2
  | PR_clause c1, PR_clause c2 -> Clause.equal c1 c2
  | PR_bool_clause c1, PR_bool_clause c2 -> Bool_lit.equal_clause c1 c2
  | PR_formula f1, PR_formula f2 -> TypedSTerm.equal f1 f2
  | PR_horn_clause _, _
  | PR_clause _, _
  | PR_bool_clause _, _
  | PR_formula _, _
    -> false

let hash (a:t): int = match a with
  | PR_horn_clause c -> Hash.combine2 10 (HC.hash c)
  | PR_clause c -> Hash.combine2 20 (Clause.hash c)
  | PR_bool_clause c -> Hash.combine2 30 (Bool_lit.hash_clause c)
  | PR_formula f -> Hash.combine2 40 (TypedSTerm.hash f)

let pp out (a:t): unit = match a with
  | PR_horn_clause c -> HC.pp out c
  | PR_clause c -> Clause.pp out c
  | PR_bool_clause c -> Bool_lit.pp_clause out c
  | PR_formula f -> TypedSTerm.pp out f

let to_string = Fmt.to_string pp

let is_absurd = function
  | PR_horn_clause hc -> HC.is_absurd hc && Trail.is_empty (HC.trail hc)
  | PR_clause c -> IArray.length (Clause.lits c) = 0
  | PR_bool_clause [] -> true
  | PR_bool_clause _ -> false
  | PR_formula f -> TypedSTerm.equal f TypedSTerm.Form.false_

module As_key = struct
  type t = proof_res
  let equal = equal
  let hash = hash
end
module Tbl = CCHashtbl.Make(As_key)
