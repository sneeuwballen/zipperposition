
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition
module Fmt = CCFormat

type t = {
  lits: Lit.t IArray.t;
}

let make lits: t = { lits }

let equal a b = IArray.equal Lit.equal a.lits b.lits
let hash a = IArray.hash Lit.hash a.lits
let pp out a =
  Fmt.fprintf out "[@[%a@]]" (Fmt.seq Lit.pp) (IArray.to_seq a.lits)
