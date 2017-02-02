
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition
module Fmt = CCFormat

type t = {
  c_lits: Lit.t IArray.t;
  c_kind: c_kind;
}

(* internal kind *)
and c_kind =
  | C_horn of int (* the index of the head *)
  | C_unit
  | C_general

type clause = t

let kind_of_lits (c_lits:Lit.t IArray.t): c_kind =
  if IArray.length c_lits=1
  then C_unit
  else (
    (* positive literals *)
    let pos =
      IArray.to_seqi c_lits
      |> Sequence.filter (fun (_,lit) -> Lit.sign lit)
      |> Sequence.to_rev_list
    in
    begin match pos with
      | [i,_] -> C_horn i (* exactly one pos *)
      | _ -> C_general
    end
  )

let make c_lits: t =
  let c_kind = kind_of_lits c_lits in
  { c_lits; c_kind }

let equal a b = IArray.equal Lit.equal a.c_lits b.c_lits
let hash a = IArray.hash Lit.hash a.c_lits
let pp out a =
  Fmt.fprintf out "[@[%a@]]" (Fmt.seq Lit.pp) (IArray.to_seq a.c_lits)
let to_string = Fmt.to_string pp

(** {2 Horn Clauses} *)

module Horn = struct
  type t = clause
  let as_clause c = c

  let idx_ c = match c.c_kind with
    | C_horn i -> i
    | _ -> assert false

  let concl c = IArray.get c.c_lits (idx_ c)

  let body_seq c =
    let i_pos = idx_ c in
    IArray.to_seqi c.c_lits
    |> Sequence.filter_map
      (fun (i,lit) -> if i = i_pos then None else Some lit)

  let body_l c = body_seq c |> Sequence.to_rev_list

  let pp = pp
end

(** {2 General Clause} *)

(** Such clauses are not Horn nor unit. They have at least two positive
    literals or 0 positive literals. *)

module General = struct
  type t = clause

  type idx = int
  (** The index of a literal in the clause *)

  let as_clause c = c

  let lits_seq c =
    assert (c.c_kind = C_general);
    IArray.to_seqi c.c_lits

  let pp = pp
end

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Unit_atom of Lit.t
  | Horn of Horn.t
  | General of General.t
(* | Unit_eq of Lit.  *) (* TODO *)


let classify (c:t): kind = match c.c_kind with
  | C_unit -> Unit_atom (IArray.get c.c_lits 0)
  | C_horn _ -> Horn c
  | C_general -> General c


