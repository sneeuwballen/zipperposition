
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

(* comparison function that makes the positif literal smaller, then
   favors other literals by heuristic *)
let compare_lits_for_horn_ (l1:Lit.t) (l2:Lit.t) : int =
  let sign1 = Lit.sign l1 in
  let sign2 = Lit.sign l2 in
  (* make positive lit smaller *)
  if sign1<>sign2
  then if sign1 then (assert (not sign2); -1)
  else (assert sign2; 1)
  else (
    let n_vars1 = Lit.vars_set l1 |> List.length in
    let n_vars2 = Lit.vars_set l2 |> List.length in
    CCOrd.( int_ n_vars1 n_vars2 <?> (int_, Lit.weight l1, Lit.weight l2) )
  )

(* Smart constructor: might sort the literals for Horn clauses.
   The conclusion comes first, then the remaining ones with some heuristic
   ordering. *)
let make_ c_kind c_lits =
  let c_kind, c_lits = match c_kind with
    | C_unit
    | C_general -> c_kind, c_lits
    | C_horn idx ->
      assert (Lit.sign (IArray.get c_lits idx));
      let arr = IArray.to_array c_lits in
      Array.sort compare_lits_for_horn_ arr;
      let c_lits = IArray.of_array_unsafe arr in
      assert (Lit.sign (IArray.get c_lits 0)); (* first *)
      C_horn 0, c_lits
  in
  { c_lits; c_kind }

let make c_lits: t =
  let c_kind = kind_of_lits c_lits in
  make_ c_kind c_lits

let equal a b = IArray.equal Lit.equal a.c_lits b.c_lits
let hash a = IArray.hash Lit.hash a.c_lits
let compare a b = IArray.compare Lit.compare a.c_lits b.c_lits
let pp out a =
  Fmt.fprintf out "[@[%a@]]" (Fmt.seq Lit.pp) (IArray.to_seq a.c_lits)
let to_string = Fmt.to_string pp

(** {2 Horn Clauses} *)

module Horn = struct
  type t = clause
  let as_clause c = c

  let idx_ c = match c.c_kind with
    | C_horn i -> assert (i=0); i
    | _ -> assert false

  let concl c = IArray.get c.c_lits (idx_ c)

  let body_seq c =
    let i_pos = idx_ c in
    IArray.to_seqi c.c_lits
    |> Sequence.filter_map
      (fun (i,lit) -> if i = i_pos then None else Some lit)

  let body_l c = body_seq c |> Sequence.to_rev_list

  let body_len c = IArray.length c.c_lits - 1

  let body_get c n =
    assert (idx_ c = 0);
    if n < 0 || n > IArray.length c.c_lits - 2 then invalid_arg "Horn.body_get";
    IArray.get c.c_lits (n-1)

  let pp = pp
end

(** {2 Unit Clauses} *)
module Unit = struct
  type t = clause

  let get c =
    assert (IArray.length c.c_lits = 1);
    IArray.get c.c_lits 0

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

(** {2 With Position} *)

module With_pos = struct
  type t = {
    clause: clause;
    pos: Position.t;
  }
  let clause t = t.clause
  let pos t = t.pos

  let make c p = {clause=c; pos=p}

  let equal t1 t2 = equal t1.clause t2.clause && Position.equal t1.pos t2.pos
  let compare t1 t2 =
    let c = compare t1.clause t2.clause in
    if c=0 then Position.compare t1.pos t2.pos else c
  let hash t = Hash.combine3 41 (hash t.clause) (Position.hash t.pos)
  let pp out t =
    Fmt.fprintf out "(@[:pos %a :in %a@])" Position.pp t.pos pp t.clause
  let to_string = Fmt.to_string pp
end

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Unit_atom of Unit.t
  | Horn of Horn.t
  | General of General.t
(* | Unit_eq of Lit.  *) (* TODO *)


let classify (c:t): kind = match c.c_kind with
  | C_unit -> Unit_atom c
  | C_horn _ -> Horn c
  | C_general -> General c


