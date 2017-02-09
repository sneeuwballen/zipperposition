
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition

module BV = CCBV
module Fmt = CCFormat
module PW = Position.With
module S = Subst

open Hornet_types

type t = Hornet_types.clause
type clause = t

type proof = Hornet_types.proof
type idx = Hornet_types.clause_idx
type bool_lit = Hornet_types.bool_lit
type horn_clause = Hornet_types.horn_clause

type constraint_ = Hornet_types.c_constraint_ =
  | C_dismatch of Dismatching_constr.t

(** {2 Basics} *)

let lits c = c.c_lits
let proof c = c.c_proof
let constr c = c.c_constr
let bool_lit c = c.c_bool_lit

let dismatch_constr c =
  constr c
  |> CCList.filter_map
    (function
      | C_dismatch d -> Some d)


let equal a b = IArray.equal Lit.equal a.c_lits b.c_lits
let hash a = IArray.hash Lit.hash a.c_lits
let compare a b = IArray.compare Lit.compare a.c_lits b.c_lits
let pp out a =
  Fmt.fprintf out "[@[%a@]]" (Fmt.seq Lit.pp) (IArray.to_seq a.c_lits)
let to_string = Fmt.to_string pp

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

let kind_of_lits (c_lits:Lit.t IArray.t) proof: c_kind =
  (* positive literals *)
  let pos =
    IArray.to_seqi c_lits
    |> Sequence.filter (fun (_,lit) -> Lit.sign lit)
    |> Sequence.to_rev_list
  in
  let mk_body arr =
    Array.sort compare_lits_for_horn_ arr; (* sort body in some order *)
    IArray.of_array_unsafe arr
  in
  begin match pos with
    | [] ->
      (* negative clause: actually a horn clause with head [false] *)
      let head = Lit.false_ in
      let body =
        Array.init (IArray.length c_lits) (fun i->Lit.neg (IArray.get c_lits i))
        |> mk_body
      in
      let hc = Horn_clause.make head body proof in
      C_horn hc
    | [i,_] ->
      let head = IArray.get c_lits i in
      let body =
        Array.init (IArray.length c_lits-1)
          (fun j ->
             let lit =
               if j<i then IArray.get c_lits j else IArray.get c_lits (j+1)
             in
             Lit.neg lit)
        |> mk_body
      in
      let hc = Horn_clause.make head body proof in
      C_horn hc
    | _ -> C_general
  end

(** How to build a clause from a ['a] and other parameters *)
type 'a builder =
  ?b_lit:bool_lit lazy_t ->
  ?constrs:constraint_ list ->
  'a ->
  proof ->
  t

(* Smart constructor: might sort the literals for Horn clauses.
   The conclusion comes first, then the remaining ones with some heuristic
   ordering. *)
let make_ c_bool_lit c_constr c_kind c_lits c_proof =
  { c_constr; c_bool_lit; c_lits; c_kind; c_proof }

let make ?b_lit ?(constrs=[]) c_lits proof: t =
  let c_kind = kind_of_lits c_lits proof in
  make_ b_lit constrs c_kind c_lits proof

let make_l ?b_lit ?constrs lits proof : t =
  make ?b_lit ?constrs (IArray.of_list lits) proof

let hash_mod_alpha c : int =
  IArray.hash_comm Lit.hash_mod_alpha c.c_lits

let is_empty c = IArray.length c.c_lits = 0

let lits_seq c = IArray.to_seqi c.c_lits

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Horn of horn_clause
  | General

let classify (c:t): kind = match c.c_kind with
  | C_horn c -> Horn c
  | C_general -> General

let is_ground c : bool =
  IArray.for_all Lit.is_ground (lits c)

let is_unit_ground c : bool =
  IArray.length c.c_lits = 1 && Lit.is_ground (IArray.get (lits c) 0)

(** {2 Utils} *)

let of_slit_l ~stmt lits =
  let conv_slit = function
    | SLiteral.True -> Lit.true_
    | SLiteral.False -> Lit.false_
    | SLiteral.Atom (t,b) -> Lit.atom ~sign:b t
    | SLiteral.Eq (_,_) -> failwith "TODO: equations"
    | SLiteral.Neq (_,_) -> failwith "TODO: disequations"
  in
  let lits = List.map conv_slit lits in
  let proof = Proof.from_stmt stmt in
  make_l lits proof

let constr_trivial_ (c:constraint_): bool = match c with
  | C_dismatch c -> Dismatching_constr.is_trivial c

let is_trivial c =
  IArray.exists Lit.is_trivial c.c_lits ||
  begin
    IArray.to_seqi c.c_lits
    |> Sequence.exists
      (fun (i,lit) ->
         IArray.to_seqi c.c_lits
         |> Sequence.exists (fun (j,lit') -> i<j && Lit.equal lit (Lit.neg lit')))
  end ||
  List.exists constr_trivial_ c.c_constr

(** {2 Unif} *)

let variant ?(subst=S.empty) (c1,sc1) (c2,sc2) : S.t Sequence.t =
  let a1 = c1.c_lits in
  let a2 = c2.c_lits in
  (* match a1.(i...) with a2\bv *)
  let rec iter2 subst bv i k =
    if i = IArray.length a1
    then k subst
    else iter3 subst bv i 0 k
  (* find a matching literal for a1.(i), within a2.(j...) *)
  and iter3 subst bv i j k =
    if j = IArray.length a2
    then ()  (* stop *)
    else (
      if not (BV.get bv j)
      then (
        (* try to match i-th literal of a1 with j-th literal of a2 *)
        BV.set bv j;
        Lit.variant ~subst (IArray.get a1 i,sc1) (IArray.get a2 i,sc2)
          (fun subst -> iter2 subst bv (i+1) k);
        BV.reset bv j
      );
      iter3 subst bv i (j+1) k
    )
  in
  fun yield ->
    if IArray.length a1 = IArray.length a2
    then (
      let bv = BV.create ~size:(IArray.length a1) false in
      iter2 subst bv 0 yield
    )

let equal_mod_alpha c1 c2 : bool =
  not (Sequence.is_empty (variant (c1,0) (c2,1)))

module Tbl_mod_alpha = CCHashtbl.Make(struct
    type t = clause
    let equal = equal_mod_alpha
    let hash = hash_mod_alpha
  end)
