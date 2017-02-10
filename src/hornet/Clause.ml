
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
type bool_trail = Hornet_types.bool_trail
type horn_clause = Hornet_types.horn_clause

type constraint_ = Hornet_types.c_constraint_ =
  | C_dismatch of Dismatching_constr.t

(** {2 Basics} *)

let lits c = c.c_lits
let proof c = c.c_proof
let constr c = c.c_constr
let trail c = c.c_trail

let dismatch_constr c =
  constr c
  |> CCList.filter_map
    (function
      | C_dismatch d -> Some d)


let equal a b = a.c_id = b.c_id
let hash a = Hash.int a.c_id
let compare a b = CCInt.compare a.c_id b.c_id
let pp out a =
  Fmt.fprintf out "[@[%a%a@]]"
    (Fmt.seq Lit.pp) (IArray.to_seq a.c_lits)
    Hornet_types_util.pp_bool_trail_opt a.c_trail
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

let kind_of_lits ~trail ~constr (c_lits:Lit.t IArray.t) proof: c_kind =
  (* positive literals *)
  let pos =
    IArray.to_seqi c_lits
    |> Sequence.filter (fun (_,lit) -> Lit.sign lit)
    |> Sequence.to_rev_list
  in
  let mk_body arr =
    Array.sort compare_lits_for_horn_ arr; (* sort body in some order *)
    IArray.of_array_unsafe arr
  and mk_horn head body =
    Horn_clause.make ~constr ~trail ~unordered_depth:0 head body proof
  in
  begin match pos with
    | [] ->
      (* negative clause: actually a horn clause with head [false] *)
      let head = Lit.false_ in
      let body =
        Array.init (IArray.length c_lits) (fun i->Lit.neg (IArray.get c_lits i))
        |> mk_body
      in
      let hc = mk_horn head body in
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
      let hc = mk_horn head body in
      C_horn hc
    | _ -> C_general
  end

(** How to build a clause from a ['a] and other parameters *)
type 'a builder =
  ?trail:bool_trail ->
  ?constr:constraint_ list ->
  'a ->
  proof ->
  t

(* Smart constructor: might sort the literals for Horn clauses.
   The conclusion comes first, then the remaining ones with some heuristic
   ordering. *)
let make_ =
  let n_ = ref 0 in
  fun c_trail c_constr c_kind c_lits c_proof ->
    let c_id = CCRef.incr_then_get n_ in
    { c_id; c_constr; c_trail ; c_lits; c_kind; c_proof }

let make ?(trail=[]) ?(constr=[]) c_lits proof: t =
  let c_kind = kind_of_lits ~trail ~constr c_lits proof in
  make_ trail constr c_kind c_lits proof

let make_l ?trail ?constr lits proof : t =
  make ?trail ?constr (IArray.of_list lits) proof

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
    | SLiteral.Eq (s,t) -> Lit.eq s t
    | SLiteral.Neq (s,t) -> Lit.eq ~sign:false s t
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
  Unif.unif_array_com subst
    (IArray.to_array_unsafe a1,sc1)
    (IArray.to_array_unsafe a2,sc2)
    ~op:(fun subst x y -> Lit.variant ~subst x y)

let equal_mod_alpha c1 c2 : bool =
  not (Sequence.is_empty (variant (c1,0) (c2,1)))

module Tbl_mod_alpha = CCHashtbl.Make(struct
    type t = clause
    let equal = equal_mod_alpha
    let hash = hash_mod_alpha
  end)
