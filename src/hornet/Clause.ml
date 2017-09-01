
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Logtk

module BV = CCBV
module Fmt = CCFormat
module PW = Position.With
module S = Subst
module T = Term

open Hornet_types

type t = Hornet_types.clause
type clause = t

type idx = Hornet_types.clause_idx

(** {2 Basics} *)

let lits c = c.c_lits
let proof c = c.c_proof
let constr c = c.c_constr
let trail c = c.c_trail
let depth c = c.c_depth

let dismatch_constr c = c.c_constr.constr_dismatch

let equal = Hornet_types_util.equal_clause
let hash = Hornet_types_util.hash_clause
let compare = Hornet_types_util.compare_clause
let pp = Hornet_types_util.pp_clause
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
    CCOrd.( int n_vars1 n_vars2 <?> (int, Lit.weight l1, Lit.weight l2) )
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
    lazy (
      Horn_clause.make head body proof
        ~constr ~trail ~unordered_depth:0 ~label:[])
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
  trail:bool_trail ->
  constr:c_constraint ->
  depth:int ->
  'a ->
  proof ->
  t

(* Smart constructor: might sort the literals for Horn clauses.
   The conclusion comes first, then the remaining ones with some heuristic
   ordering. *)
let make_ =
  let n_ = ref 0 in
  fun c_trail c_depth c_constr c_kind c_lits c_proof ->
    let c_id = CCRef.incr_then_get n_ in
    { c_id; c_depth; c_constr; c_trail;
      c_select=None; c_grounding=None; c_lits; c_kind; c_proof }

let make ~trail ~constr ~depth c_lits proof: t =
  let c_kind = kind_of_lits ~trail ~constr c_lits proof in
  make_ trail depth constr c_kind c_lits proof

let make_l ~trail ~constr ~depth lits proof : t =
  make ~trail ~constr ~depth (IArray.of_list lits) proof

let hash_mod_alpha c : int =
  IArray.hash_comm Lit.hash_mod_alpha c.c_lits

let select c = c.c_select

let select_exn c = match select c with
  | Some s -> s
  | None ->
    Util.errorf ~where:"clause.select_exn" "clause `%a`@ has no selected lit" pp c

let set_select c (s:select_lit): unit = match c.c_select with
  | Some _ -> Util.errorf ~where:"clause.set_select" "literal already selected"
  | None -> c.c_select <- Some s

let clear_select c = match c.c_select with
  | Some _ -> c.c_select <- None
  | None ->
    Util.errorf ~where:"clause.clear_select" "no literal currently selected in@ %a" pp c

let grounding c = c.c_grounding

let grounding_exn c = match c.c_grounding with
  | Some g -> g
  | None ->
    Util.errorf ~where:"clause.grounding_exn" "no grounding for@ %a" pp c

let set_grounding c g = match c.c_grounding with
  | None -> c.c_grounding <- Some g
  | Some _ ->
    Util.errorf ~where:"clause.set_grounding" "`%a`@ already grounded" pp c

let is_empty c = IArray.length c.c_lits = 0

let lits_seq c = IArray.to_seqi c.c_lits

let vars_seq c =
  IArray.to_seq c.c_lits
  |> Sequence.flat_map Lit.vars_seq

let vars_l c = vars_seq c |> T.VarSet.of_seq |> T.VarSet.to_list

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Horn of horn_clause
  | General

let classify (c:t): kind = match c.c_kind with
  | C_horn c -> Horn (Lazy.force c)
  | C_general -> General

let is_horn c = match c.c_kind with
  | C_horn _ -> true
  | C_general -> false

let is_ground c : bool =
  IArray.for_all Lit.is_ground (lits c)

let is_unit_ground c : bool =
  IArray.length c.c_lits = 1 && Lit.is_ground (IArray.get (lits c) 0)

(** {2 Utils} *)

let of_slit_l ~stmt lits : t =
  let conv_slit = function
    | SLiteral.True -> Lit.true_
    | SLiteral.False -> Lit.false_
    | SLiteral.Atom (t,b) -> Lit.atom ~sign:b t
    | SLiteral.Eq (s,t) -> Lit.eq s t
    | SLiteral.Neq (s,t) -> Lit.eq ~sign:false s t
  in
  let lits = List.map conv_slit lits in
  let proof = Proof.from_stmt stmt in
  make_l ~constr:Constraint.empty ~trail:H_trail.empty ~depth:0 lits proof

let is_trivial c =
  IArray.exists Lit.is_trivial c.c_lits ||
  begin
    IArray.to_seqi c.c_lits
    |> Sequence.exists
      (fun (i,lit) ->
         IArray.to_seqi c.c_lits
         |> Sequence.exists (fun (j,lit') -> i<j && Lit.equal lit (Lit.neg lit')))
  end ||
  Constraint.is_absurd c.c_constr

(* add constraint, re-compute kind (different horn clause) *)
let add_dismatch_constr (c:t) (d:Dismatching_constr.t): unit =
  if is_horn c then (
    Util.errorf ~where:"clause.add_dismatch_constr" "should not be horn:@ `%a`" pp c;
  );
  assert (not (is_horn c));
  c.c_constr <- Constraint.add_dismatch d c.c_constr;
  ()

(** {2 Unif} *)

let variant ?(subst=S.empty) (c1,sc1) (c2,sc2) : S.t Sequence.t =
  Lit.variant_arr ~subst (c1.c_lits,sc1)(c2.c_lits,sc2)

let equal_mod_alpha c1 c2 : bool =
  not (Sequence.is_empty (variant (c1,0) (c2,1)))

module Tbl_mod_alpha = CCHashtbl.Make(struct
    type t = clause
    let equal = equal_mod_alpha
    let hash = hash_mod_alpha
  end)
