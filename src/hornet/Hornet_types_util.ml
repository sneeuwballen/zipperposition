
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basics for Hornet Types} *)

open Libzipperposition
open Hornet_types

module T = FOTerm
module Fmt = CCFormat

let pp_lit out (t:lit): unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t
  | Eq (t,u,true) -> Fmt.fprintf out "@[%a@ = %a@]" T.pp t T.pp u
  | Eq (t,u,false) -> Fmt.fprintf out "@[%a@ @<1>≠ %a@]" T.pp t T.pp u

let pp_clause_lits out a =
  Fmt.fprintf out "@[%a@]" (Fmt.seq pp_lit) (IArray.to_seq a.c_lits)
let pp_clause out (a:clause) = Fmt.within "[" "]" pp_clause_lits out a

let pp_atom out = function
  | A_fresh i -> Fmt.fprintf out "fresh_%d" i
  | A_box_clause b -> Fmt.fprintf out "%a" pp_clause_lits b.bool_box_clause
  | A_select r ->
    Fmt.fprintf out
      "@[select@ :idx %d@ :id %d :clause %a@]"
      r.bool_select_idx r.bool_select_id pp_clause r.bool_select_clause
  | A_ground r -> pp_lit out r.bool_ground_lit

let pp_bool_lit out l =
  let pp_inner out l = Fmt.within "⟦" "⟧" pp_atom out l.bl_atom in
  if l.bl_sign
  then pp_inner out l
  else Fmt.fprintf out "¬%a" pp_inner l

let pp_bool_clause out l =
  Fmt.fprintf out "[@[%a@]]" (Util.pp_list ~sep:" ⊔ " pp_bool_lit) l

let pp_bool_trail out (l:bool_trail) =
  let ppx out (lazy lit) = pp_bool_lit out lit in
  Fmt.fprintf out "[@[%a@]]" (Util.pp_list ~sep:" ⊓ " ppx) l

let pp_bool_trail_opt out trail = match trail with
  | [] -> ()
  | _ ->
    Fmt.fprintf out " @[<hv2>@<1>⇐@ %a@]" pp_bool_trail trail

let pp_constraint out (c:c_constraint_): unit = match c with
  | C_dismatch d -> Dismatching_constr.pp out d

let pp_hc_status out (s:horn_clause_status): unit = match s with
  | HC_new -> Fmt.string out "new"
  | HC_alive -> Fmt.string out "alive"
  | HC_dead -> Fmt.string out "dead"

let vars_of_lit (l:lit): _ HVar.t Sequence.t = match l with
  | Bool _ -> Sequence.empty
  | Atom (t,_) -> T.Seq.vars t
  | Eq (t,u,_) -> Sequence.append (T.Seq.vars t) (T.Seq.vars u)

let vars_of_hclause c =
  Sequence.append
    (vars_of_lit c.hc_head)
    (IArray.to_seq c.hc_body |> Sequence.flat_map vars_of_lit)

let pp_hclause out (c:horn_clause): unit =
  let pp_constr out = function
    | [] -> Fmt.silent out ()
    | l -> Fmt.fprintf out "| @[<hv>%a@]" (Fmt.list pp_constraint) l
  and pp_body out body =
    if IArray.length body > 0 then (
      Fmt.fprintf out " @<1>← @[<hv>%a@]" (Fmt.seq pp_lit) (IArray.to_seq body)
    );
  and pp_vars pp x out = function
    | [] -> pp out x
    | vars ->
      Fmt.fprintf out "@[<2>∀ %a.@ %a@]"
        (Util.pp_list ~sep:" " Type.pp_typed_var) vars pp x
  in
  let pp_main out () =
    Fmt.fprintf out "(@[%a@,%a@,%a@,%a@])"
      pp_lit c.hc_head
      pp_body c.hc_body
      pp_bool_trail_opt c.hc_trail
      pp_constr c.hc_constr
  in
  let vars = vars_of_hclause c |> T.VarSet.of_seq |> T.VarSet.to_list in
  pp_vars pp_main () out vars

let pp_hc_sup out sup : unit =
  Fmt.fprintf out
    "(@[<hv2>hc_sup@ :active %a@ :at %a@ :passive %a@ :at %a@ :subst %a@])"
    (Scoped.pp pp_hclause) sup.hc_sup_active
    Position.pp sup.hc_sup_active_pos
    (Scoped.pp pp_hclause) sup.hc_sup_passive
    Position.pp sup.hc_sup_passive_pos
    Subst.pp sup.hc_sup_subst

let pp_proof out (p:proof) : unit = match p with
  | P_trivial -> Fmt.string out "trivial"
  | P_from_stmt st ->
    Fmt.fprintf out "(@[from_stmt@ %a@])" Statement.pp_clause st
  | P_instance (c, subst) ->
    Fmt.fprintf out "(@[<hv2>instance@ :clause %a@ :subst %a@])"
      pp_clause c Subst.pp subst
  | P_avatar_split c ->
    Fmt.fprintf out "(@[<hv2>avatar_split@ :from %a@])" pp_clause c
  | P_split c ->
    Fmt.fprintf out "(@[<hv2>split@ %a@])" pp_clause c
  | P_bool_tauto -> Fmt.string out "bool_tauto"
  | P_bool_res r ->
    Fmt.fprintf out "(@[<hv>bool_res@ :on %a@ :c1 %a@ :c2 %a@])"
      pp_bool_lit r.bool_res_atom
      pp_bool_clause r.bool_res_c1
      pp_bool_clause r.bool_res_c2
  | P_hc_superposition sup -> pp_hc_sup out sup
  | P_hc_simplify c ->
    Fmt.fprintf out "(@[simplify@ %a@])" pp_hclause c

let equal_lit (a:lit) (b:lit): bool = match a, b with
  | Bool b1, Bool b2 -> b1=b2
  | Atom (t1,sign1), Atom (t2,sign2) -> T.equal t1 t2 && sign1=sign2
  | Eq (t1,u1,sign1), Eq (t2,u2,sign2) ->
    sign1=sign2 &&
    T.equal t1 t2 && T.equal u1 u2
  | Bool _, _
  | Atom _, _
  | Eq _, _
    -> false

let hash_lit : lit -> int = function
  | Bool b -> Hash.combine2 10 (Hash.bool b)
  | Atom (t,sign) -> Hash.combine3 20 (T.hash t) (Hash.bool sign)
  | Eq (t,u,sign) -> Hash.combine4 30 (T.hash t) (T.hash u) (Hash.bool sign)

let equal_atom (a:bool_atom) b : bool =
  begin match a, b with
    | A_fresh i, A_fresh j ->  i=j
    | A_box_clause r1, A_box_clause r2 -> r1.bool_box_id = r2.bool_box_id
    | A_ground r1, A_ground r2 -> r1.bool_ground_id = r2.bool_ground_id
    | A_select r1, A_select r2 -> r1.bool_select_id = r2.bool_select_id
    | A_fresh _, _
    | A_box_clause _, _
    | A_ground _, _
    | A_select _, _
      -> false
  end

let equal_bool_lit (a:bool_lit) b : bool =
  a.bl_sign = b.bl_sign &&
  equal_atom a.bl_atom b.bl_atom

let compare_atom (a:bool_atom) b : int =
  let to_int = function
    | A_fresh _ -> 0
    | A_box_clause _ -> 1
    | A_ground _ -> 2
    | A_select _ -> 3
  in
  begin match a, b with
    | A_fresh i, A_fresh j -> CCInt.compare i j
    | A_box_clause r1, A_box_clause r2 -> CCInt.compare r1.bool_box_id r2.bool_box_id
    | A_ground r1, A_ground r2 -> CCInt.compare r1.bool_ground_id r2.bool_ground_id
    | A_select r1, A_select r2 -> CCInt.compare r1.bool_select_id r2.bool_select_id
    | A_fresh _, _
    | A_box_clause _, _
    | A_ground _, _
    | A_select _, _
      -> CCInt.compare (to_int a)(to_int b)
  end

let compare_bool_lit (a:bool_lit) b : int =
  let c = CCBool.compare a.bl_sign b.bl_sign in
  if c<> 0 then c
  else compare_atom a.bl_atom b.bl_atom

let neg_bool_lit t = {t with bl_sign=not t.bl_sign}

(* same trail, modulo ordering *)
let equal_bool_trail (a:bool_trail) b: bool =
  let cmp (lazy a)(lazy b) = compare_bool_lit a b in
  assert (CCList.is_sorted ~cmp a);
  assert (CCList.is_sorted ~cmp b);
  CCList.equal (fun a b-> cmp a b=0) a b

let hash_bool_lit a : int = match a.bl_atom with
  | A_fresh i -> Hash.combine3 10 (Hash.bool a.bl_sign) (Hash.int i)
  | A_box_clause r -> Hash.combine2 15 (Hash.int r.bool_box_id)
  | A_select r ->
    Hash.combine3 20 (Hash.bool a.bl_sign) (Hash.int r.bool_select_id)
  | A_ground r -> Hash.combine2 50 (Hash.int r.bool_ground_id)

let pp_stage out = function
  | Stage_init -> Fmt.string out "init"
  | Stage_presaturate -> Fmt.string out "presaturate"
  | Stage_start -> Fmt.string out "start"
  | Stage_exit -> Fmt.string out "exit"

let pp_event out (e:event): unit = match e with
  | E_add_component r ->
    Fmt.fprintf out "(@[add_component@ %a@])" pp_clause r.bool_box_clause
  | E_remove_component r ->
    Fmt.fprintf out "(@[remove_component@ %a@])" pp_clause r.bool_box_clause
  | E_select_lit (r,cstr) ->
    Fmt.fprintf out "(@[select_lit@ %a@ :clause %a@ :constr (@[%a@])@])"
      pp_lit r.bool_select_lit
      pp_clause r.bool_select_clause
      (Util.pp_list Dismatching_constr.pp) cstr
  | E_unselect_lit r ->
    Fmt.fprintf out "(@[select_lit@ %a@ :clause %a@])"
      pp_lit r.bool_select_lit
      pp_clause r.bool_select_clause
  | E_add_ground_lit r ->
    Fmt.fprintf out "(@[add_ground_lit@ %a@])" pp_lit r.bool_ground_lit
  | E_remove_ground_lit r ->
    Fmt.fprintf out "(@[remove_ground_lit@ %a@])" pp_lit r.bool_ground_lit
  | E_conflict (c,p) ->
    Fmt.fprintf out "(@[conflict@ :clause %a@ :proof %a@])"
      pp_bool_clause c pp_proof p
  | E_if_sat -> Fmt.string out "if_sat"
  | E_found_unsat p ->
    Fmt.fprintf out "(@[found_unsat@ :proof %a@])" pp_proof p
  | E_stage s -> Fmt.fprintf out "(@[stage %a@])" pp_stage s
