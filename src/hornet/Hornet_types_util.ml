
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basics for Hornet Types} *)

open Logtk
open Hornet_types

module T = FOTerm
module Fmt = CCFormat
module Stmt = Statement

let pp_lit out (t:lit): unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t
  | Eq (t,u,true) -> Fmt.fprintf out "@[%a@ = %a@]" T.pp t T.pp u
  | Eq (t,u,false) -> Fmt.fprintf out "@[%a@ @<1>≠ %a@]" T.pp t T.pp u

let pp_clause_lits out a =
  Fmt.fprintf out "@[%a@]" (Util.pp_seq ~sep:" ∨ " pp_lit) (IArray.to_seq a.c_lits)

let pp_atom out = function
  | A_fresh i -> Fmt.fprintf out "fresh_%d" i
  | A_box_clause b -> Fmt.fprintf out "%a" pp_clause_lits b.bool_box_clause
  | A_ground r -> pp_lit out r.bool_ground_lit

let pp_select out (r:select_lit) =
  Fmt.fprintf out
    "@[select@ :idx %d@ :lit %a@]"
    r.select_idx pp_lit r.select_lit

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
    Fmt.fprintf out "@ @[<hv2>@<1>⇐@ %a@]" pp_bool_trail trail

let pp_constraint out (c:c_constraint): unit =
  Fmt.fprintf out "{@[<hv>%a@]}"
    (Util.pp_list Dismatching_constr.pp) c.constr_dismatch

let pp_constraint_opt out (c:c_constraint): unit =
  if not (List.for_all Dismatching_constr.is_trivial c.constr_dismatch) then (
    Fmt.fprintf out "@ | %a" pp_constraint c
  )

let pp_hc_status out (s:horn_clause_status): unit = match s with
  | HC_alive -> Fmt.string out "alive"
  | HC_dead -> Fmt.string out "dead"

let vars_of_lit (l:lit): _ HVar.t Sequence.t = match l with
  | Bool _ -> Sequence.empty
  | Atom (t,_) -> T.Seq.vars t
  | Eq (t,u,_) -> Sequence.append (T.Seq.vars t) (T.Seq.vars u)

let vars_of_clause (c:clause) =
  IArray.to_seq c.c_lits |> Sequence.flat_map vars_of_lit

let vars_of_hclause (c:horn_clause) =
  Sequence.append
    (vars_of_lit c.hc_head)
    (IArray.to_seq c.hc_body |> Sequence.flat_map vars_of_lit)

(* remove trivial bindings from the substitution *)
let lc_filter_subst lc_subst: (var*term) Sequence.t =
  Type.VarMap.to_seq lc_subst
  |> Sequence.filter
    (fun (v,t) -> match T.view t with
       | T.Var v' -> not (HVar.equal Type.equal v v')
       | _ -> true)

(* stuff for printing clauses *)
module PP_c = struct
  let pp_body out body =
    if IArray.length body > 0 then (
      Fmt.fprintf out " @<1>← @[<hv>%a@]" (Util.pp_seq ~sep:" ∧ " pp_lit) (IArray.to_seq body)
    );
  and pp_vars pp x out = function
    | [] -> pp out x
    | vars ->
      Fmt.fprintf out "@[<2>∀ %a.@ %a@]"
        (Util.pp_list ~sep:" " Type.pp_typed_var) vars pp x
  and pp_lits out (lits:lit IArray.t) =
    Fmt.fprintf out "[@[<hv>%a@]]" (Fmt.seq pp_lit) (IArray.to_seq lits)
end

let pp_clause out (c:clause): unit =
  let open PP_c in
  let vars = vars_of_clause c |> T.VarSet.of_seq |> T.VarSet.to_list in
  let pp_main out () =
    Fmt.fprintf out "@[<hv2>%a%a@]"
      pp_clause_lits c pp_constraint_opt c.c_constr
  in
  pp_vars pp_main () out vars

let pp_lc out (lc:labelled_clause): unit =
  let pp_subst out lc =
    Fmt.fprintf out "{@[<hv>%a@]}"
      Fmt.(seq (pair ~sep:(return "@ -> ") HVar.pp T.pp))
      (lc_filter_subst lc.lc_subst)
  in
  Fmt.fprintf out "(@[%a@ @[:subst %a@]@ @[:select `%a`/%d@]@])"
    pp_clause lc.lc_clause pp_subst lc pp_lit lc.lc_sel.select_lit
    lc.lc_sel.select_idx

let pp_label out (l:label): unit =
  Fmt.fprintf out "{@[<hv>%a@]}" (Fmt.list pp_lc) l

let pp_hclause out (c:horn_clause): unit =
  let open PP_c in
  let pp_label_opt out = function
    | [] -> ()
    | lcs -> Fmt.fprintf out "@ label:%a" pp_label lcs
  in
  let pp_main out () =
    Fmt.fprintf out "(@[%a%a%a%a%a@])"
      pp_lit c.hc_head
      pp_body c.hc_body
      pp_bool_trail_opt c.hc_trail
      pp_constraint_opt c.hc_constr
      pp_label_opt c.hc_label
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
  | P_from_file (f,r) ->
    Fmt.fprintf out "(@[<2>file %a@ :role %a@])"
      Stmt.Src.pp_from_file f Stmt.Src.pp_role r
  | P_from_input r ->
    Fmt.fprintf out "(@[<2>input :role %a@])" Stmt.Src.pp_role r
  | P_cnf _ -> Fmt.string out "cnf"
  | P_cnf_neg _ -> Fmt.string out "cnf_neg"
  | P_renaming (_,id,_) -> Fmt.fprintf out "(@[renaming :id %a@])" ID.pp id
  | P_preprocess (_,msg) -> Fmt.fprintf out "(@[preprocess %S@])" msg
  | P_instance (c, subst) ->
    Fmt.fprintf out "(@[<hv2>instance@ :clause %a@ :subst %a@])"
      pp_clause c Subst.pp subst
  | P_avatar_split c ->
    Fmt.fprintf out "(@[<hv2>avatar_split@ :from %a@])" pp_clause c
  | P_avatar_cut (c, lits) ->
    let pp_pair out (l,_) = pp_bool_lit out l in
    Fmt.fprintf out "(@[<hv2>avatar_cut@ :from %a@ :lits (@[<hv>%a@])@])"
      pp_hclause c (Util.pp_list pp_pair) lits
  | P_split (c,sel,constr) ->
    Fmt.fprintf out "(@[<hv2>split@ :clause %a@ :idx %a@ :constr %a@])"
      pp_clause c pp_select sel pp_constraint constr
  | P_bool_tauto -> Fmt.string out "bool_tauto"
  | P_bool_res r ->
    Fmt.fprintf out "(@[<hv>bool_res@ :on %a@ :c1 %a@ :c2 %a@])"
      pp_bool_lit r.bool_res_atom
      pp_bool_clause r.bool_res_c1
      pp_bool_clause r.bool_res_c2
  | P_bool_grounding c ->
    Fmt.fprintf out "(@[<2>grounding@ %a@])" pp_clause c
  | P_hc_superposition sup -> pp_hc_sup out sup
  | P_hc_eq_res (c,subst) ->
    Fmt.fprintf out "(@[<hv>eq_resolution@ :on %a@ :subst %a@])"
      pp_hclause c Subst.pp subst
  | P_hc_simplify c ->
    Fmt.fprintf out "(@[simplify@ %a@])" pp_hclause c
  | P_hc_demod (c,c_l) ->
    Fmt.fprintf out "(@[demod@ :clause %a@ :rules {@[<hv>%a@]}@])"
      pp_hclause c (Util.pp_list pp_hclause) c_l

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
    | A_fresh _, _
    | A_box_clause _, _
    | A_ground _, _
      -> false
  end

let equal_bool_lit (a:bool_lit) b : bool =
  a.bl_sign = b.bl_sign &&
  equal_atom a.bl_atom b.bl_atom

let equal_clause (a:clause) b: bool = a.c_id = b.c_id
let hash_clause (a:clause) : int = CCHash.int a.c_id
let compare_clause a b: int = CCInt.compare a.c_id b.c_id

let equal_lc (a:labelled_clause) b: bool =
  equal_clause a.lc_clause b.lc_clause &&
  Type.VarMap.equal T.equal a.lc_subst b.lc_subst

let hash_lc (a:labelled_clause): int =
  CCHash.combine2
    CCHash.(seq (pair HVar.hash T.hash) (Type.VarMap.to_seq a.lc_subst))
    (hash_clause a.lc_clause)

let compare_lc (a:labelled_clause) b: int =
  CCOrd.(compare_clause a.lc_clause b.lc_clause
      <?> (Type.VarMap.compare T.compare, a.lc_subst, b.lc_subst))

let compare_atom (a:bool_atom) b : int =
  let to_int = function
    | A_fresh _ -> 0
    | A_box_clause _ -> 1
    | A_ground _ -> 2
  in
  begin match a, b with
    | A_fresh i, A_fresh j -> CCInt.compare i j
    | A_box_clause r1, A_box_clause r2 -> CCInt.compare r1.bool_box_id r2.bool_box_id
    | A_ground r1, A_ground r2 -> CCInt.compare r1.bool_ground_id r2.bool_ground_id
    | A_fresh _, _
    | A_box_clause _, _
    | A_ground _, _
      -> CCInt.compare (to_int a)(to_int b)
  end

let compare_bool_lit (a:bool_lit) b : int =
  let c = CCBool.compare a.bl_sign b.bl_sign in
  if c<> 0 then c
  else compare_atom a.bl_atom b.bl_atom

let neg_bool_lit t = {t with bl_sign=not t.bl_sign}

let int_of_atom (a:bool_atom): int = match a with
  | A_box_clause r -> r.bool_box_id
  | A_fresh i -> i
  | A_ground r -> r.bool_ground_id

let int_of_bool_lit (t:bool_lit): int =
  let i = int_of_atom t.bl_atom in
  if t.bl_sign then i else -i

(* same trail, modulo ordering *)
let equal_bool_trail (a:bool_trail) b: bool =
  let cmp (lazy a)(lazy b) = compare_bool_lit a b in
  assert (CCList.is_sorted ~cmp a);
  assert (CCList.is_sorted ~cmp b);
  CCList.equal (fun a b-> cmp a b=0) a b

let subsumes_bool_trail (l1:bool_trail) (l2:bool_trail): bool =
  let rec aux l1 l2 = match l1, l2 with
    | [], _ -> true
    | _, [] -> false
    | lazy t1 :: tail1, lazy t2 :: tail2 ->
      begin match compare_bool_lit t1 t2 with
        | 0 -> aux tail1 tail2
        | n when n<0 -> false (* all elements of [l2] are bigger than [t1] *)
        | _ -> aux l1 tail2 (* drop [t2] *)
      end
  in
  aux l1 l2

let hash_bool_lit a : int = match a.bl_atom with
  | A_fresh i -> Hash.combine3 10 (Hash.bool a.bl_sign) (Hash.int i)
  | A_box_clause r -> Hash.combine2 15 (Hash.int r.bool_box_id)
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
  | E_select_lit (c,r,cstr) ->
    Fmt.fprintf out "(@[<hv>select_lit@ %a@ :clause %a@ :constr %a@])"
      pp_lit r.select_lit
      pp_clause c
      pp_constraint cstr
  | E_unselect_lit (c,r) ->
    Fmt.fprintf out "(@[unselect_lit@ %a@ :clause %a@])"
      pp_lit r.select_lit pp_clause c
  | E_add_ground_lit r ->
    Fmt.fprintf out "(@[add_ground_lit@ %a@])" pp_lit r.bool_ground_lit
  | E_remove_ground_lit r ->
    Fmt.fprintf out "(@[remove_ground_lit@ %a@])" pp_lit r.bool_ground_lit
  | E_conflict (trail,l,p) ->
    Fmt.fprintf out "(@[<hv>conflict@ :trail %a@ :label %a@ :proof %a@])"
      pp_bool_trail trail pp_label l pp_proof p
  | E_if_sat -> Fmt.string out "if_sat"
  | E_found_unsat (p,_) ->
    Fmt.fprintf out "(@[found_unsat@ :proof %a@])" pp_proof p
  | E_stage s -> Fmt.fprintf out "(@[stage %a@])" pp_stage s

