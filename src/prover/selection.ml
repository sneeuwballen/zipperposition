
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = Term
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV

type t = Literal.t array -> CCBV.t

type parametrized = strict:bool -> ord:Ordering.t -> t

(* no need for classification here *)
let no_select _ : BV.t = BV.empty ()

let _ho_restriction = ref `None
let _restrict_fresh_sk_selection = ref true

(* May we select this literal? *)
let can_select_lit ~ord (lits:Lits.t) (i:int) : bool =
  if Lit.is_negativoid lits.(i)
  then (
    (* Search for the (maximal) terms with variable heads.
       Returns a list of (var_head, args) pairs. *)
    let var_headed_subterms which =
      let lits =
        if which = `Max
        then Lits.maxlits_l ~ord lits |> CCList.map (fun (l,_) -> l) |> Array.of_list
        else lits
      in
      lits |> CCArray.fold (
        fun vars lit ->
          let new_vars =
            Lit.fold_terms ~vars:true ~ty_args:false ~which ~ord ~subterms:false lit
            |> Iter.map (fun (t,_) -> T.as_app t)
            |> Iter.filter (fun (head,_) -> T.is_var head)
            |> Iter.to_list
          in
          CCList.append new_vars vars
      ) [] in
    (* Given a list of (var_head, args) pairs, check whether our lit contains
       one of those variables, but with different arguments. *)
    let occur_with_other_args vars_args =
      Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
      |> Iter.exists (fun (t,_) ->
          let t_head, t_args = T.as_app t in
          vars_args |> CCList.exists (fun (head, args) -> T.equal head t_head && not @@ T.same_l_gen t_args args)
        ) in
    let contains_maxvar_as_fo_subterm vars_args =
      let vars,_ = CCList.split vars_args in
      Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
      |> Iter.exists (fun (t, _) -> 
          (T.is_var t) && List.exists (T.equal t) vars)  in
    let is_ho_allowed = 
      match !_ho_restriction with
      | `None -> true
      | `NoVarHeadingMaxTerm ->
        (* Don't select literals containing a variable that heads the maximal term
          of the clause but occurs with different arguments there. *)
        let vars_args = var_headed_subterms `Max in
        not (occur_with_other_args vars_args)
      | `NoVarDifferentArgs ->
        (* Don't selected if the literal contains a variable that is applied to
          different arguments in the clause *)
        let vars_args = var_headed_subterms `All in
        not (occur_with_other_args vars_args)
      | `NoUnappliedVarOccurringApplied ->
        (* Don't selected if the literal contains an unapplied variable that also
          occurs applied in the clause *)
        let vars_args = var_headed_subterms `All in
        Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
        |> Iter.exists (fun (t,_) ->
            vars_args |> CCList.exists (fun (head, args) -> T.equal head t && not (CCList.is_empty args))
          )
      | `NoHigherOrderVariables ->
        (* We cannot select literals containing a HO variable: *)
        not (
          Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
          |> Iter.exists (fun (t,_) -> T.is_ho_var (fst (T.as_app t)))
        )
      | `NoMaxVarInFoContext ->
        let vars_args = var_headed_subterms `Max in
        not (contains_maxvar_as_fo_subterm vars_args)
    in
    let is_fresh_sk_allowed =
      not !_restrict_fresh_sk_selection || (
        match lits.(i) with
        | Lit.Equation(lhs,rhs,_) when Lit.is_predicate_lit lits.(i) ->
          (match T.as_const (T.head_term lhs) with 
           | Some sym -> not ((ID.is_postcnf_skolem sym) && not (ID.is_lazycnf_skolem sym))
           | _ -> true)
        | _ -> true
      )
    in
    is_ho_allowed && is_fresh_sk_allowed
  )
  else false

(* checks that [bv] is an acceptable selection for [lits]. In case
   some literal is selected, at least one negative literal must be selected. *)
let validate_fun_ ~ord lits bv =
  if BV.is_empty bv then true
  else (
    Iter.of_array_i lits
    |> Iter.exists
      (fun (i,_) -> not (BV.get bv i) || can_select_lit ~ord lits i)
  )

(* build a selection function in general, given the more specialized
   one there *)
let mk_ ~ord ~(f:Lits.t -> BV.t) (lits:Lits.t) : BV.t =
  if Array.length lits <= 1 then BV.empty ()
  else (
    (* should we select anything? *)
    let should_select = CCList.exists (can_select_lit ~ord lits) (CCList.range' 0 (CCArray.length lits)) in
    if should_select then (
      let bv = f lits in
      assert (validate_fun_ ~ord lits bv);
      bv
    ) else (
      (*Util.debugf ~section 5 "(@[should-not-select@ %a@])" (fun k->k Lits.pp lits);*)
      BV.empty ()
    )
  )

let bv_first_ bv = BV.iter_true bv |> Iter.head

let max_goal ~strict ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
      let bv = Lits.maxlits ~ord lits in
      (* only retain negative normal lits, or constraints
         that are unshielded *)
      BV.filter bv (fun i -> can_select_lit ~ord lits i);
      begin match bv_first_ bv with
        | Some i ->
          (* keep only first satisfying lit *)
          BV.clear bv;
          BV.set bv i;
          if not strict then (
            BV.union_into ~into:bv (Lits.pos lits);
          );
          bv
        | None ->
          BV.empty ()  (* empty one *)
      end)

let ho_sel_driver lits f =
  let neg_max = CCArray.mapi (fun i l -> i,l)  lits
                |> CCArray.filter (fun (_,l) -> Lit.is_negativoid l) in
  if CCArray.length neg_max = 0 then BV.empty ()
  else (
    CCArray.fast_sort (fun (i, _) (j, _) -> compare (f i) (f j)) neg_max;
    let idx, _ = CCArray.get neg_max 0 in
    let res = BV.empty () in
    BV.set res idx;
    res
  )

let avoid_app_var ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
      let avoid_av_feature i  =
        let l = lits.(i) in
        (Lit.is_app_var_eq l,
         not (Lits.is_max ~ord lits i),
         Lit.Seq.terms l |> Iter.fold (fun acc t ->
             if (T.is_app_var t) then (acc+1) else acc) 0,
         Lit.weight l) in
      ho_sel_driver lits avoid_av_feature
    )

let prefer_app_var ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
      let prefer_av_feature i  =
        let l = lits.(i) in
        (not (Lit.is_app_var_eq l),
         not (Lits.is_max ~ord lits i),
         - (Lit.Seq.terms l |> Iter.fold (fun acc t ->
             if (T.is_app_var t) then (acc+1) else acc) 0),
         -Lit.weight l) in
      ho_sel_driver lits prefer_av_feature
    )

let find_min_lit ~blocker ~can_sel ~ord ~chooser lits =
  CCArray.to_iter lits
  |> Iter.mapi (fun i l -> chooser (i,l), i)
  |> Iter.sort
  |> Iter.find_map (fun (_,i) ->
    let lit = lits.(i) in
    if can_sel ~ord lits i && not (blocker i lit) 
    then Some (lit,i)
    else None)

let weight_based_sel_driver ?(blocker=(fun _ _ -> false)) ~can_sel ~ord lits f =
  match find_min_lit ~blocker ~can_sel ~ord ~chooser:f lits with
  | None -> BV.empty ()
  | Some (_, idx) ->
    assert(can_select_lit ~ord lits idx);
    assert(not (blocker idx (CCArray.get lits idx)));
    let res = BV.empty () in
    BV.set res idx;
    res

let mk_alpha_rank_map lits = 
  CCArray.fold (fun syms l -> 
    match l with
    | Lit.Equation(lhs,_,_) when T.is_const lhs ->
      (T.as_const_exn lhs, ID.name (T.as_const_exn lhs)) :: syms
    | _ -> syms
  ) [] lits
  |> List.sort (fun (_,x) (_,y) -> CCString.compare x y)
  |> CCList.foldi (fun acc i (id,_) -> ID.Map.add id i acc) ID.Map.empty 

let lit_sel_diff_w l =
  match l with
  | Lit.Equation(lhs,rhs,_) ->
    let lhs_w,rhs_w = CCPair.map_same T.ho_weight (lhs,rhs) in
    100*((max lhs_w rhs_w) - (min lhs_w rhs_w)) + lhs_w + rhs_w
  | _ -> 0

let pred_freq ~ord lits = 
  Literals.fold_eqn ~both:false ~ord ~eligible:(fun _ _ -> true) lits
  |> Iter.fold (fun acc (l,r,sign,_) -> 
      if sign && T.equal T.true_ r then (
        (* positive predicate *)
        let hd = T.head l in
        match hd with 
        | None -> acc
        | Some id -> 
          let current_val = ID.Map.get_or id acc ~default:0 in
          ID.Map.add id (current_val+1) acc
      ) else acc
    ) ID.Map.empty

let is_truly_equational l = not (Literal.is_predicate_lit l)

let get_pred_freq ~freq_tbl lit =
  match lit with
  | Lit.Equation(l,r,_) when Lit.is_predicate_lit lit ->
    begin 
      match T.head l with
      | Some id -> ID.Map.get_or id freq_tbl ~default:0
      | None -> max_int
    end
  | _ -> max_int

let e_sel ~blocker ~ord lits = 
  let chooser ~freq_tbl (i,l) = 
    ((if Lit.is_positivoid l then 1 else 0),
     ((if Lits.is_max ~ord lits i then 0 else 100) +
      (if Lit.is_pure_var l then 0 else 10) +
      (if Lit.is_ground l then 0 else 1)),
     -(lit_sel_diff_w l),
     get_pred_freq ~freq_tbl l) in
  let freq_tbl = pred_freq ~ord lits in

  if CCArray.length (CCArray.filter Lit.is_negativoid lits) == 0 ||
     CCBV.cardinal (Literals.maxlits ~ord lits) <= 1 then CCBV.empty ()
  else (
    weight_based_sel_driver ~can_sel:can_select_lit ~ord lits (chooser ~freq_tbl) ~blocker
  )  

let e_sel2 ~blocker ~ord lits = 
  let alpha_map = mk_alpha_rank_map lits in
  let blocker x l = blocker x l || Lit.is_type_pred l || Lit.is_predicate_lit l in
  let chooser (idx ,l) = 
    let sign_val = if Lit.is_positivoid l then 1 else 0 in 
    let diff_val = -(lit_sel_diff_w l) in
    let prec = Ordering.precedence ord in
    match l with 
    | Equation(lhs,_,_) when not @@ blocker idx l ->
      if T.is_var (T.head_term lhs) then (
        (sign_val, 0, 0, diff_val)
      ) else (
        let hd_is_cst = T.is_const (T.head_term lhs) in
        let prec_weight = 
          if not hd_is_cst then 0
          else Precedence.sel_prec_weight prec (T.head_exn lhs) in
        let alpha_rank = 
          if not hd_is_cst then max_int
          else (ID.Map.get_or ~default:max_int (T.as_const_exn (T.head_term lhs)) alpha_map) in

        (sign_val, -prec_weight, alpha_rank, diff_val)
      )
    | _ -> (sign_val,max_int,max_int,diff_val) (* won't be chosen *) in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser

let e_sel3 ~blocker ~ord lits = 
  let chooser (i,l) = 
    let sign = (if Lit.is_positivoid l then 1 else 0) in
    if Lit.is_pure_var l then (
      (sign, 0, 0, 0)
    ) else if (Lit.is_ground l) then (
      (sign, 10, Lit.weight l, 0)
    ) else (
      (sign, 20, - (lit_sel_diff_w l) , 0)
    ) in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser 

let e_sel4 ~blocker ~ord lits =
  let freq_tbl = pred_freq ~ord lits in
  let chooser (i,l) = 
    let lhs = match l with
      | Lit.Equation(lhs_t,_,_) -> lhs_t
      | _ -> T.true_ (* a term to fill in *) in
    let sign = if Lit.is_positivoid l then 1 else 0 in
    let hd_freq = get_pred_freq ~freq_tbl l in
    if Lit.is_ground l then (
      (sign, 0, -(T.ho_weight lhs), hd_freq) 
    ) else if not @@ Lit.is_typex_pred l then (
      let max_term_weight = 
        Iter.of_list (Lit.Comp.max_terms ~ord l)
        |> Iter.map (T.weight ~var:0)
        |> Iter.sum in
      (sign, 10, -max_term_weight, hd_freq)
    ) else if not @@ Lit.is_type_pred l then (
      (sign, 20, -(T.ho_weight lhs), hd_freq)
    ) else (sign, max_int, max_int, max_int) in
  let blocker x l = blocker x l || Lit.is_type_pred l in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser 

let e_sel5 ~blocker ~ord lits =
  let chooser (i,l) =
    (if Lit.is_positivoid l then 1 else 0), 
    (if Lit.is_ground l then 0 else 1),
    (- (lit_sel_diff_w l)),
    0 in
  if CCArray.exists (fun l -> Lit.is_negativoid l && Lit.depth l <= 2) lits then (
    weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser 
  ) else BV.empty ()

let e_sel6 ~blocker ~ord lits =
  (* SelectLargestOrientable *)
  let is_oriented lit = 
    match lit with
    | Lit.Equation(l,r,_) ->
      Ordering.compare ord l r != Comparison.Incomparable
    | _ -> true in
  let chooser (i,l) =
    (if Lit.is_positivoid l then 1 else 0),
    (if is_oriented l then 0 else 1),
    (- (Lit.weight l)),
    0 in
  let blocker x t = blocker x t || not @@ is_oriented t in
  weight_based_sel_driver  ~can_sel:can_select_lit ~ord lits chooser ~blocker

let e_sel7 ~blocker ~ord lits = 
  (* SelectComplexExceptRRHorn *)
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else e_sel3 ~ord ~blocker lits

let e_sel8 ~blocker ~ord lits = 
  let alpha_map = mk_alpha_rank_map lits in
  let get_arity = function 
    | Lit.Equation(l,r,_) as lit when Lit.is_predicate_lit lit -> 
      List.length (Type.expected_args (Term.ty (T.head_term l)))
    | _ -> 0  in
  let alpha_rank = function 
    | Lit.Equation(l,_,_) as lit when Lit.is_predicate_lit lit 
        && Lit.is_positivoid lit &&  T.is_const (T.head_term l) -> 
      ID.Map.get_or ~default:max_int (T.head_exn l) alpha_map
    | _ -> max_int  in
  let is_propositional = function
  | (Lit.Equation(lhs, _, _) as l) ->
    Lit.is_predicate_lit l && T.is_const lhs
  | _ -> false
  in
  let blocker x l = blocker x l || Lit.is_type_pred l || is_propositional l in
  let chooser (i,l) =
    if is_truly_equational l then (
      (if Lit.is_positivoid l then 1 else 0),
      min_int, 0, lit_sel_diff_w l
    ) else (
      (if Lit.is_positivoid l then 1 else 0),
      (if not (blocker i l) then -(get_arity l) else max_int), 
      alpha_rank l, lit_sel_diff_w l
    )
  in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord lits chooser ~blocker

let e_sel9 ~blocker ~ord lits =
  (* SelectLargestOrientable *)
  let lhs_head_arity lit = 
    match lit with 
    | Lit.Equation (lhs, _, _) ->
      let hd = T.head_term lhs in
      if T.is_const hd then -(CCList.length (Type.expected_args (Term.ty hd)))
      else max_int
    | _ -> max_int in

  let lhs_head_alpha lit = 
    match lit with 
    | Lit.Equation (lhs, _, _) ->
      let hd = T.head_term lhs in
      if T.is_const hd then ID.id (T.as_const_exn hd)
      else max_int
    | _ -> max_int in


  let chooser (i,l) =
    (if is_truly_equational l then max_int else 0),
    (lhs_head_arity l),
    (lhs_head_alpha l),
    0 in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser

let e_sel10 ~blocker ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
    let bv = Lits.neg lits in
    (* select all selectable negative lits *)
    BV.filter bv (fun i -> 
      not (blocker i lits.(i)) &&
      can_select_lit ~ord lits i);
    bv)

let e_sel11 ~blocker ~ord lits =
  let freq_tbl = pred_freq ~ord lits in
  let blocker x l = blocker x l || Lit.is_type_pred l in
  let eqn_max_weight = function 
  | Lit.Equation(lhs,rhs,_) as l when Lit.is_negativoid l ->
    let cmp_res = Ordering.compare ord lhs rhs in
    if cmp_res == Comparison.Gt then Term.ho_weight lhs 
    else if cmp_res == Comparison.Lt then Term.ho_weight rhs
    else Term.ho_weight lhs + Term.ho_weight rhs
  | _ -> max_int in
  let lhs_weight  = function 
  | Lit.Equation(lhs,rhs,_) as l when Lit.is_negativoid l ->
    Term.ho_weight lhs
  | _ -> max_int in

  let chooser (i,l) =
    if Lit.is_positivoid l then (max_int,max_int,max_int)
    else if Lit.is_ground l then (0, lhs_weight l, get_pred_freq ~freq_tbl l)
    else if not (Lit.is_typex_pred l) then (10, eqn_max_weight l, get_pred_freq ~freq_tbl l)
    else if not (Lit.is_type_pred l) then (20, - (lhs_weight l), get_pred_freq ~freq_tbl l)
    else (max_int, max_int, max_int) in
  
  if CCArray.exists (fun l -> Lit.is_negativoid l && Lit.depth l <= 2) lits then (
    let sel_bv = 
      weight_based_sel_driver ~can_sel:can_select_lit ~ord lits chooser ~blocker in
    assert(CCBV.cardinal sel_bv <= 1);
    if not (CCBV.is_empty sel_bv) then (
      CCArray.iteri (fun i lit ->
        assert(not (CCBV.get sel_bv i) || Lit.is_negativoid lit);
        if Lit.is_positivoid lit then CCBV.set sel_bv i;
      ) lits;
    );
    sel_bv    
  ) else BV.empty ()

let e_sel12 ~blocker ~ord lits =
  if Lits.is_unique_max_horn_clause ~ord lits
  then BV.empty () 
  else e_sel3 ~blocker ~ord lits

let e_sel13 ~blocker ~ord lits =
   let chooser (i,l) =
    (if Lit.is_positivoid l then 1 else 0), 
    (if Lit.is_ground l then 0 else 1),
    (- (lit_sel_diff_w l)),
    0 in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser

let e_sel14 ~blocker ~ord lits =
  let freq_tbl = pred_freq ~ord lits in

  let blocked = CCBV.create ~size:(CCArray.length lits) false in

  let chooser (i,l) =
    let hd_freq = get_pred_freq ~freq_tbl l in
    let hd_is_fresh_pred = function
      | Lit.Equation(lhs, rhs, _) as l when Lit.is_predicate_lit l -> 
        begin match T.as_const (T.head_term lhs) with
        | Some c -> ID.is_postcnf_skolem c
        | None -> false end
      | _ -> false in
    let max_val = (max_int, max_int, max_int) in

    let block b i = 
      CCBV.set b i;
      max_val in
    
    if Lit.is_positivoid l ||  hd_is_fresh_pred l then (block blocked i)
    else if Lit.is_ground l then (0, Lit.weight l, hd_freq)
    else if not (Lit.is_typex_pred l) then (10, Lit.max_term_positions ~ord l, hd_freq)
    else if not (Lit.is_type_pred l) then (20, - (Lit.ho_weight l), hd_freq)
    else (block blocked i) in
  
  let blocker blocked i l = 
    blocker i l ||
    CCBV.get blocked i in

  weight_based_sel_driver ~can_sel:can_select_lit ~ord lits chooser ~blocker:(blocker blocked)

let e_sel15 ~blocker ~ord lits =
  let (<+>) = CCOpt.Infix.(<+>)  in
  let lits_l = CCList.mapi (fun i l -> (i,l)) (CCArray.to_list lits) in
  let sel_bv = CCBV.create ~size:(CCArray.length lits) false in
  let res = 
    (* find first negative pure var lit *)
    (CCList.find_map (fun (i, l) ->
      if Lit.is_negativoid l && Lit.is_pure_var l then (
        CCBV.set sel_bv i;
        Some sel_bv
      ) else None
    ) lits_l)
  <+>
    (* else find smallest negative ground lit *)
    (lits_l
     |> CCList.filter_map (fun (i, l) -> 
          if Lit.is_negativoid l && Lit.is_ground l then (
            Some (i, Lit.ho_weight l)
          ) else None)
     |> CCList.to_iter
     |> Iter.min ~lt:(fun (_, w1) (_, w2) -> w1 < w2)
     |> CCOpt.map (fun (i,_) -> CCBV.set sel_bv i; sel_bv))
  <+>
    (* else if there is a _single_ maximal positive literal,
       do not select anything *)
    (let max_lits = Literals.maxlits ~ord lits in
     if CCBV.cardinal max_lits = 1 &&
        Lit.is_positivoid lits.(CCBV.first_exn max_lits) then (
        assert(CCBV.is_empty sel_bv);
        Some sel_bv
     ) else None)
  <+>
    (* else behave as SelectNewComplexAHPNS  *)
    Some (e_sel14 ~blocker ~ord lits) in
  CCOpt.get_exn res
  |> (fun res_bv -> 
        let block_bv = 
          CCBV.create ~size:(CCArray.length lits) true in
        CCArray.iteri (fun i lit ->
          if blocker i lit then CCBV.reset block_bv i;
        ) lits;
        CCBV.inter res_bv block_bv
  )

let e_sel16 ~blocker ~ord lits =
  let blocked = CCBV.create ~size:(Array.length lits) false in
  let chooser (i,l) =
    match l with 
    | Lit.Equation(lhs,rhs, _) ->
      let hd_val =
        if not (T.is_const (T.head_term lhs)) then 0
        else ID.id (T.as_const_exn (T.head_term lhs)) in
      if T.is_true_or_false rhs then (
        (* predicate literal *)
        (-2, hd_val, lit_sel_diff_w l)
      ) else if Lit.is_type_pred l then (
        (* equational literal *)
        CCBV.set blocked i;
        (max_int, max_int, max_int)
      ) else (
        let hd_arity = List.length (Type.expected_args (T.ty (Term.head_term lhs))) in
        (-hd_arity, hd_val, lit_sel_diff_w l))
      | _ -> (max_int,max_int,max_int) in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord lits chooser 
                          ~blocker:(fun i l -> blocker i l || CCBV.get blocked i)

let e_sel17 ~blocker ~ord lits =
  let res = CCBV.create ~size:(Array.length lits) false in 
  if CCBV.cardinal (Literals.maxlits ~ord lits) > 1 then (
    let chooser (i,l) =
      (if Lit.is_positivoid l then 1 else 0), 
      (if Lit.is_ground l then 0 else 1),
      (- (lit_sel_diff_w l)),
      0 
    in
    weight_based_sel_driver ~can_sel:can_select_lit ~blocker ~ord lits chooser
  ) else res

let e_sel18 ~blocker ~ord lits =
  let alpha_map = mk_alpha_rank_map lits in
  let chooser (i,l) =
    match l with
    | Lit.Equation(lhs, _, _) ->
      (if Lit.is_positivoid l then 1 else 0), 
      (if Lit.is_predicate_lit l then 1 else 0),
      (-(List.length (Term.args lhs))),
      (if not (T.is_const (T.head_term lhs)) then max_int
        else ID.Map.get_or ~default:max_int (T.as_const_exn (T.head_term lhs)) alpha_map),
      (- (lit_sel_diff_w l))
    | _ -> (max_int, max_int, max_int, max_int, max_int)
  in
  let blocker i lit = blocker i lit || Lit.is_typex_pred lit in
  weight_based_sel_driver ~can_sel:can_select_lit ~blocker ~ord lits chooser

let e_sel19 ~blocker ~ord lits = 
  let chooser (i,l) = 
    ((if Lit.is_positivoid l then 1 else 0),
     ((if Lits.is_max ~ord lits i then 0 else 100) +
      (if Lit.is_pure_var l then 0 else 10) +
      (if Lit.is_ground l then 0 else 1)),
     -(lit_sel_diff_w l)) in

  if CCArray.length (CCArray.filter Lit.is_negativoid lits) == 0 ||
     CCBV.cardinal (Literals.maxlits ~ord lits) <= 1 then CCBV.empty ()
  else (
    weight_based_sel_driver ~can_sel:can_select_lit ~ord lits (chooser) ~blocker
  )  

let can_sel_pos ~ord = fun _ _ -> true

let make_complete ~ord lits select_bv =
  assert(CCBV.length select_bv == CCArray.length lits);
  if not (CCBV.is_empty select_bv) then (
    (* selecting either no literals is OK *)

    let has_selectable_lit = ref false in
    CCArray.iteri (fun i _ -> 
      if not !has_selectable_lit && CCBV.get select_bv i 
         && can_select_lit ~ord lits i then (
        has_selectable_lit := true
      )
    ) lits;

    if not !has_selectable_lit then (
      (* we need to make sure that there is at least one selectable
         literal. after we have assured that -> anything extra is selectable
         (including positive literals) *)
      CCBV.clear select_bv;
    )
  );
  select_bv

let pos_e_sel1 ~blocker ~ord lits =
  let chooser (i,l) =
    (Lit.is_positivoid l,
     (match l with
     | Literal.Equation(lhs,rhs,_) -> 
       Ordering.compare ord lhs rhs == Comparison.Incomparable
     | _ -> true),
     Lit.ho_weight l)
  in
  let res = CCBV.create ~size:(CCArray.length lits) false in

  if CCBV.cardinal (Literals.maxlits ~ord lits) > 1 then(
    begin match find_min_lit ~blocker ~can_sel:can_sel_pos ~ord ~chooser lits with
    | Some (l,idx) ->
      if Lit.is_negativoid l then ( 
        CCBV.set res idx;
        CCArray.iteri (fun i lit -> 
          if Lit.is_positivoid lit then CCBV.set res i) lits
      ) else CCBV.negate_self res
    | None -> () end;
    make_complete ~ord lits res
  ) else CCBV.empty ()

let pos_e_sel2 ~blocker ~ord lits =
  let chooser (i,l) =
    ((if Lit.is_positivoid l then 1 else 0),
     (if Lit.is_pure_var l then i else max_int), (* select first pure var lit*)
     (if Lit.is_ground l then (-(Lit.ho_weight l)) else max_int),
     - (lit_sel_diff_w l))
  in

  if CCArray.length (CCArray.filter Lit.is_positivoid lits) <= 1 && (
    match Literals.maxlits_l ~ord lits with
    | [(l,_)] -> Lit.is_positivoid l
    | _ -> false) then (CCBV.empty ())
  else (
    let res = CCBV.create ~size:(CCArray.length lits) false in
    begin match find_min_lit ~blocker ~can_sel:can_sel_pos ~ord ~chooser lits with
    | Some (l,idx) when Lit.is_negativoid l ->
      if not (Lit.is_pure_var l) && not (Lit.is_ground l) then ( 
        CCBV.set res idx;
        CCArray.iteri (fun i lit -> 
          if Lit.is_positivoid lit then CCBV.set res i) lits
      ) else CCBV.negate_self res
    | _ -> () end;
    make_complete ~ord lits res
  )

let pos_e_sel3 ~blocker ~ord lits =
  let chooser (i,l) =
    ((if Lit.is_positivoid l then 1 else 0),
     (if Lit.is_pure_var l then i else max_int), (* select first pure var lit*)
     (if Lit.is_ground l then (-(Lit.ho_weight l)) else max_int),
     - (lit_sel_diff_w l))
  in

  if CCArray.length (CCArray.filter Lit.is_positivoid lits) <= 1 then (CCBV.empty ())
  else (
    let res = CCBV.create ~size:(CCArray.length lits) false in
    begin match find_min_lit ~blocker ~can_sel:can_sel_pos ~ord ~chooser lits with
    | Some (l,idx) when Lit.is_negativoid l ->
      if not (Lit.is_pure_var l) && not (Lit.is_ground l) then ( 
        CCBV.set res idx;
        CCArray.iteri (fun i lit -> 
          if Lit.is_positivoid lit then CCBV.set res i) lits
      ) else CCBV.negate_self res
    | _ -> () end;
    make_complete ~ord lits res
  )

let ho_sel ~blocker ~ord lits = 
  let chooser (i,l) = 
    let sign = (if Lit.is_positivoid l then 1 else 0) in
    let ground = if Lit.is_ground l then 1.0 else 1.5 in
    let has_formula = Iter.exists T.is_formula @@ Lit.Seq.terms l in
    let app_var_num = 
      Lit.Seq.terms l
      |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true)
      |> Iter.map (fun t -> if Term.is_app_var t then 1 else 0) 
      |> Iter.sum 
      |> float_of_int in
    let weight = float_of_int (Lit.weight l) in
    (sign, 
     (if has_formula then 1 else 0),
     int_of_float (weight *. ((1.2) ** app_var_num) /. ground),  0)  in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser

let ho_sel2 ~blocker ~ord lits =
  let app_var_pen l =
    match l with 
    | Lit.Equation(lhs,rhs,_) ->
      let num_app_var_sides = (if T.is_app_var lhs then 1 else 0) +
                              (if T.is_app_var rhs then 1 else 0) in
      if num_app_var_sides = 1 then 0
      else if num_app_var_sides = 2 then 1 
      else 2
    | _ -> max_int in

  let chooser (i,l) = 
    let sign = (if Lit.is_positivoid l then 1 else 0) in
    (sign, app_var_pen l, Lit.weight l, (if not (Lit.is_ground l) then 0 else 1))  in
  weight_based_sel_driver ~can_sel:can_select_lit ~blocker ~ord lits chooser

let ho_sel3 ~blocker ~ord lits =
  let lit_penalty = function
    | Lit.Equation(lhs,rhs, _) ->
      let var_headed t = Term.is_var (Term.head_term t) in
      let lhs,rhs = 
        if var_headed rhs then (rhs,lhs) else (lhs,rhs) in
      (* Prefer using the following criteria:
         (1) literal is pure var
         (2a) literal is of the form X = %x.t or X = formula
         (2b) literal is of the form X a b c = %x.t or X a b c = formula
         (3) literal is of the form X (a b c) = s where s has a HO subterm
         (4) literal is of the form X (a b c) = s where s has no var head
         (5) literal is of the form X a b c = Y a b c *)
      if var_headed lhs then (
        if Term.is_var lhs && Term.is_var rhs then 0
        else if Term.is_fun rhs || Term.is_formula rhs then (
          1 + (if Term.is_app_var lhs then 3 else 0)) 
        else if Term.is_true_or_false rhs then (
          if Term.is_var lhs then 1 else 10)
        else if Term.is_app_var lhs && Term.is_app_var rhs then 20
        else if Term.has_ho_subterm rhs then 5
        else 10)
      else 100
    | _ -> max_int in 
  let chooser (i,l) = 
    let sign = (if Lit.is_positivoid l then 1 else 0) in
    let lit_pen = lit_penalty l in
    (sign, lit_pen, Lit.weight l, (if (Lit.is_ground l) then 0 else 1))  in
  weight_based_sel_driver ~can_sel:can_select_lit ~ord ~blocker lits chooser

let ho_sel4 ~blocker ~ord =
  e_sel3 ~ord ~blocker:(fun i -> function 
    | Literal.Equation(lhs,rhs,_) as lit -> blocker i lit || T.is_app_var lhs || T.is_app_var rhs
    | _ -> false)

let ho_sel5 ~blocker ~ord =
  e_sel ~ord ~blocker:(fun i -> function 
    | Literal.Equation(lhs,rhs,_) as lit -> blocker i lit || T.is_app_var lhs || T.is_app_var rhs
    | lit -> blocker i lit)


let except_RR_horn (p:parametrized) ~strict ~ord lits =
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else p ~strict ~ord lits  (* delegate *)

(** {2 Global selection Functions} *)

let default = max_goal ~strict:true

let bool_blockable ~blocker = 
  [ "e-selection", (e_sel ~blocker, true);
    "e-selection2", (e_sel2 ~blocker, true);
    "e-selection3", (e_sel3 ~blocker, true);
    "e-selection4", (e_sel4 ~blocker, true);
    "e-selection5", (e_sel5 ~blocker, true);
    "e-selection6", (e_sel6 ~blocker, true);
    "e-selection7", (e_sel7 ~blocker, true);
    "e-selection8", (e_sel8 ~blocker, true);
    "e-selection9", (e_sel9 ~blocker, true);
    "e-selection10", (e_sel10 ~blocker, true);
    "e-selection11", (e_sel11 ~blocker, false);
    "e-selection12", (e_sel12 ~blocker, true);
    "e-selection13", (e_sel13 ~blocker, true);
    "e-selection14", (e_sel14 ~blocker, true);
    "e-selection15", (e_sel15 ~blocker, true);
    "e-selection16", (e_sel16 ~blocker, true);
    "e-selection17", (e_sel17 ~blocker, true);
    "e-selection18", (e_sel18 ~blocker, true); (*SelectCQArNXTEqFirst*)
    "e-selection19", (e_sel19 ~blocker, true); (*SelectMaxLComplexG*)
    "ho-selection",  (ho_sel ~blocker, true);
    "ho-selection2", (ho_sel2 ~blocker, true);
    "ho-selection3", (ho_sel3 ~blocker, true);
    "ho-selection4", (ho_sel4 ~blocker, true);
    "ho-selection5", (ho_sel5 ~blocker, true);
    "pos-e-selection1", (pos_e_sel1 ~blocker, false); (*PSelectUnlessUniqMaxSmallestOrientable*)
    "pos-e-selection2", (pos_e_sel2 ~blocker, false); (*PSelectComplexExceptUniqMaxPosHorn*)
    "pos-e-selection3", (pos_e_sel3 ~blocker, false); (*PSelectComplexExceptUniqMaxHorn*)
  ]

let l =
  let basics =
    [ "NoSelection", ((fun ~ord:_ -> no_select), true);
      "default", (default, true);
      "avoid_app_var", (avoid_app_var, true);
      "prefer_app_var", (prefer_app_var, true);
    ]
  and by_ord =
    CCList.flat_map
      (fun (name,(p,c)) ->
         [ name, ((fun ~ord -> p ~strict:true ~ord), c);
           name ^ "NS", ((fun ~ord -> p ~strict:false ~ord),c);
         ])
      [ "MaxGoal", (max_goal, true);
        "MaxGoalExceptRRHorn", (except_RR_horn max_goal, true);
      ]
  in
  let b (i:int) (l:Lit.t) = false in

  basics @ by_ord @ bool_blockable ~blocker:b

let from_string ~ord s =
  let from_list name l =
    try 
      let fun_, is_complete = (List.assoc name l) in
      fun_ ~ord, is_complete
    with Not_found ->
      failwith ("no such selection function: "^s)
  in
  match CCString.chop_prefix ~pre:"bb+" s with
  | Some name -> 
    let selectable_sub =
      Bool_selection.all_selectable_subterms ~ord ~pos_builder:Position.Build.empty in
    let bool_blocker i l =
      Lit.Seq.terms l
      |> Iter.flat_map selectable_sub
      |> (fun i -> not @@ Iter.is_empty i)
    in
    from_list name (bool_blockable ~blocker:bool_blocker)
  | _ -> from_list s l
  

let all () = 
  List.map fst l
  @ List.map (fun (s, _) -> "bb+"^s) (bool_blockable ~blocker:(fun _ _ -> false))

let ho_restriction_opt =
  let set_ n = _ho_restriction := n in
  let l = [
    "none", `None;
    "no-var-heading-max-term", `NoVarHeadingMaxTerm;
    "no-var-different-args", `NoVarDifferentArgs;
    "no-unapplied-var-occurring-applied", `NoUnappliedVarOccurringApplied;
    "no-ho-vars", `NoHigherOrderVariables;
    "no-max-vars-as-fo", `NoMaxVarInFoContext] in
  Arg.Symbol (List.map fst l, fun s -> set_ (List.assoc s l))

let () =
  let set_select s = Params.select := s in
  Params.add_opts
    [ "--select", Arg.Symbol (all(), set_select), " set literal selection function. Prefix selection function with \"bb+\" to block selecting literals that have selectable boolean subterms. ";
      "--restrict-fresh-skolem-selection", Arg.Bool ((:=) _restrict_fresh_sk_selection), " Disable selection of literals whose head is fresh Skolem symbol";
      "--ho-selection-restriction", ho_restriction_opt, " selection restrictions for lambda-free higher-order terms (none/no-var-heading-max-term/no-var-different-args/no-unapplied-var-occurring-applied/no-ho-vars)"
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _ho_restriction := `NoMaxVarInFoContext
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      _ho_restriction := `NoMaxVarInFoContext
    );
  Params.add_to_mode "ho-pragmatic" (fun () ->
      _ho_restriction := `NoMaxVarInFoContext
    );
  Params.add_to_modes 
    [ "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] 
    (fun () ->
       _ho_restriction := `NoHigherOrderVariables
    );
  Params.add_to_modes ["ho-comb-complete"] (fun () -> _ho_restriction := `NoMaxVarInFoContext);
