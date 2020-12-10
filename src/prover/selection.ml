
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

(* May we select this literal? *)
let can_select_lit ~ord (lits:Lits.t) (i:int) : bool =
  if Lit.is_neg lits.(i)
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
                |> CCArray.filter (fun (_,l) -> Lit.is_neg l) in
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

let weight_based_sel_driver ?(blocker=(fun _ _ -> false)) ~ord lits f =
  let min_lit = 
    CCArray.to_iter lits
    |> Iter.mapi (fun i l -> f (i,l), i)
    |> Iter.sort
    |> Iter.find_map (fun (_,i) ->
      let lit = lits.(i) in
      if can_select_lit ~ord lits i && not (blocker i lit) 
      then Some (lit,i)
      else None) in
  match min_lit with
  | None -> BV.empty ()
  | Some (_, idx) ->
    assert(can_select_lit ~ord lits idx);
    assert(not (blocker idx (CCArray.get lits idx)));
    let res = BV.empty () in
    BV.set res idx;
    res

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

let is_truly_equational = function 
    | Lit.Equation(l,r,sign) -> not (T.equal T.true_ r)
    | _ -> false

let get_pred_freq ~freq_tbl l =
  match l with
  | Lit.Equation(l,r,sign) when T.equal T.true_ r ->
    begin 
      match T.head l with
      | Some id -> ID.Map.get_or id freq_tbl ~default:0
      | None -> max_int
    end
  | _ -> max_int

let e_sel ?(blocker=(fun _ _ -> false)) ~ord lits = 
  let chooser ~freq_tbl (i,l) = 
    ((if Lit.is_pos l then 1 else 0),
     ((if Lits.is_max ~ord lits i then 0 else 100) +
      (if Lit.is_pure_var l then 0 else 10) +
      (if Lit.is_ground l then 0 else 1)),
     -(lit_sel_diff_w l),
     get_pred_freq ~freq_tbl l) in
  let freq_tbl = pred_freq ~ord lits in
  weight_based_sel_driver ~ord lits (chooser ~freq_tbl) ~blocker

let e_sel2 ~ord lits = 
  let symbols = Lits.symbols lits 
                |> ID.Set.to_iter 
                |> Iter.sort ~cmp:ID.compare 
                |> Iter.to_array in
  let blocker _ l = Lit.is_type_pred l || Lit.is_predicate_lit l in
  let chooser (idx ,l) = 
    let sign_val = if Lit.is_pos l then 1 else 0 in 
    let diff_val = -(lit_sel_diff_w l) in
    let prec = Ordering.precedence ord in
    match l with 
    | Equation(lhs,_,sign) when not @@ blocker idx l ->
      if T.is_var (T.head_term lhs) then (
        (sign_val, 0, 0, diff_val)
      ) else (
        let hd_is_cst = T.is_const (T.head_term lhs) in
        let prec_weight = 
          if not hd_is_cst then 0
          else Precedence.sel_prec_weight prec (T.head_exn lhs) in
        let alpha_rank = 
          if not hd_is_cst then max_int
          else (
            match CCArray.bsearch ~cmp:ID.compare (T.head_exn lhs) symbols with
            | `At idx -> idx
            | _       -> max_int
          ) in

        (sign_val, -prec_weight, alpha_rank, diff_val)
      )
    | _ -> (sign_val,max_int,max_int,diff_val) (* won't be chosen *) in
  weight_based_sel_driver ~ord ~blocker lits chooser

let e_sel3 ?(blocker=(fun _ _ -> false)) ~ord lits = 
  let chooser (i,l) = 
    let sign = (if Lit.is_pos l then 1 else 0) in
    if Lit.is_pure_var l then (
      (sign, 0, 0, 0)
    ) else if (Lit.is_ground l) then (
      (sign, 10, Lit.weight l, 0)
    ) else (
      (sign, 20, - (lit_sel_diff_w l) , 0)
    ) in
  weight_based_sel_driver ~ord ~blocker lits chooser 

let e_sel4 ~ord lits =
  let freq_tbl = pred_freq ~ord lits in
  let chooser (i,l) = 
    let lhs = match l with
      | Lit.Equation(lhs_t,_,_) -> lhs_t
      | _ -> T.true_ (* a term to fill in *) in
    let sign = if Lit.is_pos l then 1 else 0 in
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
  let blocker _ l = Lit.is_type_pred l in
  weight_based_sel_driver ~ord ~blocker lits chooser 

let e_sel5 ~ord lits =
  let chooser (i,l) =
    (if Lit.is_pos l then 1 else 0), 
    (if Lit.is_ground l then 0 else 1),
    (- (lit_sel_diff_w l)),
    0 in
  if CCArray.exists (fun l -> Lit.is_neg l && Lit.depth l <= 2) lits then (
    weight_based_sel_driver ~ord lits chooser 
  ) else BV.empty ()

let e_sel6 ~ord lits =
  (* SelectLargestOrientable *)
  let is_oriented lit = 
    match lit with
    | Lit.Equation(l,r,_) ->
      Ordering.compare ord l r != Comparison.Incomparable
    | _ -> true in
  let chooser (i,l) =
    (if Lit.is_pos l then 1 else 0),
    (if is_oriented l then 0 else 1),
    (- (Lit.weight l)),
    0 in
  let blocker _ t = not @@ is_oriented t in
  weight_based_sel_driver ~ord lits chooser ~blocker

let e_sel7 ~ord lits = 
  (* SelectComplexExceptRRHorn *)
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else e_sel3 ~ord lits

let e_sel8 ~ord lits = 
  let symbols = Lits.symbols lits 
                |> ID.Set.to_iter 
                |> Iter.sort ~cmp:ID.compare 
                |> Iter.to_array in
  let get_arity = function 
    | Lit.Equation(l,r,sign) when Term.equal T.true_ r -> 
      List.length (Type.expected_args (Term.ty (T.head_term l)))
    | _ -> 0  in
  let alpha_rank = function 
    | Lit.Equation(l,r,sign) when sign && Term.equal T.true_ r 
                                       && T.is_const (T.head_term l) -> 
      let hd = T.head_exn l in
      (match CCArray.bsearch ~cmp:ID.compare hd symbols with
       | `At idx -> idx
       | _       -> max_int)
    | _ -> max_int  in
  let blocker _ l = Lit.is_type_pred l || Lit.is_predicate_lit l in
  let chooser (i,l) =
    if is_truly_equational l then (
      (if Lit.is_pos l then 1 else 0),
      min_int, 0, lit_sel_diff_w l
    ) else (
      (if Lit.is_pos l then 1 else 0),
      (if not (blocker i l) then -(get_arity l) else max_int), 
      alpha_rank l, lit_sel_diff_w l
    )
  in
  weight_based_sel_driver ~ord lits chooser ~blocker

let e_sel9 ~ord lits =
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
  weight_based_sel_driver ~ord lits chooser

let e_sel10 ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
    let bv = Lits.neg lits in
    (* select all selectable negative lits *)
    BV.filter bv (fun i -> can_select_lit ~ord lits i);
    bv)

let e_sel11 ~ord lits =
  let freq_tbl = pred_freq ~ord lits in
  let blocker _ l = Lit.is_type_pred l in
  let eqn_max_weight = function 
  | Lit.Equation(lhs,rhs,false) ->
    if Ordering.compare ord lhs rhs  == Comparison.Gt then Term.ho_weight lhs 
    else Term.ho_weight lhs + Term.ho_weight rhs
  | _ -> max_int in
  let lhs_weight  = function 
  | Lit.Equation(lhs,rhs,false) ->
    if Ordering.compare ord lhs rhs  == Comparison.Gt then Term.ho_weight lhs 
    else Term.ho_weight lhs
  | _ -> max_int in

  let chooser (i,l) =
    if Lit.is_pos l then (max_int,max_int,max_int)
    else if Lit.is_ground l then (0, lhs_weight l, get_pred_freq ~freq_tbl l)
    else if not (Lit.is_typex_pred l) then (10, eqn_max_weight l, get_pred_freq ~freq_tbl l)
    else if not (Lit.is_type_pred l) then (20, - (lhs_weight l), get_pred_freq ~freq_tbl l)
    else (max_int, max_int, max_int) in
  
  if CCArray.exists (fun l -> Lit.is_neg l && Lit.depth l <= 2) lits then (
    weight_based_sel_driver ~ord lits chooser ~blocker
  ) else BV.empty ()

let e_sel12 ~ord lits =
  if Lits.is_unique_max_horn_clause ~ord lits
  then BV.empty () 
  else e_sel3 ~ord lits

let e_sel13 ~ord lits =
   let chooser (i,l) =
    (if Lit.is_pos l then 1 else 0), 
    (if Lit.is_ground l then 0 else 1),
    (- (lit_sel_diff_w l)),
    0 in
  weight_based_sel_driver ~ord lits chooser

let e_sel14 ~ord lits =
  let freq_tbl = pred_freq ~ord lits in

  let blocked = CCBV.create ~size:(CCArray.length lits) false in

  let chooser (i,l) =
    let hd_freq = get_pred_freq ~freq_tbl l in
    let hd_is_fresh_pred = function
      | Lit.Equation(lhs, rhs, _) when T.is_true_or_false rhs -> 
        begin match T.as_const (T.head_term lhs) with
        | Some c -> ID.is_postcnf_skolem c
        | None -> false end
      | _ -> false in
    let max_val = (max_int, max_int, max_int) in

    let block b i = 
      CCBV.set b i;
      max_val in
    
    if Lit.is_pos l ||  hd_is_fresh_pred l then (block blocked i)
    else if Lit.is_ground l then (0, Lit.weight l, hd_freq)
    else if not (Lit.is_typex_pred l) then (10, Lit.max_term_positions ~ord l, hd_freq)
    else if not (Lit.is_type_pred l) then (20, - (Lit.ho_weight l), hd_freq)
    else (block blocked i) in
  
  let blocker blocked i l = CCBV.get blocked i in

  weight_based_sel_driver ~ord lits chooser ~blocker:(blocker blocked)

let e_sel15 ~ord lits =
  let (<+>) = CCOpt.Infix.(<+>)  in
  let lits_l = CCList.mapi (fun i l -> (i,l)) (CCArray.to_list lits) in
  let sel_bv = CCBV.create ~size:(CCArray.length lits) false in
  let res = 
    (* find first negative pure var lit *)
    (CCList.find_map (fun (i, l) ->
      if Lit.is_neg l && Lit.is_pure_var l then (
        CCBV.set sel_bv i;
        Some sel_bv
      ) else None
    ) lits_l)
  <+>
    (* else find smallest negative ground lit *)
    (lits_l
     |> CCList.filter_map (fun (i, l) -> 
          if Lit.is_neg l && Lit.is_ground l then (
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
        Lit.is_pos lits.(CCBV.first_exn max_lits) then (
        assert(CCBV.is_empty sel_bv);
        Some sel_bv
     ) else None)
  <+>
    (* else behave as SelectNewComplexAHPNS  *)
    Some (e_sel14 ~ord lits) in
  CCOpt.get_exn res

let e_sel16 ~ord lits =
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
  weight_based_sel_driver ~ord lits chooser ~blocker:(fun i _ -> CCBV.get blocked i)

let ho_sel ~ord lits = 
  let chooser (i,l) = 
    let sign = (if Lit.is_pos l then 1 else 0) in
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
  weight_based_sel_driver ~ord lits chooser

let ho_sel2 ~ord lits =
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
    let sign = (if Lit.is_pos l then 1 else 0) in
    (sign, app_var_pen l, Lit.weight l, (if not (Lit.is_ground l) then 0 else 1))  in
  weight_based_sel_driver ~ord lits chooser

let ho_sel3 ~ord lits =
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
    let sign = (if Lit.is_pos l then 1 else 0) in
    let lit_pen = lit_penalty l in
    (sign, lit_pen, Lit.weight l, (if (Lit.is_ground l) then 0 else 1))  in
  weight_based_sel_driver ~ord lits chooser

let ho_sel4 ~ord =
  e_sel3 ~ord ~blocker:(fun _ -> function 
    | Literal.Equation(lhs,rhs,_) -> T.is_app_var lhs || T.is_app_var rhs
    | _ -> false)

let ho_sel5 ~ord =
  e_sel ~ord ~blocker:(fun _ -> function 
    | Literal.Equation(lhs,rhs,_) -> T.is_app_var lhs || T.is_app_var rhs
    | _ -> false)


let except_RR_horn (p:parametrized) ~strict ~ord lits =
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else p ~strict ~ord lits  (* delegate *)

(** {2 Global selection Functions} *)

let default = max_goal ~strict:true

let l =
  let basics =
    [ "NoSelection", (fun ~ord:_ -> no_select);
      "default", default;
      "avoid_app_var", avoid_app_var;
      "prefer_app_var", prefer_app_var;
      "e-selection", e_sel ~blocker:(fun _ _ -> false);
      "e-selection2", e_sel2;
      "e-selection3", e_sel3 ~blocker:(fun _ _ -> false);
      "e-selection4", e_sel4;
      "e-selection5", e_sel5;
      "e-selection6", e_sel6;
      "e-selection7", e_sel7;
      "e-selection8", e_sel8;
      "e-selection9", e_sel9;
      "e-selection10", e_sel10;
      "e-selection11", e_sel11;
      "e-selection12", e_sel12;
      "e-selection13", e_sel13;
      "e-selection14", e_sel14;
      "e-selection15", e_sel15;
      "e-selection16", e_sel16;
      "ho-selection", ho_sel;
      "ho-selection2", ho_sel2;
      "ho-selection3", ho_sel3;
      "ho-selection4", ho_sel4;
      "ho-selection5", ho_sel5;
    ]
  and by_ord =
    CCList.flat_map
      (fun (name,p) ->
         [ name, (fun ~ord -> p ~strict:true ~ord);
           name ^ "NS", (fun ~ord -> p ~strict:false ~ord);
         ])
      [ "MaxGoal", max_goal;
        "MaxGoalExceptRRHorn", except_RR_horn max_goal;
      ]
  in
  basics @ by_ord

let from_string ~ord s =
  try (List.assoc s l) ~ord
  with Not_found ->
    failwith ("no such selection function: "^s)

let all () = List.map fst l

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
    [ "--select", Arg.Symbol (all(), set_select), " set literal selection function";
      "--ho-selection-restriction", ho_restriction_opt, " selection restrictions for lambda-free higher-order terms (none/no-var-heading-max-term/no-var-different-args/no-unapplied-var-occurring-applied/no-ho-vars)"
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _ho_restriction := `NoVarHeadingMaxTerm
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      _ho_restriction := `NoVarHeadingMaxTerm
    );
  Params.add_to_mode "ho-pragmatic" (fun () ->
      _ho_restriction := `NoVarHeadingMaxTerm
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
