
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
        )
    in
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
          vars_args |> CCList.exists (fun (head, args) -> head = t && not (CCList.is_empty args))
        )
    | `NoHigherOrderVariables ->
      (* We cannot select literals containing a HO variable: *)
      not (
        Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
        |> Iter.exists (fun (t,_) -> T.is_ho_var (fst (T.as_app t)))
      )
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

let weight_based_sel_driver ?(blocker=(fun _ -> false)) ~ord lits f =
  let min_lit = 
    CCArray.to_seq lits
    |> Iter.mapi (fun i el -> f (i,el), i) 
    |> Iter.min in
  match min_lit with
  | None -> BV.empty ()
  | Some (_, idx) -> 
    if can_select_lit ~ord lits idx && not @@ blocker (CCArray.get lits idx) then (
      let res = BV.empty () in
      (* CCFormat.printf "Selected %d: %a.\n" idx Lits.pp lits; *)
      BV.set res idx;
      res
    ) else BV.empty ()

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

let get_pred_freq ~freq_tbl l =
  match l with
  | Lit.Equation(l,r,true) when T.is_true_or_false r ->
    begin 
      match T.head l with
      | Some id -> ID.Map.get_or id freq_tbl ~default:0
      | None -> max_int
    end
  | _ -> max_int

let e_sel ~ord lits = 
  let chooser ~freq_tbl (i,l) = 
    ((if Lit.is_pos l then 1 else 0),
     (if Lits.is_max ~ord lits i then 0 else 100 +
                                             if Lit.is_pure_var l then 0 else 10 +
                                                                              if Lit.is_ground l then 0 else 1),
     -(lit_sel_diff_w l),
     get_pred_freq ~freq_tbl l) in
  let freq_tbl = pred_freq ~ord lits in
  weight_based_sel_driver ~ord lits (chooser ~freq_tbl)

let e_sel2 ~ord lits = 
  let symbols = Lits.symbols lits 
                |> ID.Set.to_seq 
                |> Iter.sort ~cmp:ID.compare 
                |> Iter.to_array in
  let blocker l = Lit.is_type_pred l || Lit.is_propositional l in
  let chooser (_,l) = 
    let sign_val = if Lit.is_pos l then 1 else 0 in 
    let diff_val = -(lit_sel_diff_w l) in
    let prec = Ordering.precedence ord in
    match l with 
    | Equation(lhs,rhs,sign) when not @@ blocker l ->
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

let e_sel3 ~ord lits = 
  let chooser (i,l) = 
    let sign = (if Lit.is_pos l then 1 else 0) in
    if Lit.is_pure_var l then (
      (sign, 0, 0, 0)
    ) else if (Lit.is_ground l) then (
      (sign, 10, Lit.weight l, 0)
    ) else (
      (sign, 20, - (lit_sel_diff_w l) , 0)
    ) in
  weight_based_sel_driver ~ord lits chooser

let e_sel4 ~ord lits =
  let chooser (i,l) = 
    let lhs = match l with
      | Lit.Equation(lhs_t,rhs_t,_) -> lhs_t
      | _ -> T.true_ (* a term to fill in *) in
    let sign = if Lit.is_pos l then 1 else 0 in
    let freq_tbl = pred_freq ~ord lits in
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
  let blocker = Lit.is_type_pred in
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
  let blocker t = not @@ is_oriented t in
  weight_based_sel_driver ~ord lits chooser ~blocker

let e_sel7 ~ord lits = 
  (* SelectComplexExceptRRHorn *)
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else e_sel3 ~ord lits

let e_sel8 ~ord lits = 
  let symbols = Lits.symbols lits 
                |> ID.Set.to_seq 
                |> Iter.sort ~cmp:ID.compare 
                |> Iter.to_array in
  let is_truly_equational = function 
    | Lit.Equation(l,r,sign) -> 
      not (Term.is_true_or_false r)
    | _ -> false  in
  let get_arity = function 
    | Lit.Equation(l,r,sign) when sign && Term.is_true_or_false r -> 
      List.length (Type.expected_args (Term.ty (T.head_term l)))
    | _ -> 0  in
  let alpha_rank = function 
    | Lit.Equation(l,r,sign) when sign && Term.is_true_or_false r 
                                  && T.is_const (T.head_term l) -> 
      let hd = T.head_exn l in
      (match CCArray.bsearch ~cmp:ID.compare hd symbols with
       | `At idx -> idx
       | _       -> max_int)
    | _ -> max_int  in
  let blocker l = Lit.is_type_pred l || Lit.is_propositional l in
  let chooser (i,l) =
    if is_truly_equational l then (
      (if Lit.is_pos l then 1 else 0),
      min_int, 0, lit_sel_diff_w l
    ) else (
      (if Lit.is_pos l then 1 else 0),
      (if not (blocker l) then -(get_arity l) else max_int), 
      alpha_rank l, lit_sel_diff_w l
    )
  in
  weight_based_sel_driver ~ord lits chooser ~blocker

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
      let num_app_var_sides = (if T.is_var @@ T.head_term lhs then 1 else 0) +
                              (if T.is_var @@ T.head_term rhs then 1 else 0) in
      if num_app_var_sides = 1 then 0
      else if num_app_var_sides = 2 then 1 
      else 2
    | _ -> max_int in

  let chooser (i,l) = 
    let sign = (if Lit.is_pos l then 1 else 0) in
    (sign, app_var_pen l, Lit.weight l, (if not (Lit.is_ground l) then 0 else 1))  in
  weight_based_sel_driver ~ord lits chooser

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
      "e-selection", e_sel;
      "e-selection2", e_sel2;
      "e-selection3", e_sel3;
      "e-selection4", e_sel4;
      "e-selection5", e_sel5;
      "e-selection6", e_sel6;
      "e-selection7", e_sel7;
      "e-selection8", e_sel8;
      "ho-selection", ho_sel;
      "ho-selection2", ho_sel2;
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
    "no-ho-vars", `NoHigherOrderVariables] in
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
