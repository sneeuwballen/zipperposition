(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

open Logtk

module O = Ordering
module Lit = Literal
module Lits = Literals

module type S = ClauseQueue_intf.S

type profile = ClauseQueue_intf.profile

let cr_var_ratio = ref 5
let cr_var_mul   = ref 1.1
let parameters_magnitude = ref `Large
let goal_penalty = ref false

let profiles_ =
  let open ClauseQueue_intf in
  [ "default", P_default
  ; "bfs", P_bfs
  ; "almost-bfs", P_almost_bfs
  ; "explore", P_explore
  ; "ground", P_ground
  ; "goal", P_goal
  ; "conjecture-relative", P_conj_rel
  ; "conjecture-relative-var", P_conj_rel_var
  ; "ho-weight", P_ho_weight
  ; "ho-weight-init", P_ho_weight_init
  ; "avoid-expensive", P_avoid_expensive
  ]

let profile_of_string s =
  let s = s |> String.trim |> CCString.lowercase_ascii in
  try
    match CCString.chop_prefix ~pre:"conjecture-relative-var" s with
    | Some suffix -> 
      let err_msg = "conjecutre-relative-var(ratio:int,var_mul:float,par:[L,S],goal_penalty:[true,false])" in
      let args = List.map (fun s -> 
          String.trim (CCString.replace ~sub:")" ~by:"" 
                         (CCString.replace ~sub:"(" ~by:"" s))) 
          (CCString.split ~by:"," suffix) in
      if List.length args != 4 then (invalid_arg err_msg)
      else (
        let ratio = CCInt.of_string (List.nth args 0) in
        let var_mul =
          try float_of_string (List.nth args 1)
          with _ -> invalid_arg err_msg
        in
        let par_mag = List.nth args 2 in
        let goal_pen = List.nth args 3 in
        if CCOpt.is_none ratio then (
          invalid_arg err_msg;
        ) else (
          cr_var_ratio := CCOpt.get_exn ratio;
          cr_var_mul := var_mul;
          parameters_magnitude := if CCString.equal par_mag "l" then `Large
            else if CCString.equal par_mag "s" then `Small
            else invalid_arg err_msg;
          goal_penalty         := if CCString.prefix ~pre:"t" goal_pen then true
            else if CCString.prefix ~pre:"f" goal_pen then false
            else invalid_arg err_msg;
          ClauseQueue_intf.P_conj_rel_var
        )
      )
    | None ->  List.assoc s profiles_
  with Not_found -> invalid_arg ("unknown queue profile: " ^ s)

let _profile = ref ClauseQueue_intf.P_default
let get_profile () = !_profile
let set_profile p = _profile := p
let parse_profile s = _profile := (profile_of_string s)
let funs_to_parse = ref [] 


module Make(C : Clause_intf.S) = struct
  module C = C

  (* weight of a term [t], using the precedence's weight *)
  let term_weight t = Term.size t

  (** {6 Weight functions} *)
  module WeightFun = struct
    type t = C.t -> int

    let weight_lits_ l =
      Array.fold_left
        (fun acc lit -> acc + Lit.heuristic_weight term_weight lit)
        0 l

    let default c =
      (* maximum depth of types. Avoids reasoning on list (list (list .... (list int))) *)
      let _depth_ty =
        Lits.Seq.terms (C.lits c)
        |> Iter.map Term.ty
        |> Iter.map Type.depth
        |> Iter.max ?lt:None
        |> CCOpt.map_or CCFun.id ~default:0
      in
      let w_lits = weight_lits_ (C.lits c) in
      w_lits * Array.length (C.lits c) + _depth_ty

    let  ho_weight_calc c = 
      let all_terms c =
        C.Seq.terms c 
        |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true) in

      let app_var_num c = 
        float_of_int @@ 
        Iter.fold (fun acc t -> 
            acc + (if Term.is_app_var t then 1 else 0 )) 0 (all_terms c) in

      let formulas_num c =
        float_of_int @@
        Iter.fold (fun acc t -> 
            acc + (if Term.is_formula t then 1 else 0 )) 0 (C.Seq.terms c) in

      let p_depth c = float_of_int (C.proof_depth c) in

      let t_depth c = C.Seq.terms c 
                      |> Iter.map Term.depth
                      |> Iter.max
                      |> CCOpt.get_or ~default:0
                      |> float_of_int in

      let weight c = float_of_int (weight_lits_ (C.lits c)) in

      let res = 
        int_of_float (weight c *. (1.25 ** (app_var_num c *. (1.35 ** p_depth c)))
                      *. (0.85 ** formulas_num c)
                      *. 1.05 ** t_depth c) in
      Util.debugf 5 "[C_W:]@ @[%a@]@ :@ %d(%g, %g, %g).\n"
        (fun k -> k C.pp c res (app_var_num c) (formulas_num c) (p_depth c));
      res

    let avoid_expensive c =
      let max_lits = C.maxlits (c,0) Subst.empty in
      let signature = C.Ctx.signature () in
      CCArray.foldi (fun acc i l -> 
          let max_terms = 
            if CCBV.get max_lits i then Literal.Comp.max_terms ~ord:(C.Ctx.ord ()) l else [] in
          let lit_weight = 
            Literal.Seq.terms l
            |> Iter.map (fun t -> 
                let var,f_nc,f_c = if List.mem t max_terms then (2,6,3) else (1,2,1) in
                let sym id_ = if Signature.sym_in_conj id_ signature then f_c else f_nc in
                Term.weight ~var ~sym t * (if Term.is_app_var t then 2 else 1)
              ) |> Iter.sum in
          let multiplier = (if Literal.is_typex_pred l then 1.2 else 1.0) *.
                           (if Literal.is_type_pred l then 1.8 else 1.0) *.
                           (if Literal.is_ground l then 0.5 else 1.0) in
          int_of_float (multiplier *. (float_of_int lit_weight)) + acc
        ) 0 (C.lits c)

    let orient_lmax_weight ~v_w ~f_w ~pos_m ~unord_m ~max_l_mul c =
      let max_lits = C.maxlits (c,0) Subst.empty in
      let ord = C.Ctx.ord () in
      let res = CCArray.foldi (fun sum i lit -> 
          let term_w = (fun t -> float_of_int (Term.weight ~var:v_w ~sym:(fun _ -> f_w) t)) in
          let w = 
            match lit with 
            | Lit.Equation(l,r,_) ->
              let t_w = max (term_w l) (term_w r) in
              let t_w = if Lit.is_pos lit then pos_m *. t_w else t_w in
              let t_w = if CCBV.get max_lits i then max_l_mul *. t_w else t_w in
              let ordered = Ordering.compare ord l r != Comparison.Incomparable in
              t_w *. (if not ordered then unord_m else 1.0)
            | _ -> 1.0 in
          sum +. w
        ) 0.0 (C.lits c) in
      int_of_float res

    let pn_refined_weight ~pv_w ~pf_w ~nv_w ~nf_w ~max_t_m ~max_l_m ~pos_m c =
      let max_lits = C.maxlits (c,0) Subst.empty in
      let ord = C.Ctx.ord () in
      let res = CCArray.foldi (fun sum i lit -> 
          let pterm_w = (fun t -> float_of_int (Term.weight ~var:pv_w ~sym:(fun _ -> pf_w) t)) in
          let nterm_w = (fun t -> float_of_int (Term.weight ~var:nv_w ~sym:(fun _ -> nf_w) t)) in
          let w = 
            match lit with 
            | Lit.Equation(l,r,_) ->
              let term_w = if Lit.is_pos lit then pterm_w else nterm_w in
              let ord_side = Ordering.compare ord l r in
              let l_mul = if ord_side = Comparison.Gt || ord_side = Comparison.Incomparable 
                then max_t_m else 1.0 in
              let r_mul = if ord_side = Comparison.Lt || ord_side = Comparison.Incomparable 
                then max_t_m else 1.0 in
              let t_w = l_mul *. (term_w l) +. r_mul *. (term_w r) in
              let t_w = if Lit.is_pos lit then pos_m *. t_w else t_w in
              let t_w = if CCBV.get max_lits i then max_l_m *. t_w else t_w in
              t_w 
            | _ -> 1.0 in
          sum +. w
        ) 0.0 (C.lits c) in
      int_of_float res

    let ho_weight_initial c =
      if C.proof_depth c  = 0 then 1
      else ho_weight_calc c

    let rec calc_tweight t sg v w c_mul =
      match Term.view t with 
        Term.AppBuiltin (_,l) -> 
        w + List.fold_left (fun acc t -> acc + 
                                         calc_tweight t sg v w c_mul) 0 l
      | Term.Var _ -> v
      | Term.DB _ -> w
      | Term.App (f, l) ->
        let v = if Term.is_var f then 2*v else v in
        calc_tweight f sg v w c_mul +
        List.fold_left (fun acc t -> acc + calc_tweight t sg v w c_mul) 0 l
      | Term.Const id -> (int_of_float ((if Signature.sym_in_conj id sg then c_mul else 1.0)*.float_of_int w))
      | Term.Fun (_, t) -> calc_tweight t sg v w c_mul

    let calc_lweight l sg v w c_mul =
      assert (Literal.no_prop_invariant l);
      match l with 
      (* Special treatment of propositions *)
      | Lit.Equation (lhs,rhs,true) when Term.equal rhs Term.true_ 
                                      || Term.equal rhs Term.false_ ->
        calc_tweight lhs sg v w c_mul, Term.equal rhs Term.true_
      | Lit.Equation (lhs,rhs,sign) -> (calc_tweight lhs sg v w c_mul + 
                                        calc_tweight rhs sg v w c_mul, sign)
      | _ -> (0,false)

    let conj_relative_cheap ~v ~f ~pos_mul ~conj_mul ~dist_var_mul c  =
      let sgn = C.Ctx.signature () in
      let res = 
        Array.mapi (fun i xx -> i,xx) (C.lits c)
        |> (Array.fold_left (fun acc (i,l) -> acc +. 
                                              let l_w, l_s = (calc_lweight l sgn v f conj_mul) in 
                                              ( if l_s then pos_mul else 1.0 )*. float_of_int l_w ) 0.0) in
      let dist_vars =  List.length (Literals.vars (C.lits c)) in
      int_of_float (dist_var_mul ** (float_of_int dist_vars) *. res)

    let conj_relative ?(distinct_vars_mul=(-1.0)) ?(parameters_magnitude=`Large) ?(goal_penalty=false) c =
      let sgn = C.Ctx.signature () in
      let max_lits = C.maxlits (c,0) Subst.empty in
      let pos_mul, max_mul, v,f =
        match parameters_magnitude with 
        |`Large -> (1.5,1.5,100,100)
        |`Small -> (2.0,1.5,2,3)
      in
      let conj_mul = 0.5 in
      Array.mapi (fun i xx -> i,xx) (C.lits c)
      |> 
      (Array.fold_left (fun acc (i,l) -> acc +. 
                                         let l_w, l_s = (calc_lweight l sgn v f conj_mul) in 
                                         ( if l_s then pos_mul else 1.0 )*.
                                         ( if CCBV.get max_lits i then max_mul else 1.0)*. 
                                         float_of_int l_w ) 0.0) 
      |> (fun res -> 
          if distinct_vars_mul < 0.0 then int_of_float res
          else
            let dist_vars = 
              Literals.vars (C.lits c)
              |> List.filter (fun v -> not (Type.is_tType (HVar.ty v)))  in
            let n_vars = List.length dist_vars + 1  in
            let dist_var_penalty = distinct_vars_mul ** (float_of_int n_vars) in
            let goal_dist_penalty = 
              if goal_penalty then (
                let divider = 
                  match C.distance_to_goal c with
                  | Some d -> 1.5 ** (1.0 /. (1.0 +. (float_of_int @@ d)))
                  | None -> 1.0 in
                1.0 /. divider
              ) else 1.0 in
            let val_ = int_of_float (goal_dist_penalty *. dist_var_penalty *. res) in
            (Util.debugf  10 "cl: %a, w:%d\n" (fun k -> k C.pp c val_);
             val_))


    let penalty = C.penalty

    let penalize w c = assert (penalty c >= 1); w c * penalty c

    let penalty_coeff_ = 20

    let favor_pos_unit c =
      let is_unit_pos c = match C.lits c with
        | [| lit |] when Lit.is_pos lit -> true
        | _ -> false
      in
      if is_unit_pos c then 0 else penalty_coeff_

    (* favorize small number of variables in a clause *)
    let favor_small_num_vars c =
      (* number of distinct term variables *)
      let n_vars =
        Literals.vars (C.lits c)
        |> List.filter (fun v -> not (Type.is_tType (HVar.ty v)))
        |> List.length
      in
      let n =
        if n_vars < 4 then 0
        else if n_vars < 6 then 1
        else if n_vars < 8 then 3
        else n_vars
      in
      n * penalty_coeff_

    let favor_horn c =
      if Lits.is_horn (C.lits c) then 0 else penalty_coeff_

    let goal_threshold_ = 15

    let favor_goal c =
      if C.is_empty c then 0
      else
        let d = match C.distance_to_goal c with
          | Some d -> min d goal_threshold_
          | None -> goal_threshold_
        in
        1+d

    (* check whether a literal is a ground neg lit *)
    let is_ground_neg lit = Lit.is_neg lit && Lit.is_ground lit
    let all_ground_neg c = CCArray.for_all is_ground_neg (C.lits c)

    let favor_all_neg c =
      if all_ground_neg c then 0 else penalty_coeff_

    let favor_ground c = if C.is_ground c then 0 else penalty_coeff_

    let favor_non_all_neg c =
      if not (all_ground_neg c) then 0 else penalty_coeff_

    let combine ws =
      assert (ws <> []);
      assert (List.for_all (fun (_,c) -> c > 0) ws);
      fun c ->
        List.fold_left
          (fun sum (w,coeff) -> sum + coeff * w c)
          0 ws

    let explore_fun = 
      penalize (
        combine
          [default, 4; favor_small_num_vars, 1;
           favor_all_neg, 1 ]
      )

    let default_fun =
      penalize (
        combine
          [ default, 3; favor_all_neg, 1; favor_small_num_vars, 2
          ; favor_goal, 1; favor_pos_unit, 1; ]
      )

    let parse_crv s = 
      let crv_regex = Str.regexp "conjecture-relative-var(\\([0-9]+[.]?[0-9]*\\),\\([lsLS]\\),\\([tfTF]\\))" in
      try
        ignore(Str.search_forward crv_regex s 0);
        let distinct_vars_mul = CCFloat.of_string (Str.matched_group 1 s) in
        let parameters_magnitude = 
          Str.matched_group 2 s |> CCString.trim |> String.lowercase_ascii
          |> (fun s -> if CCString.prefix ~pre:"l" s then `Large else `Small) in
        let goal_penalty = 
          Str.matched_group 3 s |> CCString.trim |> String.lowercase_ascii
          |> CCString.prefix ~pre:"t" in
        conj_relative ~distinct_vars_mul ~parameters_magnitude ~goal_penalty
      with Not_found | Invalid_argument _ -> 
        invalid_arg 
          "expected conjecture-relative-var(dist_var_mul:float,parameters_magnitude:l/s,goal_penalty:t/f)"

    let parse_orient_lmax s = 
      let or_lmax_regex = 
        Str.regexp 
          ("orient-lmax(\\([0-9]+[.]?[0-9]*\\)," 
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\))") in
      try
        ignore(Str.search_forward or_lmax_regex s 0);
        let v_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 1 s)) in
        let f_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 2 s)) in
        let pos_m = CCFloat.of_string (Str.matched_group 3 s) in
        let unord_m = CCFloat.of_string (Str.matched_group 4 s) in
        let max_l_mul = CCFloat.of_string (Str.matched_group 5 s) in

        orient_lmax_weight ~v_w ~f_w ~pos_m ~unord_m ~max_l_mul
      with Not_found | Invalid_argument _ -> 
        invalid_arg @@
        "expected orient-lmax(var_weight:int,fun_weight:int" ^
        "pos_lit_mult:float, unorderable_lit_mul:float, max_lit_mul:float)"

    let parse_pnrefine s = 
      let or_lmax_regex = 
        Str.regexp 
          ("pnrefined(\\([0-9]+[.]?[0-9]*\\)," 
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\))") in
      try
        ignore(Str.search_forward or_lmax_regex s 0);
        let pv_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 1 s)) in
        let pf_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 2 s)) in
        let nv_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 2 s)) in
        let nf_w = CCOpt.get_exn (CCInt.of_string (Str.matched_group 2 s)) in
        let pos_m = CCFloat.of_string (Str.matched_group 3 s) in
        let max_t_m = CCFloat.of_string (Str.matched_group 4 s) in
        let max_l_m = CCFloat.of_string (Str.matched_group 5 s) in
        pn_refined_weight ~pv_w ~pf_w ~nv_w ~nf_w ~pos_m ~max_t_m ~max_l_m
      with Not_found | Invalid_argument _ -> 
        invalid_arg @@
        "expected pnrefined(+var_weight:int,+fun_weight:int" ^
        "-var_weight:int,-fun_weight:int" ^
        "pos_lit_mult:float, max_t_mult:float, max_lit_mul:float)"

    let parse_conj_relative_cheap s = 
      let or_lmax_regex = 
        Str.regexp 
          ("conjecture-relative-cheap(\\([0-9]+\\)," 
           ^ "\\([0-9]+\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\),"
           ^ "\\([0-9]+[.]?[0-9]*\\))") in
      try
        ignore(Str.search_forward or_lmax_regex s 0);
        let v = CCOpt.get_exn (CCInt.of_string (Str.matched_group 1 s)) in
        let f = CCOpt.get_exn (CCInt.of_string (Str.matched_group 2 s)) in
        let pos_mul = CCFloat.of_string (Str.matched_group 3 s) in
        let conj_mul = CCFloat.of_string (Str.matched_group 4 s) in
        let dist_var_mul = CCFloat.of_string (Str.matched_group 5 s) in
        conj_relative_cheap ~v ~f ~pos_mul ~conj_mul ~dist_var_mul
      with Not_found | Invalid_argument _ ->
        Util.invalid_argf
          "expected conjecture-relative-cheap(v:int,f:int,pos_mul:float,conj_mul:float,dist_var_mul:float\n\
           got: %s" s

    let parsers = 
      ["fifo", (fun _ c -> C.id c);
       "default", (fun _ -> default_fun);
       "explore",  (fun _ -> explore_fun);
       "conjecture-relative", (fun _ -> conj_relative ~distinct_vars_mul:1.0 
                                  ~parameters_magnitude:`Large 
                                  ~goal_penalty:false );
       "conjecture-relative-var", parse_crv;
       "conjecture-relative-cheap", parse_conj_relative_cheap;
       "pnrefined", parse_pnrefine;
       "orient-lmax", parse_orient_lmax]

    let of_string s =
      try
        let splitted = CCString.split ~by:"(" s in
        let name = List.hd splitted in
        List.assoc name parsers s
      with Not_found | Failure _ -> 
        invalid_arg (CCFormat.sprintf "unknown weight function %s" s)

  end

  module PriorityFun = struct
    type t = C.t -> int

    let const_prio c = 1

    let prefer_ho_steps c = if Proof.Step.has_ho_step (C.proof_step c) then 0 else 1

    let prefer_sos c = 
      if C.proof_depth c = 0 || CCOpt.is_some (C.distance_to_goal c) then 0 else 1

    let prefer_non_goals c = 
      if Iter.exists Literal.is_pos (C.Seq.lits c) then 0 else 1

    let prefer_unit_ground_non_goals c = 
      if Iter.exists Literal.is_pos (C.Seq.lits c) &&
         C.is_unit_clause c && C.is_ground c then 0 else 1

    let prefer_goals c = 
      - (prefer_non_goals c)

    let prefer_processed c =
      if C.is_backward_simplified c then 0 else 1

    let prefer_lambdas c = 
      if (C.Seq.terms c |> Iter.exists (fun t -> Iter.exists Term.is_fun (Term.Seq.subterms t)))
      then 0 else 1

    let defer_lambdas c =
      - (prefer_lambdas c)

    let prefer_formulas c =
      if (C.Seq.terms c |> Iter.exists (fun t -> Iter.exists Term.is_formula (Term.Seq.subterms t)))
      then 0 else 1

    let prefer_easy_ho c =
      let has_lam_eq c =
        C.Seq.lits c
        |> Iter.exists (fun l -> 
            match l with
            | Literal.Equation(lhs,_,_) -> Type.is_fun (Term.ty lhs)
            | _ -> false) in
      if has_lam_eq c || prefer_formulas c = 1 then 0 
      else if prefer_lambdas c = 1 then 1
      else 2

    let defer_formulas c =
      - (prefer_formulas c)

    let prefer_fo c = 
      if Iter.for_all Term.is_fo_term (C.Seq.terms c) then 0 else 1

    let defer_fo c = 
      if Iter.for_all Term.is_fo_term (C.Seq.terms c) then 0 else 1

    let prefer_ground c =
      if C.is_ground c then 0 else 1

    let defer_ground c =
      if C.is_ground c then 1 else 0

    let defer_sos c = 
      if C.proof_depth c = 0 || CCOpt.is_some (C.distance_to_goal c) then 1 else 0

    let parsers = 
      ["const", (fun _ -> const_prio);
       "prefer-ho-steps", (fun _ -> prefer_ho_steps);
       "prefer-sos", (fun _ -> prefer_sos);
       "defer-sos", (fun _ -> defer_sos);
       "prefer-goals", (fun _ -> prefer_goals);
       "prefer-non-goals", (fun _ -> prefer_non_goals);
       "prefer-unit-ground-non-goals", (fun _ -> prefer_unit_ground_non_goals);            
       "prefer-processed", (fun _ -> prefer_processed);
       "prefer-lambdas", (fun _ -> prefer_lambdas);
       "defer-lambdas", (fun _ -> defer_lambdas);
       "prefer-formulas", (fun _ -> prefer_formulas);
       "defer-formulas", (fun _ -> defer_formulas);
       "prefer-easy-ho", (fun _ -> prefer_easy_ho);
       "prefer-ground", (fun _ -> prefer_ground);
       "defer-ground", (fun _ -> defer_ground);
       "defer-fo", (fun _ -> defer_fo);
       "prefer-fo", (fun _ -> prefer_fo);]

    let of_string s = 
      try 
        List.assoc (String.lowercase_ascii s) parsers s
      with Not_found -> 
        let err_msg = 
          CCFormat.sprintf "unknown priortity: %s.\noptions:@ %a"
            s (CCList.pp ~start:"{" ~stop:"}" CCString.pp) (List.map fst parsers) in
        invalid_arg err_msg
  end

  module H = CCHeap.Make(struct
      (* heap ordered by [priority, weight].
         the lower the better. *)
      type t = (int * int * C.t)
      let leq (i10, i11, c1) (i20, i21, c2) =
        if i10 < i20 then true
        else if (i10 = i20) then (
          if i11 < i21 then true 
          else if i11 = i21 then (
            C.compare c1 c2 < 0
          ) else false
        ) else false
    end)

  (** A priority queue of clauses + FIFO queue *)
  type t =
    | FIFO of C.t Queue.t
    | Mixed of mixed

  and mixed = {
    mutable heaps : H.t array;
    mutable weight_funs : (C.t -> (int * int)) array;
    tbl: unit C.Tbl.t;
    mutable ratios: int array;
    mutable ratios_limit: int;
    mutable current_step: int;
    mutable current_heap_idx: int;
  }

  (** generic clause queue based on some ordering on clauses, given
      by a weight function *)
  let is_empty_mixed q = C.Tbl.length q.tbl = 0

  let is_empty (q:t) = match q with
    | FIFO q -> Queue.is_empty q
    | Mixed q -> is_empty_mixed q

  let length q = match q with
    | FIFO q -> Queue.length q
    | Mixed q -> C.Tbl.length q.tbl

  let add q c = match q with
    | FIFO q -> Queue.push c q
    | Mixed q ->
      if not (C.Tbl.mem q.tbl c) then (
        C.Tbl.add q.tbl c ();
        let weights = Array.map (fun f -> f c) q.weight_funs in
        let heaps = Array.mapi (fun i (prio,weight) ->  
            let heap = Array.get q.heaps i in
            H.insert (prio,weight,c) heap) weights in
        q.heaps <- heaps)

  let add_seq q hcs = Iter.iter (add q) hcs

  let rec take_first_mixed q =
    let move_queue q =
      if q.current_step < q.ratios_limit then (
        (* we still have to pick from current heap *)
        q.current_step <- q.current_step + 1;
      ) else ( 
        (* we have to choose the next heap *)

        if (q.current_heap_idx + 1 = Array.length (q.heaps)) then (
          (* cycled through all the heaps, starting over  *)
          q.current_heap_idx <- 0;
          q.current_step <- 0;
          q.ratios_limit <- Array.get q.ratios 0;
        ) else (
          (* moving to the next heap  *)
          q.current_step <- q.current_step + 1;
          q.current_heap_idx <- q.current_heap_idx + 1;
          q.ratios_limit <- q.ratios_limit + (Array.get q.ratios q.current_heap_idx)
        )
      ) in

    if is_empty_mixed q then raise Not_found;
    (* find next clause *)
    let current_heap = Array.get q.heaps q.current_heap_idx in
    let current_heap, (_,_,c) =  H.take_exn current_heap in
    Array.set q.heaps q.current_heap_idx current_heap;

    if not (C.Tbl.mem q.tbl c) then take_first_mixed q
    else (
      C.Tbl.remove q.tbl c;
      move_queue q;
      c
    )

  let mixed_eval = {
    heaps=CCArray.empty;
    weight_funs=CCArray.empty;
    tbl=C.Tbl.create 16;
    ratios = CCArray.empty;
    ratios_limit=0;
    current_step=0;
    current_heap_idx=0;
  }

  let add_to_mixed_eval ~ratio ~weight_fun =
    let was_empty = CCArray.length mixed_eval.heaps = 0 in
    mixed_eval.heaps <- Array.append mixed_eval.heaps ([| H.empty |]);
    mixed_eval.weight_funs <- Array.append mixed_eval.weight_funs ([| weight_fun |]);
    mixed_eval.ratios <- Array.append mixed_eval.ratios ([| ratio |]);
    if was_empty then (
      mixed_eval.ratios_limit <- ratio
    );
    Mixed mixed_eval

  let take_first = function
    | FIFO q ->
      if Queue.is_empty q then raise Not_found else Queue.pop q
    | Mixed q -> take_first_mixed q

  let name q = match q with
    | FIFO _ -> "bfs"
    | Mixed q -> "mixed"

  (** {6 Combination of queues} *)

  let const_prioritize_fun wf =
    (fun c -> wf c, 1)

  let fifo_wf c = C.id c, 1

  let goal_oriented () : t =
    let open WeightFun in
    let weight =
      penalize ( 
        combine [default, 4; favor_small_num_vars, 2;
                 favor_goal, 1; favor_all_neg, 1; ]
      ) in
    (* make ~ratio:6 ~weight name *)
    let weight_fun = const_prioritize_fun weight in
    ignore(add_to_mixed_eval ~ratio:5 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let bfs () : t = FIFO (Queue.create ())

  let almost_bfs () : t =
    let open WeightFun in
    let weight = 
      penalize ( combine [ default, 3; ] ) in
    (* make ~ratio:1 ~weight "almost_bfs" *)
    let weight_fun = const_prioritize_fun weight in
    ignore(add_to_mixed_eval ~ratio:1 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let explore () : t =
    let open WeightFun in
    (* make ~ratio:6 ~weight "explore" *)
    let weight_fun = const_prioritize_fun explore_fun in
    ignore(add_to_mixed_eval ~ratio:5 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let ground () : t =
    let open WeightFun in
    let weight =
      penalize (
        combine [favor_pos_unit, 1; favor_ground, 2;
                 favor_small_num_vars, 10; ]
      )
    in
    (* make ~ratio:6 ~weight "ground" *)
    let weight_fun = const_prioritize_fun weight in
    ignore(add_to_mixed_eval ~ratio:5 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let default () : t =
    let open WeightFun in
    (* make ~ratio:6 ~weight "default" *)
    let weight_fun = const_prioritize_fun default_fun in
    ignore(add_to_mixed_eval ~ratio:5 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let conj_relative_mk () : t =
    (* make ~ratio:6 ~weight:WeightFun.conj_relative "conj_relative" *)
    let weight_fun = const_prioritize_fun WeightFun.conj_relative in
    ignore(add_to_mixed_eval ~ratio:5 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let conj_var_relative_mk () : t =
    (* make ~ratio:!cr_var_ratio ~weight:(WeightFun.conj_relative ~distinct_vars_mul:!cr_var_mul)
         "conj_relative_var" *)
    let weight_fun = const_prioritize_fun 
        (WeightFun.conj_relative ~distinct_vars_mul:!cr_var_mul 
           ~parameters_magnitude:!parameters_magnitude ~goal_penalty:!goal_penalty) in
    ignore(add_to_mixed_eval ~ratio:!cr_var_ratio ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let ho_weight () =
    (* make ~ratio:4 ~weight:WeightFun.ho_weight_calc "ho-weight" *)
    let weight_fun = const_prioritize_fun WeightFun.ho_weight_calc in
    ignore(add_to_mixed_eval ~ratio:3 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let ho_weight_init () =
    (* make ~ratio:5 ~weight:WeightFun.ho_weight_initial "ho-weight-init" *)
    let weight_fun = const_prioritize_fun WeightFun.ho_weight_initial in
    ignore(add_to_mixed_eval ~ratio:4 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let avoid_expensive_mk () : t =
    (* make ~ratio:20 ~weight:WeightFun.avoid_expensive "avoid-expensive" *)
    let weight_fun = const_prioritize_fun WeightFun.avoid_expensive in
    ignore(add_to_mixed_eval ~ratio:10 ~weight_fun);
    add_to_mixed_eval ~ratio:1 ~weight_fun:fifo_wf

  let of_profile p =
    let open ClauseQueue_intf in
    if CCList.is_empty !funs_to_parse then (
      match p with
      | P_default -> default ()
      | P_bfs -> bfs ()
      | P_almost_bfs -> almost_bfs ()
      | P_explore -> explore ()
      | P_ground -> ground ()
      | P_goal -> goal_oriented ()
      | P_conj_rel ->  conj_relative_mk ()
      | P_conj_rel_var -> conj_var_relative_mk ()
      | P_ho_weight -> ho_weight ()
      | P_ho_weight_init -> ho_weight_init ()
      | P_avoid_expensive -> avoid_expensive_mk ())
    else (
      List.fold_left (fun _ (ratio, prio, weight) -> 
          let prio_fun = PriorityFun.of_string prio in
          let weight_fun = WeightFun.of_string weight in
          add_to_mixed_eval ~ratio ~weight_fun:(fun c -> prio_fun c, weight_fun c)
        ) (Mixed mixed_eval) !funs_to_parse
    )

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end



let parse_wf_with_priority s = 
  let wf_with_prio_regex = Str.regexp "\\([0-9]+\\)|\\(.+\\)|\\(.+\\)" in
  try
    ignore(Str.search_forward wf_with_prio_regex s 0);
    let ratio = CCOpt.get_exn (CCInt.of_string (Str.matched_group 1 s)) in
    let priority_str = CCString.trim (Str.matched_group 2 s) in
    let weight_fun = CCString.trim (Str.matched_group 3 s) in
    funs_to_parse := (ratio, priority_str, weight_fun) :: !funs_to_parse
  with Not_found | Invalid_argument _ -> 
    invalid_arg 
      "weight funciton is of the form \"ratio:int|priority:name|weight:name(options..)\""

let () =
  let o = Arg.String (parse_profile) in
  let add_queue = Arg.String parse_wf_with_priority in
  Params.add_opts
    [ "--clause-queue", o,
      " choose which set of clause queues to use (for selecting next active clause)";
      "-cq", o, " alias to --clause-queue";
      "--add-queue", add_queue, " create a new clause evaluation queue. Its description is of the form" ^ 
                                " RATIO|PRIORITY_FUN|WEIGHT_FUN";
      "-q", add_queue, "alias to --add-queue"
    ];

  Params.add_to_mode "ho-pragmatic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
    );
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
    );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
    );

