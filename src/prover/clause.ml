
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

open Logtk

module BV = CCBV
module T = Term
module S = Subst
module Lit = Literal
module Lits = Literals
module Stmt = Statement

let stat_clause_create = Util.mk_stat "clause.create"

module type S = Clause_intf.S

type proof_step = Proof.Step.t

(** Bundle of clause sets *)
type 'c sets = {
  c_set: 'c CCVector.ro_vector; (** main set of clauses *)
  c_sos: 'c CCVector.ro_vector; (** set of support *)
}

(** {2 Type def} *)
module Make(Ctx : Ctx.S) : S with module Ctx = Ctx = struct
  module Ctx = Ctx

  type flag = SClause.flag

  (* re-export type, to access fields *)
  type sclause = SClause.t = private {
    id: int;
    lits: Literals.t;
    trail: Trail.t;
    mutable flags: flag;
  }

  type t = {
    sclause : sclause;
    mutable penalty: int; (** heuristic penalty *)
    selected : BV.t Lazy.t; (** bitvector for selected lits*)
    bool_selected : (Term.t * Position.t) list Lazy.t;
    max_lits : int list Lazy.t; (** bitvector for maximal lits *)
    mutable proof : proof_step; (** Proof of the clause *)
    mutable eligible_res: BV.t option; (* eligible for resolution? *)
    mutable eligible_bool : SClause.TPSet.t option;
  }

  type clause = t

  (** {2 boolean flags} *)

  let get_flag flag c = SClause.get_flag flag c.sclause
  let set_flag flag c b = SClause.set_flag flag c.sclause b

  let mark_redundant c = set_flag SClause.flag_redundant c true
  let is_redundant c = get_flag SClause.flag_redundant c
  let mark_backward_simplified c = set_flag SClause.flag_backward_simplified c true
  let is_backward_simplified c = get_flag SClause.flag_backward_simplified c

  (** {2 Hashcons} *)

  let equal c1 c2 = SClause.equal c1.sclause c2.sclause

  let compare c1 c2 = SClause.compare c1.sclause c2.sclause

  let hash c = SClause.hash c.sclause

  let id c = SClause.id c.sclause

  let is_ground c = Literals.is_ground c.sclause.lits

  let weight c = Lits.weight c.sclause.lits
  let ho_weight c = 
    Lits.Seq.terms c.sclause.lits
    |> Iter.fold (fun acc t -> T.ho_weight t + acc) 0

  let trail c = c.sclause.trail
  let has_trail c = not (Trail.is_empty c.sclause.trail)
  let trail_subsumes c1 c2 = Trail.subsumes c1.sclause.trail c2.sclause.trail
  let is_active c ~v = Trail.is_active c.sclause.trail ~v
  let penalty c = c.penalty
  let inc_penalty c inc = c.penalty <- c.penalty + inc

  let trail_l = function
    | [] -> Trail.empty
    | [c] -> c.sclause.trail
    | [c1; c2] -> Trail.merge c1.sclause.trail c2.sclause.trail
    | l -> Trail.merge_l (List.map trail l)

  let lits c = c.sclause.lits

  module Tbl = CCHashtbl.Make(struct
      type t = clause
      let hash = hash
      let equal = equal
    end)

  (** {2 Utils} *)

  let is_goal c = Proof.Step.is_goal c.proof

  let distance_to_goal c = Proof.Step.distance_to_goal c.proof
  let comes_from_goal c = CCOpt.is_some @@ distance_to_goal c

  (* private function for building clauses *)
  let create_inner ~penalty ~selected ~bool_selected sclause proof =
    (* create the structure *)
    let ord = Ctx.ord () in
    let max_lits = lazy ( BV.to_list @@ Lits.maxlits sclause.lits ~ord ) in
    let rec c = {
      sclause;
      penalty;
      selected;
      bool_selected;
      proof;
      max_lits;
      eligible_res=None;
      eligible_bool=None;
    } in
    (* return clause *)
    Util.incr_stat stat_clause_create;
    c

  let of_sclause ?(penalty=1) c proof =
    let selected = lazy (Ctx.select c.lits) in
    let bool_selected = lazy (Ctx.bool_select c.lits) in
    create_inner ~penalty ~selected ~bool_selected c proof

  let lit_is_false_ = function
    | Literal.False -> true
    | _ -> false

  let create_a ~penalty ~trail lits proof =
    (* remove spurious "false" literals automatically *)
    let lits =
      if CCArray.exists lit_is_false_ lits
      then CCArray.filter (fun lit -> not (lit_is_false_ lit)) lits
      else lits
    in
    let selected = lazy (Ctx.select lits) in
    let bool_selected = lazy (Ctx.bool_select lits) in
    create_inner ~penalty ~selected ~bool_selected (SClause.make ~trail lits) proof

  let create ~penalty ~trail lits proof =
    (* let lits = List.fast_sort (fun l1 l2 -> -CCInt.compare (Lit.hash l1) (Lit.hash l2)) lits in *)
    create_a ~penalty ~trail (Array.of_list lits) proof

  let of_forms ?(penalty=1) ~trail forms proof =
    let lits = List.map Ctx.Lit.of_form forms |> Array.of_list in
    create_a ~penalty ~trail lits proof

  let of_forms_axiom ?(penalty=1) ~file ~name forms =
    let lits = List.map Ctx.Lit.of_form forms in
    let proof = Proof.Step.assert' ~file ~name () in
    create ~penalty ~trail:Trail.empty lits proof

  let of_statement ?(convert_defs=false) st =
    let of_lits lits =
      (* convert literals *)
      let lits = List.map Ctx.Lit.of_form lits in
      let proof = Stmt.proof_step st in
      let c = create ~trail:Trail.empty ~penalty:1 lits proof in
      c
    in
    match Stmt.view st with
    | Stmt.Data _
    | Stmt.TyDecl _ -> []
    | Stmt.Def _
    | Stmt.Rewrite _ -> 
      if not convert_defs then [] (*dealt with by rewriting *)
      (* dealt with  *)
      else List.map of_lits (Stmt.get_formulas_from_defs st)
    | Stmt.Assert lits -> [of_lits lits]
    | Stmt.Goal lits -> [of_lits lits]
    | Stmt.Lemma l
    | Stmt.NegatedGoal (_,l) -> List.map of_lits l

  let update_trail f c =
    let sclause = SClause.update_trail f c.sclause in
    create_inner sclause c.proof ~selected:c.selected ~bool_selected:c.bool_selected ~penalty:c.penalty

  let proof_step c = c.proof

  let proof c = Proof.S.mk c.proof (SClause.mk_proof_res c.sclause)
  let proof_parent c = Proof.Parent.from (proof c)

  let proof_parent_subst renaming (c,sc) subst =
    Proof.Parent.from_subst renaming (proof c,sc) subst

  let update_proof c f =
    let new_proof = f c.proof in
    create_a ~trail:c.sclause.trail ~penalty:c.penalty c.sclause.lits new_proof

  let is_empty c =
    Lits.is_absurd c.sclause.lits && Trail.is_empty c.sclause.trail

  let length c = SClause.length c.sclause

  let _apply_subst_no_simpl subst (lits,sc) =
    if Subst.is_empty subst
    then lits (* id *)
    else
      let renaming = S.Renaming.create () in
      Array.map
        (fun l -> Lit.apply_subst_no_simp renaming subst (l,sc))
        lits

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are maximal under [ord] *)
  let maxlits (c,sc) subst =
    let ord = Ctx.ord () in
    if not @@ Subst.is_empty subst then (
      let lits' = _apply_subst_no_simpl subst (lits c,sc) in
      Lits.maxlits ~ord lits')
    else BV.of_list @@ Lazy.force c.max_lits

  (** Check whether the literal is maximal *)
  let is_maxlit (c,sc) subst ~idx =
    if not @@ Subst.is_empty subst then (
      let ord = Ctx.ord () in
      let lits' = _apply_subst_no_simpl subst (lits c,sc) in
      Lits.is_max ~ord lits' idx
    ) else (BV.get (BV.of_list @@ Lazy.force c.max_lits) idx)


  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. *)
  let eligible_res (c,sc) subst =
    let ord = Ctx.ord () in
    let selected = Lazy.force c.selected in
    let bool_selected = Lazy.force c.bool_selected in
    if BV.is_empty selected && CCList.is_empty bool_selected
    then (
      (* maximal literals *)
      if not @@ Subst.is_empty subst then (
        let lits' = _apply_subst_no_simpl subst (lits c,sc) in
        Lits.maxlits ~ord lits'
      ) else (BV.of_list @@ Lazy.force c.max_lits)
    ) else (
      let lits' = _apply_subst_no_simpl subst (lits c,sc) in
      let bv = BV.copy selected in
      let n = Array.length lits' in
      (* Only keep literals that are maximal among selected literals of the
          same sign. *)
      for i = 0 to n-1 do
        (* i-th lit is already known not to be max? *)
        if not (BV.get bv i) then () else
          let lit = lits'.(i) in
          for j = i+1 to n-1 do
            let lit' = lits'.(j) in
            (* check if both lits are still potentially eligible, and have the same
               sign if [check_sign] is true. *)
            if Lit.is_positivoid lit = Lit.is_positivoid lit' && BV.get bv j
            then match Lit.Comp.compare ~ord lit lit' with
              | Comparison.Incomparable
              | Comparison.Eq -> ()     (* no further information about i-th and j-th *)
              | Comparison.Gt -> BV.reset bv j  (* j-th cannot be max *)
              | Comparison.Lt -> BV.reset bv i  (* i-th cannot be max *)
          done;
      done;
      bv
    )

  let eligible_res_no_subst c =
    begin match c.eligible_res with
      | Some r -> r
      | None ->
        let bv = eligible_res (c,0) Subst.empty in
        c.eligible_res <- Some bv;
        bv
    end

  let eligible_subterms_of_bool_ c =
    let module PB = Position.Build in
    let starting_positions = 
      Lazy.force c.bool_selected
      |> List.map (fun (_, pos) -> Position.Build.of_pos pos) 
    in
    let res = 
      (* directly at position of selected booleans *)
      Lazy.force c.bool_selected 
      @
      (* below selected selected booleans *)
      CCList.flat_map (fun pb ->
        let pos = Position.Build.to_pos pb in 
        let t = Literals.Pos.at (lits c) pos in
        (* selects --subterms-- of given t that are eligible *)
        Iter.to_list 
          (Bool_selection.all_eligible_subterms ~ord:(Ctx.ord()) ~pos_builder:pb t)) 
      (starting_positions)
    in
    let res =  
      List.filter (fun (_,p) -> 
        let module P = Position in
        match p with
        | P.Arg(idx, P.Left P.Stop)
        | P.Arg(idx, P.Right P.Stop) ->
          (match (lits c).(idx) with 
          | Lit.Equation(_,_,false) -> true
          | _ -> false)
        | _ -> true
      ) res 
    in

    if CCList.is_empty res then (
      Util.debugf 1 "nothing selected for @[%a@]@." (fun k -> k Lits.pp (lits c));
    ) else (
      Util.debugf 1 "For @[%a@]@." (fun k -> k Lits.pp (lits c));
      CCList.iter (fun (t,p) -> 
        Util.debugf 1 "  |@[%a@] -> @[%a@]@." (fun k -> k Position.pp p T.pp t);
      ) res;
    );

    res

  let eligible_subterms_of_bool c =
    match c.eligible_bool with 
    | None ->
      let res = SClause.TPSet.of_list (eligible_subterms_of_bool_ c) in
      c.eligible_bool <- Some res;
      res
    | Some cache -> cache

  let eta_reduce c =
    let lit_arr = lits c in
    let changed = ref false in
    let new_lits = Literals.map (fun t -> 
        let reduced = Lambda.eta_reduce (Lambda.snf t) in
        if not (Term.equal t reduced) then changed := true;
        reduced) lit_arr in
    if !changed then (
      let penalty = penalty c and trail = trail c and proof = proof_step c in
      Some (create ~penalty ~trail (CCArray.to_list new_lits) proof)
    ) else None 

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. *)
  let eligible_param (c,sc) subst =
    let ord = Ctx.ord () in
    if BV.is_empty (Lazy.force c.selected) && 
       CCList.is_empty (Lazy.force c.bool_selected) then (
      let bv, lits' = 
        if not @@ Subst.is_empty subst then (
          let lits' = _apply_subst_no_simpl subst (lits c,sc) in
          (* maximal ones *)
          Lits.maxlits ~ord lits', lits')
        else (BV.of_list @@ Lazy.force c.max_lits, lits c) in
      (* only keep literals that are positive equations *)
      BV.filter bv (fun i -> Lit.eqn_sign lits'.(i) (* == true*));
      bv
    ) else
      BV.empty () (* no eligible literal when some are selected *)

  let is_eligible_param (c,sc) subst ~idx =
    Lit.eqn_sign c.sclause.lits.(idx)
    &&
    BV.is_empty (Lazy.force c.selected)
    &&
    CCList.is_empty (Lazy.force c.bool_selected)
    &&
    is_maxlit (c,sc) subst ~idx

  (** are there selected literals in the clause? *)
  let has_selected_lits c = not (BV.is_empty (Lazy.force c.selected))

  (** Check whether the literal is selected *)
  let is_selected c i = BV.get (Lazy.force c.selected) i

  (** Indexed list of selected literals *)
  let selected_lits c = BV.selecti (Lazy.force c.selected) c.sclause.lits

  let selected_lits_bv c = Lazy.force c.selected

  let bool_selected c  = Lazy.force c.bool_selected

  (** is the clause a unit clause? *)
  let is_unit_clause c = match c.sclause.lits with
    | [|_|] -> true
    | _ -> false

  let is_oriented_rule c =
    let ord = Ctx.ord () in
    match c.sclause.lits with
    | [| Lit.Equation (l, r, true) |] ->
      (* counting predicate literals of the form p = FALSE as rewrite rules *)
      begin match Ordering.compare ord l r with
        | Comparison.Gt
        | Comparison.Lt -> true
        | Comparison.Eq
        | Comparison.Incomparable -> false
      end
    | _ -> false

  let symbols ?(init=ID.Set.empty) ?(include_types=false) seq =
    Iter.fold
      (fun set c -> Lits.symbols ~include_types ~init:set c.sclause.lits)
      init seq

  let to_forms c = Lits.Conv.to_forms c.sclause.lits
  let to_sclause c = c.sclause

  let to_s_form c = SClause.to_s_form c.sclause

  let is_inj_axiom c =
    if CCArray.length (lits c) = 2 then (
      let inj_defs = CCArray.filter_map Lit.as_inj_def (lits c) in
      if (CCArray.length inj_defs) != 1 then None
      else (
        let sym, var_pairs = CCArray.get inj_defs 0 in
        let l = CCArray.filter_map Lit.as_pos_pure_var (lits c) |> CCArray.to_list in
        if List.length l != 1 then None
        else (
          let (x, y) = List.hd l in
          let v_eq = HVar.equal Type.equal in
          let rec args_same n = function 
            | [] -> None
            | (x', y') :: xs -> if (v_eq x x' && v_eq y y') || 
                                   (v_eq x y' && v_eq y x') then Some (sym, n)
              else args_same (n+1) xs in
          args_same 0 var_pairs
        )
      )
    ) else None

  let proof_depth c =
    Proof.Step.inferences_performed (proof_step c)

  module Seq = struct
    let lits c = Iter.of_array c.sclause.lits
    let terms c = lits c |> Iter.flat_map Lit.Seq.terms
    let vars c = terms c |> Iter.flat_map T.Seq.vars
  end

  let apply_subst ?(renaming) ?(proof=None) ?(penalty_inc=None) (c,sc) subst =
    let lits = lits c in
    let renaming = CCOpt.get_or ~default:(S.Renaming.create ()) renaming in
    let new_lits = Literals.apply_subst renaming  subst (lits, sc) in
    let proof_step = CCOpt.get_or ~default:(proof_step c) proof in
    (* increase can be negative if we perform a simplification *)
    let penalty = max ((CCOpt.get_or ~default:0 penalty_inc) + (penalty c)) 1 in
    create ~trail:(trail c) ~penalty (CCArray.to_list new_lits) proof_step


  let ground_clause c =
    let new_lits = CCArray.to_list @@ Lits.ground_lits (lits c) in
    let proof_step = proof_step c in
    create ~trail:(trail c) ~penalty:(penalty c) new_lits proof_step

  let is_orphaned c =
    let aux proof =
      let p_res, step = Proof.S.result proof, Proof.S.step proof in
      (* we reached the root *)
      if Proof.Result.is_stmt p_res then false
      else (
        Proof.Step.is_inference step &&
        not (List.mem Proof.Tag.T_cannot_orphan (Proof.Step.tags step)) &&
        List.exists (fun pr ->
          Proof.Result.is_dead_cl (Proof.S.result pr) ())
        (List.map Proof.Parent.proof (Proof.Step.parents step))) 
    in
    not (is_empty c) && aux (proof c)
  (** {2 Filter literals} *)

  module Eligible = struct
    type t = int -> Lit.t -> bool

    let res c =
      let bv = eligible_res (Scoped.make c 0) S.empty in
      fun i _lit -> BV.get bv i

    let param c =
      let bv = eligible_param (Scoped.make c 0) S.empty in
      fun i _lit -> BV.get bv i

    let eq _ lit = match lit with
      | Lit.Equation (_, _, true) -> true
      | _ -> false

    let filter f _ lit = f lit

    let max c =
      fun i _ -> BV.get (BV.of_list @@ Lazy.force c.max_lits) i

    let pos _ lit = Lit.is_positivoid lit

    let pos_eq _ lit = match lit with
      | Lit.Equation(l,r,s) -> s
      | _ -> false

    let neg _ lit = Lit.is_negativoid lit

    let always _ _ = true

    let combine l = match l with
      | [] -> (fun _ _ -> true)
      | [x] -> x
      | [x; y] -> (fun i lit -> x i lit && y i lit)
      | [x; y; z] -> (fun i lit -> x i lit && y i lit && z i lit)
      | _ -> (fun i lit -> List.for_all (fun eligible -> eligible i lit) l)

    let ( ** ) f1 f2 i lit = f1 i lit && f2 i lit
    let ( ++ ) f1 f2 i lit = f1 i lit || f2 i lit
    let ( ~~ ) f i lit = not (f i lit)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet = CCSet.Make(struct
      type t = clause
      let compare c1 c2 = SClause.compare c1.sclause c2.sclause
    end)

  (** {2 Positions in clauses} *)

  module Pos = struct
    let at c pos = Lits.Pos.at c.sclause.lits pos
  end

  module WithPos = struct
    type t = {
      clause : clause;
      pos : Position.t;
      term : T.t;
    }

    let compare t1 t2 =
      let c = SClause.compare t1.clause.sclause t2.clause.sclause in
      if c <> 0 then c else
        let c = T.compare t1.term t2.term in
        if c <> 0 then c else
          Position.compare t1.pos t2.pos

    let pp out t =
      Format.fprintf out "@[clause @[%a@]@ at pos @[%a@]@]"
        Lits.pp t.clause.sclause.lits Position.pp t.pos
  end

  (** {2 IO} *)

  let pp_trail = SClause.pp_trail

  let pp out c =
    let pp_selected selected out i =
      if BV.get selected i then Format.fprintf out "+"
    and pp_maxlit maxlits out i =
      if BV.get maxlits i then Format.fprintf out "*"
    in
    (* print literals with a '*' for maximal, and '+' for selected *)
    let selected = Lazy.force c.selected in
    let max = maxlits (Scoped.make c 0) S.empty in
    let pp_lits out lits =
      if Array.length lits = 0
      then CCFormat.string out "⊥"
      else (
        let pp_lit out (i,lit) =
          Format.fprintf out "@[%a%a%a@]"
            Lit.pp lit (pp_selected selected) i (pp_maxlit max) i
        in
        Format.fprintf out "[@[%a@]]"
          (Util.pp_iter ~sep:" ∨ " pp_lit)
          (Iter.of_array_i lits)
      )
    in
    Format.fprintf out "@[%a@[<2>%a%a@]@]/id:%d/depth:%d/penalty:%d/red:%b"
      SClause.pp_vars c.sclause pp_lits c.sclause.lits
      SClause.pp_trail c.sclause.trail c.sclause.id
      (proof_depth c) (penalty c) (is_redundant c);
    ()

  let pp_tstp out c = SClause.pp_tstp out c.sclause

  let pp_tstp_full out c = SClause.pp_tstp_full out c.sclause

  let to_string = CCFormat.to_string pp
  let to_string_tstp = CCFormat.to_string pp_tstp

  let pp_set out set =
    Format.fprintf out "{@[<hv>%a@]}"
      (Util.pp_iter ~sep:"," pp)
      (ClauseSet.to_iter set)

  let pp_set_tstp out set =
    Format.fprintf out "@[<v>%a@]"
      (Util.pp_iter ~sep:"," pp_tstp)
      (ClauseSet.to_iter set)


  let check_types c =
    Util.debugf 5 "(@[check_types@ %a@])" (fun k->k pp c);
    lits c
    |> Literals.Seq.terms
    |> Iter.iter
      (fun t -> ignore (Term.rebuild_rec t))
end
