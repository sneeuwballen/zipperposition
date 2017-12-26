
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting on Terms} *)

module T = Term
module Fmt = CCFormat

let section = Util.Section.make "rewrite"
let stat_term_rw = Util.mk_stat "rw.steps_term"
let prof_term_rw = Util.mk_profiler "rw.term"
let stat_lit_rw = Util.mk_stat "rw.steps_lit"
let prof_lit_rw = Util.mk_profiler "rw.lit"

(* do we rewrite literals of the form [t = u]? *)
let allow_pos_eqn_rewrite_ = ref false

type term = Term.t

type proof = Proof.step

type term_rule = {
  term_head: ID.t; (* head symbol of LHS *)
  term_args: term list; (* arguments *)
  term_arity: int; (* [length args] *)
  term_lhs: term; (* [lhs = head args] *)
  term_rhs: term;
  term_proof: proof;
}

type lit_rule = {
  lit_lhs: Literal.t;
  lit_rhs: Literal.t list list; (* list of clauses *)
  lit_proof: proof;
}

let compare_tr r1 r2 =
  CCOrd.(T.compare r1.term_lhs r2.term_lhs
    <?> (T.compare, r1.term_rhs, r2.term_rhs))

let compare_lr r1 r2 =
  let open CCOrd.Infix in
  Literal.compare r1.lit_lhs r2.lit_lhs
  <?> (CCList.compare (CCList.compare Literal.compare), r1.lit_rhs, r2.lit_rhs)

module TR_set = CCSet.Make(struct type t = term_rule let compare = compare_tr end)
module LR_set = CCSet.Make(struct type t = lit_rule let compare = compare_lr end)

type defined_position = Defined_pos.t
type defined_positions = defined_position IArray.t

type rule =
  | T_rule of term_rule
  | L_rule of lit_rule

let compare_rule r1 r2 =
  let to_int = function T_rule _ -> 0 | L_rule _ -> 1 in
  begin match r1, r2 with
    | T_rule r1, T_rule r2 -> compare_tr r1 r2
    | L_rule r1, L_rule r2 -> compare_lr r1 r2
    | T_rule _, _
    | L_rule _, _ -> CCInt.compare (to_int r1) (to_int r2)
  end

module Rule_set = CCSet.Make(struct type t = rule let compare = compare_rule end)

type rule_set = Rule_set.t

type defined_cst = {
  defined_id: ID.t;
  defined_ty: Type.t;
  defined_level: int option;
  mutable defined_rules: rule_set;
  (* set of rewrite rules.
      invariant: all these rules have [term_head = defined_id]
      or are equations of type [tau] with [head tau = defined_id] *)
  mutable defined_positions: defined_positions lazy_t; (* metadata on positions *)
}

let pp_term_rule out r =
  Fmt.fprintf out "@[<2>@[%a@] :=@ @[%a@]@]" T.pp r.term_lhs T.pp r.term_rhs

let pp_term_rules out (s:term_rule Sequence.t): unit =
  Fmt.(within "{" "}" @@ hvbox @@ Util.pp_seq pp_term_rule) out s

let pp_lit_rule out r =
  let pp_c = CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp) in
  Format.fprintf out "@[<2>@[%a@] :=@ [@[<v>%a@]]@]"
    Literal.pp r.lit_lhs (Util.pp_list ~sep:"∧" pp_c) r.lit_rhs

let pp_lit_rules out (s:lit_rule Sequence.t): unit =
  Format.fprintf out "{@[<hv>%a@]}" (Util.pp_seq pp_lit_rule) s

let pp_rule out = function
  | T_rule r -> Format.fprintf out "(@[%a [T]@])" pp_term_rule r
  | L_rule l -> Format.fprintf out "(@[%a [B]@])" pp_lit_rule l

let pp_rule_set out (rs: rule_set): unit =
  Fmt.(within "{" "}" @@ hvbox @@ Util.pp_seq pp_rule) out (Rule_set.to_seq rs)

(** Annotation on IDs that are defined. *)
exception Payload_defined_cst of defined_cst

let as_defined_cst id =
  ID.payload_find id
    ~f:(function
      | Payload_defined_cst c -> Some c
      | _ -> None)

let is_defined_cst id = CCOpt.is_some (as_defined_cst id)

module Cst_ = struct
  type t = defined_cst

  let rules t = t.defined_rules
  let rules_seq t = Rule_set.to_seq (rules t)

  let rules_term_seq t : term_rule Sequence.t =
    rules_seq t
    |> Sequence.filter_map
      (function T_rule t -> Some t | _ -> None)

  let rules_lit_seq t : lit_rule Sequence.t =
    rules_seq t
    |> Sequence.filter_map
      (function L_rule t -> Some t | _ -> None)

  let defined_positions t = Lazy.force t.defined_positions
  let ty t = t.defined_ty
  let level t = CCOpt.get_or ~default:0 t.defined_level

  let pp out (t:t): unit =
    Fmt.fprintf out "(@[defined_id@ :ty %a @ :rules %a@ :positions %a@])"
      Type.pp (ty t) pp_rule_set (rules t)
      Defined_pos.Arr.pp (defined_positions t)

  let to_string = Fmt.to_string pp
end

type pseudo_rule = ID.t * term list * term list Sequence.t
(* LHS id, LHS args, sequence of occurrences of same ID's arguments on RHS *)

(* compute position roles for a set of rules with the given terms as LHS *)
let compute_pos_gen (l:pseudo_rule list): defined_positions =
  (* ID, number of arguments *)
  let id, n = match l with
    | [] -> assert false
    | (id,args,_) :: _ -> id, List.length args
  in
  assert
    (l|>List.for_all
       (fun (id',args',_) -> ID.equal id id' && List.length args'=n));
  (* now compute position roles *)
  let pos = Array.make n Defined_pos.P_invariant in
  begin
    Sequence.of_list l
    |> Sequence.flat_map
      (fun (_,args,rhs) ->
         fun yield -> List.iteri (fun i sub -> yield (rhs,i,sub)) args)
    |> Sequence.iter
      (fun (rhs,i,arg) ->
         begin match T.view arg with
           | T.Var x ->
             (* if all occurrences of [r.id] on the RHS also have [x] at this
                position, the position stays invariant, otherwise
                it is an accumulator *)
             let is_invariant =
               rhs
               |> Sequence.filter_map
                 (fun args' ->
                    let len = List.length args' in
                    if len > i
                    then Some (List.nth args' (len-i-1))
                    else None)
               |> Sequence.for_all
                 (fun sub -> match T.view sub with
                    | T.Var y -> HVar.equal Type.equal x y
                    | _ -> false)
             in
             (* position is accumulator *)
             if not is_invariant && pos.(i) = Defined_pos.P_invariant then (
               pos.(i) <- Defined_pos.P_accumulator;
             );
           | _ ->
             (* pattern, consider this as input *)
             pos.(i) <- Defined_pos.P_active
         end)
  end;
  IArray.of_array_unsafe pos

module Term = struct
  type rule = term_rule
  module Rule = struct
    type t = term_rule

    let lhs r = r.term_lhs
    let rhs r = r.term_rhs
    let head_id r = r.term_head
    let args r = r.term_args
    let arity r = r.term_arity
    let ty r = T.ty r.term_rhs
    let proof r = r.term_proof

    let as_lit (r:t): Literal.t = Literal.mk_eq (lhs r)(rhs r)

    let vars r = T.vars (lhs r)
    let vars_l r = vars r |> T.VarSet.to_list

    let make_ head args term_lhs term_rhs proof =
      let term_proof =
        Proof.Step.define head (Proof.Src.internal[]) [Proof.Parent.from proof]
      in
      { term_head=head; term_args=args; term_arity=List.length args;
        term_lhs; term_rhs; term_proof }

    (* constant rule [id := rhs] *)
    let make_const ~proof id ty rhs : t =
      let lhs = T.const ~ty id in
      assert (Type.equal (T.ty rhs) (T.ty lhs));
      if not (T.VarSet.is_empty @@ T.vars rhs) then (
        Util.invalid_argf
          "Rule.make_const %a %a:@ invalid rule, RHS contains variables"
          ID.pp id T.pp rhs
      );
      make_ id [] lhs rhs proof

    (* [id args := rhs] *)
    let make ~proof id ty args rhs : t =
      let lhs = T.app (T.const ~ty id) args in
      assert (Type.equal (T.ty lhs) (T.ty rhs));
      if not (T.VarSet.subset (T.vars rhs) (T.vars lhs)) then (
        Util.invalid_argf
          "Rule.make_const %a %a:@ invalid rule, RHS contains variables"
          ID.pp id T.pp rhs
      );
      make_ id args lhs rhs proof

    let pp out r = pp_term_rule out r

    let conv_ ~ctx lhs rhs =
      let module F = TypedSTerm.Form in
      F.eq
        (Term.Conv.to_simple_term ctx lhs)
        (Term.Conv.to_simple_term ctx rhs)
      |> F.close_forall


    let to_form ~ctx r = conv_ ~ctx (lhs r) (rhs r)

    let compare = compare_tr
    let hash r = Hash.combine2 (T.hash @@ lhs r) (T.hash @@ rhs r)
    let equal r1 r2 = compare r1 r2 = 0
    let to_string = Fmt.to_string pp
  end

  module Set = struct
    include TR_set
    let pp out (s:t) = pp_term_rules out (to_seq s)
  end
  type rule_set = Set.t

  (* term rules for this ID, if any *)
  let rules_of_id id: rule Sequence.t =
    begin match as_defined_cst id with
      | None -> Sequence.empty
      | Some dcst -> Cst_.rules_term_seq dcst
    end

  module Rule_inst_set = struct
    include CCSet.Make(struct
        type t = term_rule * Subst.t * Scoped.scope
        let compare (t1,s1,sc1) (t2,s2,sc2) =
          let open CCOrd.Infix in
          CCInt.compare sc1 sc2
          <?> (Rule.compare, t1, t2)
          <?> (Subst.compare, s1, s2)
      end)
    let pp out (s:t) : unit =
      let pp_triple out (r,subst,sc) =
        Fmt.fprintf out "(@[%a@ :with %a[%d]@])" pp_term_rule r Subst.pp subst sc
      in
      Fmt.fprintf out "{@[<hv>%a@]}" (Util.pp_seq pp_triple) (to_seq s)
  end

  (* TODO: {b long term}

     use De Bruijn  indices for rewrite rules, with RuleSet.t being a
     decision tree (similar to pattern-matching compilation) on head symbols
     + equality contraints for non-linear rules.

     Use the Term.DB case extensively... *)

  let normalize_term_ max_steps (t0:term): term * Rule_inst_set.t =
    assert (max_steps >= 0);
    let set = ref Rule_inst_set.empty in
    let sc_t = 0 in (* scope of variables of term being normalized *)
    let sc_r = 1 in
    let fuel = ref max_steps in
    (* compute normal form of subterm. Tail-recursive.
       @param k the continuation
       @return [t'] where [t'] is the normal form of [t] *)
    let rec reduce t k = match T.view t with
      | _ when !fuel = 0 -> k t
      | T.Const id ->
        (* pick a constant rule *)
        begin match rules_of_id id |> Sequence.head with
          | Some r when T.is_const r.term_lhs ->
            (* reduce [rhs], but no variable can be bound *)
            assert (T.equal t r.term_lhs);
            let cur_sc_r = sc_r in
            set := Rule_inst_set.add (r,Subst.empty,cur_sc_r) !set;
            Util.incr_stat stat_term_rw;
            decr fuel;
            Util.debugf ~section 5
              "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@]"
              (fun k->k T.pp t Rule.pp r);
            reduce r.term_rhs k
          | Some _ ->
            assert (Type.is_fun (T.ty t) || Type.is_forall (T.ty t));
            k t (* must be a partial application *)
          | None -> k t
        end
      | T.App (f, l) ->
        (* first, reduce subterms *)
        reduce_l l
          (fun l' ->
             let t' = if T.same_l l l' then t else T.app f l' in
             let n_l = List.length l' in
             begin match T.view f with
               | T.Const id ->
                 let find_rule =
                   rules_of_id id
                   |> Sequence.find_map
                     (fun r ->
                        try
                          let n_r = Rule.arity r in
                          let t', l_rest =
                            if n_l=n_r then t', []
                            else if n_r < n_l then (
                              let l1, l2 = CCList.take_drop n_r l' in
                              T.app f l1, l2
                            ) else (
                              raise Exit;
                            )
                          in
                          let subst' =
                            Unif.FO.matching ~pattern:(r.term_lhs,sc_r) (t',sc_t)
                          in
                          let cur_sc_r = sc_r in
                          Some (r, subst', cur_sc_r, l_rest)
                        with Unif.Fail | Exit -> None)
                 in
                 begin match find_rule with
                   | None -> k t'
                   | Some (r, subst, sc_r, l_rest) ->
                     (* rewrite [t = r.lhs\sigma] into [rhs] (and normalize [rhs],
                        which contain variables bound by [subst]) *)
                     Util.debugf ~section 5
                       "(@[<2>rewrite `@[%a@]`@ :using `@[%a@]`@ \
                        :with `@[%a@]`[%d]@ :rest [@[%a@]]@])"
                       (fun k->k T.pp t' Rule.pp r Subst.pp subst sc_r
                           (Util.pp_list ~sep:"," T.pp) l_rest);
                     set := Rule_inst_set.add (r,subst,sc_r) !set;
                     Util.incr_stat stat_term_rw;
                     decr fuel;
                     (* NOTE: not efficient, will traverse [t'] fully *)
                     let rhs = Subst.FO.apply Subst.Renaming.none subst (r.term_rhs,sc_r) in
                     (* add leftover arguments *)
                     let rhs = T.app rhs l_rest in
                     reduce rhs k
                 end
               | _ -> k t'
             end)
      | T.Fun (arg, body) ->
        (* term rewrite rules, because [vars(rhs)⊆vars(lhs)], map
           closed terms to closed terms, so we can safely rewrite under λ *)
        reduce body
          (fun body' ->
             let t =
               if T.equal body body' then t else (T.fun_ arg body')
             in k t)
      | T.Var _
      | T.DB _ -> k t
      | T.AppBuiltin (_,[]) -> k t
      | T.AppBuiltin (b,l) ->
        reduce_l l
          (fun l' ->
             let t' = if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l' in
             k t')
    (* reduce list *)
    and reduce_l (l:_ list) k = match l with
      | [] -> k []
      | t :: tail ->
        reduce_l tail
          (fun tail' ->
             reduce t (fun t' -> k (t' :: tail')))
    in
    reduce t0 (fun t->t, !set)

  let normalize_term ?(max_steps=max_int) (t:term): term * Rule_inst_set.t =
    Util.with_prof prof_term_rw (normalize_term_ max_steps) t

  let normalize_term_fst ?max_steps t = fst (normalize_term ?max_steps t)

  let narrow_term ?(subst=Unif_subst.empty) ~scope_rules:sc_r (t,sc_t): _ Sequence.t =
    begin match T.view t with
      | T.Const _ -> Sequence.empty (* already normal form *)
      | T.App (f, _) ->
        begin match T.view f with
          | T.Const id ->
            (* try to match the rules of [id] *)
            rules_of_id id
            |> Sequence.filter_map
              (fun r ->
                 try Some (r, Unif.FO.unify_full ~subst (r.term_lhs,sc_r) (t,sc_t))
                 with Unif.Fail -> None)
          | _ -> Sequence.empty
        end
      | T.Fun _
      | T.Var _
      | T.DB _
      | T.AppBuiltin _ -> Sequence.empty
    end
end

module Lit = struct
  type rule = lit_rule

  module Rule = struct
    type t = lit_rule

    let rule = Proof.Rule.mk "rw.lit"

    let make ~proof lit_lhs lit_rhs =
      let lit_proof =
        Proof.Step.esa ~rule [Proof.Parent.from proof]
      in
      {lit_lhs; lit_rhs; lit_proof}

    let lhs c = c.lit_lhs
    let rhs c = c.lit_rhs
    let proof c = c.lit_proof

    (* conversion into regular clauses *)
    let as_clauses (c:t): Literals.t list =
      assert (not (Literal.is_constraint @@ lhs c));
      List.map
        (fun rhs_c -> Array.of_list (Literal.negate (lhs c) :: rhs_c))
        (rhs c)

    let head_id c = match lhs c with
      | Literal.Prop (t, _) ->
        begin match T.view t with
          | T.Const id -> Some id
          | T.App (f, _) ->
            begin match T.view f with
              | T.Const id -> Some id | _ -> assert false
            end
          | _ -> assert false
        end
      | Literal.Equation _ -> None
      | _ -> assert false

    let is_equational c = match lhs c with
      | Literal.Equation _ -> true
      | _ -> false

    let conv_ ~ctx lhs rhs =
      let module F = TypedSTerm.Form in
      (* put variables of [lhs] first, so that that instances of [lhs := rhs]
         resemble [forall vars(lhs). (lhs = (forall …. rhs))] *)
      let close_forall_ord lhs f =
        let module TT = TypedSTerm in
        let vars_lhs = TT.free_vars_set lhs in
        let vars =
          TT.free_vars f
          |> List.sort
            (fun v1 v2 ->
               let c1 = Var.Set.mem vars_lhs v1 in
               let c2 = Var.Set.mem vars_lhs v2 in
               if c1=c2 then Var.compare v1 v2
               else if c1 then -1
               else 1)
        in
        F.forall_l vars f
      in
      let conv_lit lit = Literal.Conv.to_s_form ~ctx lit in
      let lhs = conv_lit lhs in
      F.equiv
        lhs
        (rhs |> List.map (fun l -> List.map conv_lit l |> F.or_) |> F.and_)
      |> close_forall_ord lhs

    let to_form ~ctx r = conv_ ~ctx (lhs r) (rhs r)

    let vars r =
      Sequence.cons (lhs r)
        (Sequence.of_list (rhs r) |> Sequence.flat_map_l CCFun.id)
      |> Sequence.flat_map Literal.Seq.vars
      |> T.VarSet.of_seq |> T.VarSet.to_list

    let compare r1 r2: int = compare_lr r1 r2
    let pp = pp_lit_rule
  end

  module Set = struct
    include LR_set

    let add_clause r s =
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]" (fun k->k Rule.pp r);
      add r s

    let pp out s = pp_lit_rules out (to_seq s)
  end

  (* rules on equality *)
  let eq_rules_ : Set.t ref = ref Set.empty

  let add_eq_rule (r:Rule.t): unit = match Rule.lhs r with
    | Literal.Equation (t,u,sign) ->
      let ty = T.ty t in
      if sign && not !allow_pos_eqn_rewrite_ && T.is_var t && T.is_var u then (
        (* ignore positive rules *)
        Util.debugf ~section 2 "@[<2>ignore positive equational rewrite `%a`@]"
          (fun k->k Rule.pp r);
      ) else if Type.is_const ty || Type.is_app ty then (
        eq_rules_ := Set.add r !eq_rules_;
      ) else (
        Util.invalid_argf
          "Rewrite.Lit.add_eq_rule:@ invalid equation type `@[%a@]`@ for rule `%a`"
          Type.pp ty Rule.pp r
      )
    | _ ->
      Util.invalid_argf
        "Rewrite.Lit.add_eq_rule:@ non-equational rule `%a`" Rule.pp r

  (* term rules for this ID, if any *)
  let rules_of_id id: rule Sequence.t =
    begin match as_defined_cst id with
      | None -> Sequence.empty
      | Some dcst -> Cst_.rules_lit_seq dcst
    end

  let rules_of_lit lit: rule Sequence.t = match lit with
    | Literal.Prop (t, _) ->
      begin match T.Classic.view t with
        | T.Classic.App (id, _) -> rules_of_id id
        | _ -> Sequence.empty
      end
    | Literal.Equation _ -> Set.to_seq !eq_rules_
    | _ -> Sequence.empty

  (* find rules that can apply to this literal *)
  let step_lit (lit:Literal.t) =
    rules_of_lit lit
    |> Sequence.find_map
      (fun r ->
         let substs = Literal.matching ~pattern:(r.lit_lhs,1) (lit,0) in
         begin match Sequence.head substs with
           | None -> None
           | Some (subst,tags) -> Some (r, subst, tags)
         end)

  (* try to rewrite this literal, returning a list of list of lits instead *)
  let normalize_clause_ (lits:Literals.t) : _ option =
    let eval_ll renaming subst (l,sc) =
      List.map
        (List.map
           (fun lit -> Literal.apply_subst renaming subst (lit,sc)))
        l
    in
    let step =
      CCArray.findi
        (fun i lit -> match step_lit lit with
           | None -> None
           | Some (rule,subst,tags) ->
             let clauses = rule.lit_rhs in
             Util.debugf ~section 5
               "@[<2>rewrite `@[%a@]`@ :into `@[<v>%a@]`@ :with @[%a@]@ :rule `%a`@]"
               (fun k->k Literal.pp lit
                   (Util.pp_list (Fmt.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp)))
                   clauses Subst.pp subst Rule.pp rule);
             Util.incr_stat stat_lit_rw;
             Some (i, clauses, subst, rule, tags))
        lits
    in
    begin match step with
      | None -> None
      | Some (i, clause_chunks, subst, rule, tags) ->
        let renaming = Subst.Renaming.create () in
        (* remove rewritten literal, replace by [clause_chunks], apply
           substitution (clause_chunks might contain other variables!),
           distribute to get a CNF again *)
        let lits = CCArray.except_idx lits i in
        let lits = Literal.apply_subst_list renaming subst (lits,0) in
        let clause_chunks = eval_ll renaming subst (clause_chunks,1) in
        let clauses =
          List.rev_map
            (fun new_lits -> Array.of_list (new_lits @ lits))
            clause_chunks
        in
        Some (clauses,rule,subst,1,renaming,tags)
    end

  let normalize_clause lits =
    Util.with_prof prof_lit_rw normalize_clause_ lits

  let narrow_lit ?(subst=Unif_subst.empty) ~scope_rules:sc_r (lit,sc_lit) =
    rules_of_lit lit
    |> Sequence.flat_map
      (fun r ->
         Literal.unify ~subst (r.lit_lhs,sc_r) (lit,sc_lit)
         |> Sequence.map (fun (subst,tags) -> r, subst, tags))
end

let pseudo_rule_of_rule (r:rule): pseudo_rule = match r with
  | T_rule r ->
    let id = Term.Rule.head_id r in
    let args = Term.Rule.args r in
    let rhs =
      Term.Rule.rhs r
      |> T.Seq.subterms
      |> Sequence.filter_map
        (fun sub -> match T.Classic.view sub with
           | T.Classic.App (id', args') when ID.equal id' id ->
             Some args'
           | _ -> None)
    in
    id, args, rhs
  | L_rule r ->
    let view_atom id (t:term) = match T.Classic.view t with
      | T.Classic.App (id', args') when ID.equal id' id -> Some args'
      | _ -> None
    in
    let view_lit id (lit:Literal.t) = match lit with
      | Literal.Prop (t, _) -> view_atom id t
      | _ -> None
    in
    let fail() =
      Util.invalid_argf "cannot compute position for rule %a" Lit.Rule.pp r
    in
    begin match Lit.Rule.lhs r with
      | Literal.Prop (t, _) ->
        begin match T.Classic.view t with
          | T.Classic.App (id, args) ->
            (* occurrences of literals with same [id] on RHS *)
            let rhs =
              Lit.Rule.rhs r
              |> Sequence.of_list
              |> Sequence.flat_map Sequence.of_list
              |> Sequence.filter_map (view_lit id)
            in
            id, args, rhs
          | _ -> fail()
        end
      | Literal.True | Literal.False
      | Literal.Equation _ | Literal.Int _ | Literal.Rat _ -> fail()
    end

module Rule = struct
  type t = rule
  let of_term t = T_rule t
  let of_lit t = L_rule t
  let pp = pp_rule

  let to_form ?(ctx=Type.Conv.create()) (r:rule) : TypedSTerm.t =
    begin match r with
      | T_rule r -> Term.Rule.to_form ~ctx r
      | L_rule r -> Lit.Rule.to_form ~ctx r
    end

  let to_form_subst
      ?(ctx=Type.Conv.create()) (sp:Subst.Projection.t) (r:rule) : TypedSTerm.t * _ =
    let module TT = TypedSTerm in
    let {Subst.Projection.renaming;scope=sc;subst} = sp in
    begin match r with
      | T_rule {term_lhs;term_rhs;_} ->
        let lhs = Subst.FO.apply renaming subst (term_lhs,sc) in
        let rhs = Subst.FO.apply renaming subst (term_rhs,sc) in
        let f = Term.Rule.conv_ ~ctx lhs rhs in
        let inst = Subst.Projection.as_inst ~ctx sp (T.vars_prefix_order term_lhs) in
        f, inst
      | L_rule ({lit_lhs;lit_rhs;_} as lit_r) ->
        let lhs = Literal.apply_subst renaming subst (lit_lhs,sc) in
        let rhs =
          List.map (fun l-> Literal.apply_subst_list renaming subst (l,sc)) lit_rhs
        in
        let inst = Subst.Projection.as_inst ~ctx sp (Lit.Rule.vars lit_r) in
        Lit.Rule.conv_ ~ctx lhs rhs, inst
    end

  let pp_zf out r = TypedSTerm.ZF.pp out (to_form r)
  let pp_tptp out r = TypedSTerm.TPTP.pp out (to_form r)

  let pp_in = function
    | Output_format.O_normal -> pp
    | Output_format.O_zf -> pp_zf
    | Output_format.O_tptp -> pp_tptp
    | Output_format.O_none -> (fun _ _ -> ())

  let proof = function
    | T_rule r -> Term.Rule.proof r
    | L_rule r -> Lit.Rule.proof r

  let contains_skolems (t:term): bool =
    T.Seq.symbols t
    |> Sequence.exists ID.is_skolem

  let make_lit ~proof lit_lhs lit_rhs =
    L_rule (Lit.Rule.make ~proof lit_lhs lit_rhs)

  let compare a b = match a, b with
    | T_rule r1, T_rule r2 -> Term.Rule.compare r1 r2
    | L_rule r1, L_rule r2 -> Lit.Rule.compare r1 r2
    | T_rule _, L_rule _ -> -1
    | L_rule _, T_rule _ -> 1

  exception E_p of t

  let res_tc : t Proof.result_tc =
    Proof.Result.make_tc
      ~to_exn:(fun t -> E_p t)
      ~of_exn:(function E_p p -> Some p | _ -> None)
      ~compare ~flavor:(fun _ -> `Def)
      ~pp_in
      ~to_form:(fun ~ctx r -> to_form ~ctx r)
      ~to_form_subst:(fun ~ctx subst r -> to_form_subst ~ctx subst r)
      ()

  let as_proof r =
    Proof.S.mk (proof r) (Proof.Result.make res_tc r)

  let lit_as_proof_parent_subst renaming subst (r,sc) : Proof.parent =
    let proof =
      Proof.S.mk (Lit.Rule.proof r) (Proof.Result.make res_tc (L_rule r))
    in
    Proof.Parent.from_subst renaming (proof,sc) subst

  let set_as_proof_parents (s:Term.Rule_inst_set.t) : Proof.parent list =
    Term.Rule_inst_set.to_seq s
    |> Sequence.map
      (fun (r,subst,sc) ->
         let proof =
           Proof.S.mk (Term.Rule.proof r) (Proof.Result.make res_tc (T_rule r))
         in
         Proof.Parent.from_subst Subst.Renaming.none (proof,sc) subst)
    |> Sequence.to_rev_list
end

let allcst_ : Cst_.t list ref = ref []

module Defined_cst = struct
  include Cst_

  (* check the ID of this rule *)
  let check_id_tr id (r:term_rule): unit =
    if not (ID.equal id (Term.Rule.head_id r)) then (
      Util.invalid_argf
        "Rewrite_term.Defined_cst:@ rule %a@ should have id %a"
        Term.Rule.pp r ID.pp id
    )

  let compute_pos id (s:rule_set) =
    let pos =
      Rule_set.to_seq s
      |> Sequence.map pseudo_rule_of_rule
      |> Sequence.to_rev_list
      |> compute_pos_gen
    in
    Util.debugf ~section 3
      "(@[<2>defined_pos %a@ :pos (@[<hv>%a@])@])"
      (fun k->k ID.pp id (Util.pp_seq Defined_pos.pp) (IArray.to_seq pos));
    pos

  let check_rules id rules =
    Rule_set.iter
      (function
        | T_rule r -> check_id_tr id r
        | _ -> ())
      rules

  (* main builder *)
  let make_ level id ty rules: t =
    check_rules id rules;
    { defined_id=id;
      defined_level=level;
      defined_ty=ty;
      defined_rules=rules;
      defined_positions=lazy (compute_pos id rules);
    }

  let declare ?level id (rules:rule_set): t =
    (* declare that [id] is a defined constant of level [l+1] *)
    Util.debugf ~section 2
      "@[<2>declare %a@ as defined constant@ :rules %a@]"
      (fun k->k ID.pp id pp_rule_set rules);
    let ty =
      if Rule_set.is_empty rules then (
        Util.invalid_argf
          "cannot declare %a as defined constant with empty set of rules"
          ID.pp id;
      );
      begin match Rule_set.choose rules with
        | T_rule s -> Term.Rule.ty s
        | L_rule _ -> Type.prop
      end
    in
    let dcst = make_ level id ty rules in
    ID.set_payload id (Payload_defined_cst dcst);
    CCList.Ref.push allcst_ dcst;
    dcst

  let add_rule (dcst:t) (r:rule): unit =
    begin match r with
      | T_rule r -> check_id_tr dcst.defined_id r;
      | L_rule _ -> ()
    end;
    let rules = Rule_set.add r (rules dcst) in
    dcst.defined_rules <- rules;
    dcst.defined_positions <-
      lazy (compute_pos dcst.defined_id rules); (* update positions *)
    ()

  let add_term_rule (dcst:t) (r:term_rule): unit = add_rule dcst (T_rule r)
  let add_lit_rule (dcst:t) (r:lit_rule): unit = add_rule dcst (L_rule r)

  let add_term_rule_l dcst = List.iter (add_term_rule dcst)
  let add_lit_rule_l dcst = List.iter (add_lit_rule dcst)

  let add_eq_rule = Lit.add_eq_rule
  let add_eq_rule_l = List.iter add_eq_rule

  let declare_or_add id (rule:rule): unit = match as_defined_cst id with
    | Some c ->
      Util.debugf ~section 2
        "@[<2>add rule@ :to %a@ :rule %a@]" (fun k->k ID.pp id pp_rule rule);
      add_rule c rule
    | None ->
      ignore (declare ?level:None id (Rule_set.singleton rule))

  (* make a single rule [proj (C … x_i …) --> x_i] *)
  let mk_rule_proj_ (p:Ind_ty.projector) proof : rule =
    let i = Ind_ty.projector_idx p in
    let id = Ind_ty.projector_id p in
    let cstor = Ind_ty.projector_cstor p in
    let ty_proj = Ind_ty.projector_ty p in
    (* build the variable arguments *)
    let ty_cstor = cstor.Ind_ty.cstor_ty in
    let n_ty_vars, _, _ = Type.open_poly_fun ty_cstor in
    let ty_vars = CCList.init n_ty_vars (fun i -> HVar.make ~ty:Type.tType i) in
    let _, ty_args, _ =
      Type.apply ty_cstor (List.map Type.var ty_vars)
      |> Type.open_poly_fun
    in
    let vars = List.mapi (fun i ty -> HVar.make (i+n_ty_vars) ~ty) ty_args in
    (* the term [cstor … x_i …] *)
    let t =
      T.app_full
        (T.const ~ty:ty_cstor cstor.Ind_ty.cstor_name)
        (List.map Type.var ty_vars)
        (List.map T.var vars)
    in
    let rhs = T.var (List.nth vars i) in
    T_rule (Term.Rule.make ~proof id ty_proj (List.map T.var ty_vars @ [t]) rhs)

  let declare_proj ~proof (p:Ind_ty.projector): unit =
    let p_id = Ind_ty.projector_id p in
    begin match as_defined_cst p_id with
      | Some _ ->
        Util.invalid_argf "cannot declare proj %a, already defined" ID.pp p_id
      | None ->
        let rule = mk_rule_proj_ p proof in
        Util.debugf ~section 3 "(@[declare-proj %a@ :rule %a@])"
          (fun k->k ID.pp p_id Rule.pp rule);
        ignore (declare ?level:None p_id (Rule_set.singleton rule))
    end

  (* make a single rule [C (proj_1 x)…(proj_n x) --> x] *)
  let mk_rule_cstor_ (c:Ind_ty.constructor) proof : rule =
    let c_id = c.Ind_ty.cstor_name in
    let projs = List.map snd c.Ind_ty.cstor_args in
    assert (projs <> []);
    (* make type variables *)
    let c_ty = c.Ind_ty.cstor_ty in
    let n_ty_vars, _, _ = Type.open_poly_fun c_ty in
    let ty_vars = CCList.init n_ty_vars (fun i -> HVar.make ~ty:Type.tType i) in
    (* build LHS *)
    let _, _, ty_x =
      Type.apply c_ty (List.map Type.var ty_vars)
      |> Type.open_poly_fun
    in
    let x = HVar.make ~ty:ty_x 0 in
    let args =
      List.map
        (fun proj ->
           T.app_full
             (T.const ~ty:(Ind_ty.projector_ty proj) (Ind_ty.projector_id proj))
             (List.map Type.var ty_vars)
             [T.var x])
        projs
    in
    let rhs = T.var x in
    T_rule (Term.Rule.make ~proof c_id c_ty (List.map T.var ty_vars @ args) rhs)

  let declare_cstor ~proof (c:Ind_ty.constructor): unit =
    let c_id = c.Ind_ty.cstor_name in
    if not (CCList.is_empty c.Ind_ty.cstor_args) then (
      begin match as_defined_cst c_id with
        | Some _ ->
          Util.invalid_argf "cannot declare cstor %a, already defined" ID.pp c_id
        | None ->
          let rule = mk_rule_cstor_ c proof in
          Util.debugf ~section 3 "(@[declare-cstor %a@ :rule %a@])"
            (fun k->k ID.pp c_id Rule.pp rule);
          ignore (declare ?level:None c_id (Rule_set.singleton rule))
      end
    )
end

let all_cst k = List.iter k !allcst_

let all_rules =
  all_cst
  |> Sequence.flat_map Defined_cst.rules_seq

let () =
  Options.add_opts
    [ "--rw-pos-eqn", Arg.Set allow_pos_eqn_rewrite_, " do rewriting on positive equations";
      "--no-rw-pos-eqn", Arg.Clear allow_pos_eqn_rewrite_, " no rewriting on positive equations";
    ]
