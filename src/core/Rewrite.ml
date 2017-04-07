
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting on Terms} *)

module T = FOTerm
module Fmt = CCFormat

let section = Util.Section.make "rewrite"
let stat_term_rw = Util.mk_stat "rw.steps_term"
let prof_term_rw = Util.mk_profiler "rw.term"
let stat_lit_rw = Util.mk_stat "rw.steps_lit"
let prof_lit_rw = Util.mk_profiler "rw.lit"

type term = FOTerm.t

type term_rule = {
  term_head: ID.t; (* head symbol of LHS *)
  term_args: term list; (* arguments *)
  term_lhs: term; (* [lhs = head args] *)
  term_rhs: term;
}

type lit_rule = {
  lit_lhs: Literal.t;
  lit_rhs: Literal.t list list; (* list of clauses *)
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
  match r1, r2 with
  | T_rule r1, T_rule r2 -> compare_tr r1 r2
  | L_rule r1, L_rule r2 -> compare_lr r1 r2
  | T_rule _, _
  | L_rule _, _ -> CCInt.compare (to_int r1) (to_int r2)

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
  Fmt.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp r.term_lhs T.pp r.term_rhs

let pp_term_rules out (s:term_rule Sequence.t): unit =
  Fmt.(within "{" "}" @@ hvbox @@ Util.pp_seq pp_term_rule) out s

let pp_lit_rule out r =
  let pp_c = CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp) in
  Format.fprintf out "@[<2>@[%a@] ==>@ [@[<v>%a@]]@]"
    Literal.pp r.lit_lhs (Util.pp_list ~sep:"∧" pp_c) r.lit_rhs

let pp_lit_rules out (s:lit_rule Sequence.t): unit =
  Format.fprintf out "{@[<hv>%a@]}" (Util.pp_seq pp_lit_rule) s

let pp_rule out = function
  | T_rule r -> Format.fprintf out "(@[(term) %a@])" pp_term_rule r
  | L_rule l -> Format.fprintf out "(@[(lit) %a@])" pp_lit_rule l

let pp_rule_set out (rs: rule_set): unit =
  Fmt.(within "{" "}" @@ hvbox @@ Util.pp_seq pp_rule) out (Rule_set.to_seq rs)

(** Annotation on IDs that are defined. *)
exception Payload_defined_cst of defined_cst

let as_defined_cst id = match ID.payload id with
  | Payload_defined_cst c -> Some c
  | _ -> None

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
                    if List.length args' > i
                    then Some (List.nth args' i)
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
    let ty r = T.ty r.term_rhs

    let vars r = T.vars (lhs r)
    let vars_l r = vars r |> T.VarSet.to_list

    let make_ head args term_lhs term_rhs =
      { term_head=head; term_args=args; term_lhs; term_rhs }

    (* constant rule [id := rhs] *)
    let make_const id ty rhs =
      let lhs = T.const ~ty id in
      assert (Type.equal (T.ty rhs) (T.ty lhs));
      if not (T.VarSet.is_empty @@ T.vars rhs) then (
        Util.invalid_argf
          "Rule.make_const %a %a:@ invalid rule, RHS contains variables"
          ID.pp id T.pp rhs
      );
      make_ id [] lhs rhs

    (* [id args := rhs] *)
    let make id ty args rhs =
      let lhs = T.app (T.const ~ty id) args in
      assert (Type.equal (T.ty lhs) (T.ty rhs));
      if not (T.VarSet.subset (T.vars rhs) (T.vars lhs)) then (
        Util.invalid_argf
          "Rule.make_const %a %a:@ invalid rule, RHS contains variables"
          ID.pp id T.pp rhs
      );
      make_ id args lhs rhs

    let pp out r = pp_term_rule out r

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

  (* TODO: {b long term}

     use De Bruijn  indices for rewrite rules, with RuleSet.t being a
     decision tree (similar to pattern-matching compilation) on head symbols
     + equality contraints for non-linear rules.

     Use the FOTerm.DB case extensively... *)

  let normalize_term_ max_steps (t:term): term * rule_set =
    assert (max_steps >= 0);
    let set = ref TR_set.empty in
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
            set := TR_set.add r !set;
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
             begin match T.view f with
               | T.Const id ->
                 let find_rule =
                   rules_of_id id
                   |> Sequence.find_map
                     (fun r ->
                        try
                          let subst' =
                            Unif.FO.matching ~pattern:(r.term_lhs,1) (t',0)
                          in
                          Some (r, subst')
                        with Unif.Fail -> None)
                 in
                 begin match find_rule with
                   | None -> k t'
                   | Some (r, subst) ->
                     (* rewrite [t = r.lhs\sigma] into [rhs] (and normalize [rhs],
                        which contain variables bound by [subst]) *)
                     Util.debugf ~section 5
                       "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@ with `@[%a@]`@]"
                       (fun k->k T.pp t' Rule.pp r Subst.pp subst);
                     set := TR_set.add r !set;
                     Util.incr_stat stat_term_rw;
                     decr fuel;
                     (* NOTE: not efficient, will traverse [t'] fully *)
                     let t' = Subst.FO.apply_no_renaming subst (r.term_rhs,1) in
                     reduce t' k
                 end
               | _ -> k t'
             end)
      | T.Var _
      | T.DB _ -> k t
      | T.AppBuiltin (_,[]) -> k t
      | T.AppBuiltin (b,l) ->
        reduce_l l
          (fun l' ->
             let t' = if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l' in
             k t')
    (* reduce list *)
    and reduce_l l k = match l with
      | [] -> k []
      | t :: tail ->
        reduce_l tail
          (fun tail' ->
             reduce t (fun t' -> k (t' :: tail')))
    in
    reduce t (fun t->t, !set)

  let normalize_term ?(max_steps=max_int) (t:term): term * rule_set =
    Util.with_prof prof_term_rw (normalize_term_ max_steps) t

  let normalize_term_fst ?max_steps t = fst (normalize_term ?max_steps t)

  let narrow_term ?(subst=Subst.empty) ~scope_rules:sc_r (t,sc_t): _ Sequence.t =
    begin match T.view t with
      | T.Const _ -> Sequence.empty (* already normal form *)
      | T.App (f, _) ->
        begin match T.view f with
          | T.Const id ->
            (* try to match the rules of [id] *)
            rules_of_id id
            |> Sequence.filter_map
              (fun r ->
                 try Some (r, Unif.FO.unification ~subst (r.term_lhs,sc_r) (t,sc_t))
                 with Unif.Fail -> None)
          | _ -> Sequence.empty
        end
      | T.Var _
      | T.DB _
      | T.AppBuiltin _ -> Sequence.empty
    end
end

module Lit = struct
  type rule = lit_rule

  module Rule = struct
    type t = lit_rule

    let make lit_lhs lit_rhs = {lit_lhs; lit_rhs}

    let lhs c = c.lit_lhs
    let rhs c = c.lit_rhs

    let head_id c = match lhs c with
      | Literal.Prop (t, _) ->
        begin match T.Classic.view t with
          | T.Classic.App (id, _) -> Some id
          | _ -> assert false
        end
      | Literal.Equation _ -> None
      | _ -> assert false

    let is_equational c = match lhs c with
      | Literal.Equation _ -> true
      | _ -> false

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
    | Literal.Equation (t,_,_) ->
      let ty = T.ty t in
      if Type.is_const ty || Type.is_app ty then (
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
           | Some subst -> Some (r, subst)
         end)

  (* try to rewrite this literal, returning a list of list of lits instead *)
  let normalize_clause_ (lits:Literals.t) =
    let eval_ll ~renaming subst (l,sc) =
      List.map
        (List.map
           (fun lit -> Literal.apply_subst ~renaming subst (lit,sc)))
        l
    in
    let step =
      CCArray.findi
        (fun i lit -> match step_lit lit with
           | None -> None
           | Some (rule,subst) ->
             let clauses = rule.lit_rhs in
             Util.debugf ~section 5
               "@[<2>rewrite `@[%a@]`@ :into `@[<v>%a@]`@ :with @[%a@]@ :rule `%a`@]"
               (fun k->k Literal.pp lit
                   (Util.pp_list (CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp)))
                   clauses Subst.pp subst Rule.pp rule);
             Util.incr_stat stat_lit_rw;
             Some (i, clauses, subst))
        lits
    in
    begin match step with
      | None -> None
      | Some (i, clause_chunks, subst) ->
        let renaming = Subst.Renaming.create () in
        (* remove rewritten literal, replace by [clause_chunks], apply
           substitution (clause_chunks might contain other variables!),
           distribute to get a CNF again *)
        let lits = CCArray.except_idx lits i in
        let lits = Literal.apply_subst_list ~renaming subst (lits,0) in
        let clause_chunks = eval_ll ~renaming subst (clause_chunks,1) in
        let clauses =
          List.rev_map
            (fun new_lits -> Array.of_list (new_lits @ lits))
            clause_chunks
        in
        Some clauses
    end

  let normalize_clause lits =
    Util.with_prof prof_lit_rw normalize_clause_ lits

  let narrow_lit ?(subst=Subst.empty) ~scope_rules:sc_r (lit,sc_lit) =
    rules_of_lit lit
    |> Sequence.flat_map
      (fun r ->
         Literal.unify ~subst (r.lit_lhs,sc_r) (lit,sc_lit)
         |> Sequence.map (fun subst -> r, subst))
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

  let make_lit lit_lhs lit_rhs = match lit_lhs, lit_rhs with
    | Literal.Prop (t, true), [[ lit0 ]] ->
      let lhs = match T.Classic.view t with
        | T.Classic.App (id, args) ->
          let ty_id = match T.view t with
            | T.Const _ -> T.ty t
            | T.App (f, _) -> T.ty f
            | _ -> assert false
          in
          Some (id, ty_id, args)
        | _ -> None
      and rhs = match lit0 with
        | Literal.Prop (u, true) -> Some u
        | Literal.True -> Some T.true_
        | Literal.False -> Some T.false_
        | _ -> None
      in
      begin match lhs, rhs with
        | Some (id,ty_id,args), Some rhs ->
          T_rule (Term.Rule.make id ty_id args rhs)
        | _ -> L_rule (Lit.Rule.make lit_lhs lit_rhs)
      end
    | _ -> L_rule (Lit.Rule.make lit_lhs lit_rhs)
end

module Defined_cst = struct
  include Cst_

  (* check the ID of this rule *)
  let check_id_tr id (r:term_rule): unit =
    if not (ID.equal id (Term.Rule.head_id r)) then (
      Util.invalid_argf
        "Rewrite_term.Defined_cst:@ rule %a@ should have id %a"
        Term.Rule.pp r ID.pp id
    )

  let compute_pos (s:rule_set) =
    Rule_set.to_seq s
    |> Sequence.map pseudo_rule_of_rule
    |> Sequence.to_rev_list
    |> compute_pos_gen

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
      defined_positions=lazy (compute_pos rules);
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
    dcst

  let add_rule (dcst:t) (r:rule): unit =
    begin match r with
      | T_rule r -> check_id_tr dcst.defined_id r;
      | L_rule _ -> ()
    end;
    let rules = Rule_set.add r (rules dcst) in
    dcst.defined_rules <- rules;
    dcst.defined_positions <- lazy (compute_pos rules); (* update positions *)
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
end

