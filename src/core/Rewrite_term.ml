
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting on Terms} *)

module T = FOTerm
module Fmt = CCFormat

type term = FOTerm.t

type rule = {
  rule_head: ID.t; (* head symbol of LHS *)
  rule_args: term list; (* arguments *)
  rule_lhs: term; (* [lhs = head args] *)
  rule_rhs: term;
}

let compare_rule r1 r2 =
  CCOrd.(T.compare r1.rule_lhs r2.rule_lhs
    <?> (T.compare, r1.rule_rhs, r2.rule_rhs))

module R_set = CCSet.Make(struct
    type t = rule
    let compare = compare_rule
  end)

type rule_set = R_set.t

type defined_position =
  | Pos_active
  | Pos_invariant
  | Pos_accumulator

type defined_positions = defined_position IArray.t

type defined_cst = {
  defined_id: ID.t;
  defined_ty: Type.t;
  defined_level: int option;
  mutable defined_rules: rule_set;
  (* set of rewrite rules.
      invariant: all these rules have [rule_head = defined_id] *)
  mutable defined_positions: defined_positions lazy_t; (* metadata on positions *)
}

(** Annotation on IDs that are defined. *)
exception Payload_defined_cst of defined_cst

let section = Util.Section.make "rewrite_term"
let stat_term_rw = Util.mk_stat "rw.steps_term"
let prof_term_rw = Util.mk_profiler "rw.term"

let pp_defined_position out = function
  | Pos_active -> Fmt.string out "active"
  | Pos_invariant -> Fmt.string out "invariant"
  | Pos_accumulator -> Fmt.string out "accumulator"

let pp_defined_positions out (a:defined_positions) =
  Fmt.(within "[" "]" @@ hvbox @@
    seq @@ pair ~sep:(return ":") int pp_defined_position)
    out (IArray.to_seqi a)

module Rule = struct
  type t = rule

  let lhs r = r.rule_lhs
  let rhs r = r.rule_rhs
  let head_id r = r.rule_head
  let args r = r.rule_args

  let vars r = T.vars (lhs r)
  let vars_l r = vars r |> T.VarSet.to_list

  let make_ head args rule_lhs rule_rhs =
    { rule_head=head; rule_args=args; rule_lhs; rule_rhs }

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

  let pp out r =
    Fmt.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp (lhs r) T.pp (rhs r)

  let compare = compare_rule
  let hash r = Hash.combine2 (T.hash @@ lhs r) (T.hash @@ rhs r)
  let equal r1 r2 = compare r1 r2 = 0
  let to_string = Fmt.to_string pp
end

let pp_rule_seq out (s:rule Sequence.t): unit =
  Fmt.(within "{" "}" @@ hvbox @@ Util.pp_seq Rule.pp) out s
let pp_rule_set out (s:rule_set) = pp_rule_seq out (R_set.to_seq s)

(* compute role of positions *)
let compute_positions (s:rule_set): defined_positions =
  (* check that all rules have the same ID and number of args *)
  assert (not @@ R_set.is_empty s);
  (* ID, number of arguments *)
  let id, n =
    let some_r = R_set.choose s in
    Rule.head_id some_r, List.length (Rule.args some_r)
  in
  assert (s |> R_set.for_all (fun r ->
      ID.equal id (Rule.head_id r) &&
      List.length (Rule.args r) = n));
  (* now compute position roles *)
  let pos = Array.make n Pos_invariant in
  begin
    R_set.to_seq s
    |> Sequence.flat_map
      (fun r ->
         let l = Rule.args r in
         fun yield -> List.iteri (fun i sub -> yield (r,i,sub)) l)
    |> Sequence.iter
      (fun (r,i,arg) ->
         begin match T.view arg with
           | T.Var x ->
             (* if all occurrences of [r.id] on the RHS also have [x] at this
                position, the position stays invariant, otherwise
                it is an accumulator *)
             let is_invariant =
               T.Seq.subterms (Rule.rhs r)
               |> Sequence.filter_map
                 (fun sub -> match T.Classic.view sub with
                    | T.Classic.App (id', args')
                      when ID.equal id' id && List.length args' > i ->
                      (* [i]-th argument of all subterms of [rhs] that have head [id] *)
                      Some (List.nth args' i)
                    | _ -> None)
               |> Sequence.for_all
                 (fun sub -> match T.view sub with
                    | T.Var y -> HVar.equal Type.equal x y
                    | _ -> false)
             in
             (* position is accumulator *)
             if not is_invariant && pos.(i) = Pos_invariant then (
               pos.(i) <- Pos_accumulator;
             );
           | _ ->
             (* pattern, consider this as input *)
             pos.(i) <- Pos_active
         end)
  end;
  IArray.of_array_unsafe pos

module Defined_cst = struct
  type t = defined_cst

  let rules t = t.defined_rules
  let rules_seq t : rule Sequence.t = R_set.to_seq t.defined_rules
  let defined_positions t = Lazy.force t.defined_positions
  let ty t = t.defined_ty
  let level t = CCOpt.get_or ~default:0 t.defined_level

  let pp out (t:t): unit =
    Fmt.fprintf out "(@[defined_id@ :ty %a @ :rules %a@ :positions %a@])"
      Type.pp (ty t) pp_rule_seq (rules_seq t)
      pp_defined_positions (defined_positions t)

  let to_string = Fmt.to_string pp

  (* check the ID of this rule *)
  let check_id id (r:rule): unit =
    if not (ID.equal id (Rule.head_id r)) then (
      Util.invalid_argf
        "Rewrite_term.Defined_cst:@ rule %a@ should have id %a"
        Rule.pp r ID.pp id
    )

  (* main builder *)
  let make level id ty rules: t =
    List.iter (check_id id) rules;
    let rules = R_set.of_list rules in
    { defined_id=id;
      defined_level=level;
      defined_ty=ty;
      defined_rules=rules;
      defined_positions=lazy (compute_positions rules);
    }

  let add_rule (dcst:t) (r:rule): unit =
    check_id dcst.defined_id r;
    let rules = R_set.add r (rules dcst) in
    dcst.defined_rules <- rules;
    dcst.defined_positions <- lazy (compute_positions rules); (* update positions *)
    ()

  let add_rule_l dcst = List.iter (add_rule dcst)
end

let as_defined_cst id = match ID.payload id with
  | Payload_defined_cst c -> Some c
  | _ -> None

let is_defined_cst id = CCOpt.is_some (as_defined_cst id)

let declare_defined_cst ?level id rules: defined_cst =
  (* declare that [id] is a defined constant of level [l+1] *)
  Util.debugf ~section 2
    "@[<2>declare %a@ as defined constant@ :rules %a@]"
    (fun k->k ID.pp id pp_rule_seq (Sequence.of_list rules));
  let ty = match rules with
    | [] ->
      Util.invalid_argf
        "cannot declare %a as defined constant with empty set of rules"
        ID.pp id;
    | r :: _ -> T.ty (Rule.lhs r)
  in
  let dcst = Defined_cst.make level id ty rules in
  ID.set_payload id (Payload_defined_cst dcst);
  dcst

let add_rule = Defined_cst.add_rule
let add_rule_l = Defined_cst.add_rule_l

let declare_cst_or_add id rule: unit = match as_defined_cst id with
  | Some c ->
    Util.debugf ~section 2
      "@[<2>add rule@ :to %a@ :rule %a@]" (fun k->k ID.pp id Rule.pp rule);
    add_rule c rule
  | None ->
    ignore (declare_defined_cst ?level:None id [rule])

(** {2 Rewriting and Narrowing} *)

let rules_of_id id: rule Sequence.t =
  begin match as_defined_cst id with
    | None -> Sequence.empty
    | Some dcst -> Defined_cst.rules_seq dcst
  end

(* TODO: {b long term}

   use De Bruijn  indices for rewrite rules, with RuleSet.t being a
   decision tree (similar to pattern-matching compilation) on head symbols
   + equality contraints for non-linear rules.

   Use the FOTerm.DB case extensively... *)

let normalize_term_ (t:term): term * rule_set =
  let set = ref R_set.empty in
  (* compute normal form of subterm. Tail-recursive.
     @param k the continuation
     @return [t'] where [t'] is the normal form of [t] *)
  let rec reduce t k = match T.view t with
    | T.Const id ->
      (* pick a constant rule *)
      begin match rules_of_id id |> Sequence.head with
        | None -> k t
        | Some r ->
          assert (T.is_const r.rule_lhs && T.equal t r.rule_lhs);
          (* reduce [rhs], but no variable can be bound *)
          set := R_set.add r !set;
          Util.incr_stat stat_term_rw;
          Util.debugf ~section 5
            "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@]"
            (fun k->k T.pp t Rule.pp r);
          reduce r.rule_rhs k
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
                          Unif.FO.matching ~pattern:(r.rule_lhs,1) (t',0)
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
                   set := R_set.add r !set;
                   Util.incr_stat stat_term_rw;
                   (* NOTE: not efficient, will traverse [t'] fully *)
                   let t' = Subst.FO.apply_no_renaming subst (r.rule_rhs,1) in
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

let normalize_term (t:term): term * rule_set =
  Util.with_prof prof_term_rw normalize_term_ t

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
               try Some (r, Unif.FO.unification ~subst (r.rule_lhs,sc_r) (t,sc_t))
               with Unif.Fail -> None)
        | _ -> Sequence.empty
      end
    | T.Var _
    | T.DB _
    | T.AppBuiltin _ -> Sequence.empty
  end
