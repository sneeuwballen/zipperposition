
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module TI = InnerTerm
module T = Term
module Fmt = CCFormat

module RW = Rewrite

let section = Util.Section.(make "test_prop")
let stat_narrow = Util.mk_stat "test_prop.narrow.calls"
let stat_narrow_fail = Util.mk_stat "test_prop.narrow.fails"
let stat_narrow_ok = Util.mk_stat "test_prop.narrow.ok"
let stat_narrow_step_term = Util.mk_stat "test_prop.narrow.steps_term"
let stat_narrow_step_lit = Util.mk_stat "test_prop.narrow.steps_lit"
let prof_narrow = Util.mk_profiler "test_prop.narrow"

type term = T.t
type lit = Literal.t
type clause = Literals.t
type form = clause list
type var = Type.t HVar.t

type res =
  | R_ok
  | R_fail of Subst.t (* counter-example *)

type 'a t_view =
  | T_Z of Z.t
  | T_Q of Q.t
  | T_bool of bool
  | T_cstor of ID.t * 'a list (* cstor application *)
  | T_app of ID.t * 'a list (* other application *)
  | T_fun_app of 'a * 'a list
  | T_builtin of Builtin.t * 'a list
  | T_fun of Type.t * 'a
  | T_var of var

(* TODO: β reduction *)

let t_view (t:term): term t_view = match T.view t with
  | T.AppBuiltin (Builtin.Int n, []) -> T_Z n
  | T.AppBuiltin (Builtin.Rat n, []) -> T_Q n
  | T.AppBuiltin (Builtin.True, []) -> T_bool true
  | T.AppBuiltin (Builtin.False, []) -> T_bool false
  | T.AppBuiltin (b, l) -> T_builtin (b,l)
  | T.Var v -> T_var v
  | T.Const id when Ind_ty.is_constructor id -> T_cstor (id, [])
  | T.Const id -> T_app (id, [])
  | T.Fun (arg,bod) -> T_fun (arg,bod)
  | T.App (f, l) ->
    begin match T.view f with
      | T.Const id when Ind_ty.is_constructor id -> T_cstor (id, l)
      | T.Const id -> T_app (id, l)
      | _ -> T_fun_app (f,l)
    end
  | T.DB _ -> assert false

let pp_form out (f:form): unit =
  let pp_c = Literals.pp in
  begin match f with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "∧{@[%a@]}" (Util.pp_list ~sep:"," pp_c) f
  end

let normalize_form (f:form): form =
  let module RW = Rewrite in
  let rec simplify c =
    let lit_abs = CCArray.find_idx Literal.is_absurd c in
    begin match lit_abs with
      | None -> c
      | Some (i,_) ->
        let new_c = CCArray.except_idx c i |> Array.of_list in
        simplify new_c
    end
  in
  (* fixpoint of rewriting *)
  let rec normalize_up_to fuel (c:clause): clause Sequence.t =
    assert (fuel>=0);
    if fuel=0 then Sequence.return c
    else normalize_step (fuel-1) c
  and normalize_step fuel c =
    let progress=ref false in
    (* how to normalize a term/lit (with restricted resources) *)
    let rw_term t =
      let t', rules = RW.Term.normalize_term ~max_steps:10 t in
      if not (RW.Term.Rule_inst_set.is_empty rules) then progress := true;
      t'
    in
    let rw_terms c = Literals.map rw_term c
    and rw_clause c = match RW.Lit.normalize_clause c with
      | None -> [c]
      | Some (cs,_,_,_,_,_) ->
        progress := true;
        cs
    and rm_trivial =
      List.filter (fun c -> not (Literals.is_trivial c))
    in
    let cs = c |> rw_terms |> rw_clause |> rm_trivial in
    if !progress
    then normalize_form fuel cs (* normalize each result recursively *)
    else (
      (* done, just simplify *)
      Sequence.of_list cs |> Sequence.map simplify
    )
  and normalize_form fuel (f:form): clause Sequence.t =
    Sequence.of_list f |> Sequence.flat_map (normalize_up_to fuel)
  in
  normalize_form 3 f |> Sequence.to_rev_list

module Narrow : sig
  val default_limit: int
  val check_form: limit:int -> form -> res
end = struct
  let default_limit = 10

  (* pseudo-substitution that is accumulated *)
  type subst_acc = T.t T.VarMap.t

  let subst_of_acc (s:subst_acc): Subst.t =
    T.VarMap.to_list s
    |> List.map (fun (v,t) -> (v,0),(t,1))
    |> Subst.FO.of_list' ?init:None

  let compose renaming (subst:Subst.t) (s1:subst_acc Scoped.t): subst_acc =
    let s1, sc1 = s1 in
    T.VarMap.map
      (fun t -> Subst.FO.apply renaming subst (t,sc1))
      s1

  let form_is_false (f:form): bool = List.exists Literals.is_absurd f

  (* free variables of the form *)
  let vars_of_form (f:form): var list =
    Sequence.of_list f
    |> Sequence.flat_map Literals.Seq.vars
    |> T.VarSet.of_seq |> T.VarSet.to_list

  (* perform term narrowing in [f] *)
  let narrow_term (acc:subst_acc) (f:form): (subst_acc*form) Sequence.t =
    let sc_rule = 1 in
    let sc_c = 0 in
    (* find the various pairs (rule,subst) that can apply *)
    let subst_rule_l =
      Sequence.of_list f
      |> Sequence.flat_map Literals.Seq.terms
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.flat_map
        (fun t -> RW.Term.narrow_term ~scope_rules:sc_rule (t,sc_c))
      |> Sequence.to_rev_list
      |> CCList.sort_uniq
        ~cmp:CCOrd.(pair RW.Term.Rule.compare Unif_subst.compare)
    in
    (* now do one step for each *)
    begin
      Sequence.of_list subst_rule_l
      |> Sequence.map
        (fun (rule,us) ->
           let renaming = Subst.Renaming.create() in
           let subst = Unif_subst.subst us in
           let c_guard = Literals.of_unif_subst renaming us in
           (* evaluate new formula by substituting and evaluating *)
           let f' =
             f
             |> List.map
               (fun lits ->
                  CCArray.append c_guard
                    (Literals.apply_subst renaming subst (lits,sc_c)))
             |> normalize_form
           in
           (* make new formula *)
           Util.incr_stat stat_narrow_step_term;
           Util.debugf ~section 5
             "(@[<2>test_prop.narrow_term@ :from %a@ :to %a@ :rule %a@ :subst %a@])"
             (fun k->k pp_form f pp_form f' RW.Term.Rule.pp rule Subst.pp subst);
           let new_acc = compose renaming subst (acc,sc_c) in
           new_acc, f')
    end

  (* perform lit narrowing in [f] *)
  let narrow_lit (acc:subst_acc) (f:form): (subst_acc*form) Sequence.t =
    let sc_rule = 1 in
    let sc_c = 0 in
    (* find the various pairs (rule,subst) that can apply *)
    let subst_rule_l =
      Sequence.of_list f
      |> Sequence.flat_map Sequence.of_array
      |> Sequence.flat_map
        (fun lit -> RW.Lit.narrow_lit ~scope_rules:sc_rule (lit,sc_c))
      |> Sequence.to_rev_list
      |> CCList.sort_uniq
        ~cmp:CCOrd.(triple RW.Lit.Rule.compare Unif_subst.compare (list compare))
    in
    (* now do one step for each *)
    begin
      Sequence.of_list subst_rule_l
      |> Sequence.map
        (fun (rule,us,_) ->
           let renaming = Subst.Renaming.create() in
           let subst = Unif_subst.subst us in
           let c_guard = Literals.of_unif_subst renaming us in
           (* evaluate new formula by substituting and evaluating *)
           let f' =
             f
             |> List.map
               (fun lits ->
                  CCArray.append c_guard
                    (Literals.apply_subst renaming subst (lits,sc_c)))
             |> normalize_form
           in
           (* make new formula *)
           Util.incr_stat stat_narrow_step_lit;
           Util.debugf ~section 5
             "(@[<2>test_prop.narrow_lit@ :from %a@ :to %a@ :rule %a@ :subst %a@])"
             (fun k->k pp_form f pp_form f' RW.Lit.Rule.pp rule Subst.pp subst);
           let new_acc = compose renaming subst (acc,sc_c) in
           new_acc, f')
    end

  exception Found_unsat of Subst.t

  let check_form ~limit (f:form) =
    Util.incr_stat stat_narrow;
    let q = Queue.create() in
    let acc0 =
      vars_of_form f
      |> List.map (fun v -> v, T.var v)
      |> T.VarMap.of_list
    in
    Queue.push (acc0,f) q;
    let n = ref limit in
    try
      while !n > 0 && not (Queue.is_empty q) do
        decr n;
        let subst, f = Queue.pop q in
        let new_f_l =
          Sequence.append (narrow_term subst f) (narrow_lit subst f)
        in
        Sequence.iter
          (fun (acc,f') ->
             if form_is_false f' then (
               let subst = subst_of_acc acc in
               raise (Found_unsat subst);
             );
             Queue.push (acc,f') q)
          new_f_l;
      done;
      R_ok
    with Found_unsat subst ->
      R_fail subst
end

let default_limit = Narrow.default_limit

let check_form ?(limit=Narrow.default_limit) (f:form): res =
  Util.with_prof prof_narrow (Narrow.check_form ~limit) f

(* [t] head symbol is a function that is not a constructor *)
let starts_with_fun (t:T.t): bool = match T.head t with
  | None -> false
  | Some id -> not (Ind_ty.is_constructor id)
