
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module TI = InnerTerm
module T = FOTerm
module Fmt = CCFormat

module RT = Rewrite_term
module RL = Rewrite_lit

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
  | T_var of var

let t_view (t:term): term t_view = match T.view t with
  | T.AppBuiltin (Builtin.Int n, []) -> T_Z n
  | T.AppBuiltin (Builtin.Rat n, []) -> T_Q n
  | T.AppBuiltin (Builtin.True, []) -> T_bool true
  | T.AppBuiltin (Builtin.False, []) -> T_bool false
  | T.AppBuiltin (b, l) -> T_builtin (b,l)
  | T.Var v -> T_var v
  | T.Const id when Ind_ty.is_constructor id -> T_cstor (id, [])
  | T.Const id -> T_app (id, [])
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

module Narrow : sig
  val default_limit: int
  val check_form: limit:int -> RL.Set.t -> form -> res
end = struct
  let default_limit = 50

  (* pseudo-substitution that is accumulated *)
  type subst_acc = T.t T.VarMap.t

  let subst_of_acc (s:subst_acc): Subst.t =
    T.VarMap.to_list s
    |> List.map (fun (v,t) -> (v,0),(t,1))
    |> Subst.FO.of_list' ?init:None

  let compose (subst:Subst.t) ~renaming (s1:subst_acc Scoped.t): subst_acc =
    let s1, sc1 = s1 in
    T.VarMap.map
      (fun t -> Subst.FO.apply ~renaming subst (t,sc1))
      s1

  let form_is_false (f:form): bool = List.exists Literals.is_absurd f

  (* free variables of the form *)
  let vars_of_form (f:form): var list =
    Sequence.of_list f
    |> Sequence.flat_map Literals.Seq.vars
    |> T.VarSet.of_seq |> T.VarSet.to_list

  let normalize_form (rules:RL.Set.t) (f:form): form =
    (* fixpoint of rewriting *)
    let rec normalize (c:clause): clause Sequence.t =
      let progress=ref false in
      (* how to normalize a term/lit *)
      let rw_term t =
        let t', rules = RT.normalize_term t in
        if not (RT.R_set.is_empty rules) then progress := true;
        t'
      in
      let rw_terms c = Literals.map rw_term c
      and rw_clause c = match RL.normalize_clause rules c with
        | None -> [c]
        | Some cs ->
          progress := true;
          cs
      in
      let cs = c |> rw_terms |> rw_clause in
      if !progress
      then normalize_form cs (* normalize each result recursively *)
      else Sequence.of_list cs (* done *)
    and normalize_form (f:form): clause Sequence.t =
      Sequence.of_list f |> Sequence.flat_map normalize
    in
    normalize_form f |> Sequence.to_rev_list

  (* perform term narrowing in [f] *)
  let narrow_term (rules:RL.Set.t) (acc:subst_acc) (f:form): (subst_acc*form) Sequence.t =
    let sc_rule = 1 in
    let sc_c = 0 in
    (* find the various pairs (rule,subst) that can apply *)
    let subst_rule_l =
      Sequence.of_list f
      |> Sequence.flat_map Literals.Seq.terms
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.flat_map
        (fun t -> RT.narrow_term ~scope_rules:sc_rule (t,sc_c))
      |> Sequence.to_rev_list
      |> CCList.sort_uniq ~cmp:(CCOrd.pair RT.Rule.compare Subst.compare)
    in
    (* now do one step for each *)
    begin
      Sequence.of_list subst_rule_l
      |> Sequence.map
        (fun (rule,subst) ->
           let renaming = Subst.Renaming.create() in
           (* evaluate new formula by substituting and evaluating *)
           let f' =
             List.map
               (fun lits -> Literals.apply_subst ~renaming subst (lits,sc_c)) f
             |> normalize_form rules
           in
           (* make new formula *)
           Util.incr_stat stat_narrow_step_term;
           Util.debugf ~section 5
             "(@[<2>test_prop.narrow_term@ :from %a@ :to %a@ :rule %a@ :subst %a@])"
             (fun k->k pp_form f pp_form f' RT.Rule.pp rule Subst.pp subst);
           let new_acc = compose ~renaming subst (acc,sc_c) in
           new_acc, f')
    end

  (* perform lit narrowing in [f] *)
  let narrow_lit (rules:RL.Set.t) (acc:subst_acc) (f:form): (subst_acc*form) Sequence.t =
    let sc_rule = 1 in
    let sc_c = 0 in
    (* find the various pairs (rule,subst) that can apply *)
    let subst_rule_l =
      Sequence.of_list f
      |> Sequence.flat_map Sequence.of_array
      |> Sequence.flat_map
        (fun lit -> RL.narrow_lit (rules,sc_rule) (lit,sc_c))
      |> Sequence.to_rev_list
      |> CCList.sort_uniq ~cmp:(CCOrd.pair RL.Rule.compare Subst.compare)
    in
    (* now do one step for each *)
    begin
      Sequence.of_list subst_rule_l
      |> Sequence.map
        (fun (rule,subst) ->
           let renaming = Subst.Renaming.create() in
           (* evaluate new formula by substituting and evaluating *)
           let f' =
             List.map
               (fun lits -> Literals.apply_subst ~renaming subst (lits,sc_c)) f
             |> normalize_form rules
           in
           (* make new formula *)
           Util.incr_stat stat_narrow_step_lit;
           Util.debugf ~section 5
             "(@[<2>test_prop.narrow_lit@ :from %a@ :to %a@ :rule %a@ :subst %a@])"
             (fun k->k pp_form f pp_form f' RL.Rule.pp rule Subst.pp subst);
           let new_acc = compose ~renaming subst (acc,sc_c) in
           new_acc, f')
    end

  exception Found_unsat of Subst.t

  let check_form ~limit (rules:RL.Set.t) (f:form) =
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
          Sequence.append
            (narrow_term rules subst f)
            (narrow_lit rules subst f)
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

let check_form ?(limit=Narrow.default_limit) (rules:RL.Set.t) (f:form): res =
  Util.with_prof prof_narrow (Narrow.check_form rules ~limit) f

(* [t] head symbol is a function that is not a constructor *)
let starts_with_fun (t:T.t): bool = match T.head t with
  | None -> false
  | Some id -> not (Ind_ty.is_constructor id)
