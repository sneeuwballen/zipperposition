
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module TI = InnerTerm
module T = FOTerm
module Lit = SLiteral
module Fmt = CCFormat
module RT = Rewrite_term

let section = Util.Section.(make "test_prop")
let stat_narrow = Util.mk_stat "test_prop.narrow.calls"
let stat_narrow_fail = Util.mk_stat "test_prop.narrow.fails"
let stat_narrow_ok = Util.mk_stat "test_prop.narrow.ok"
let stat_narrow_step = Util.mk_stat "test_prop.narrow.steps"
let prof_narrow = Util.mk_profiler "test_prop.narrow"

type term = T.t
type lit = term SLiteral.t
type form = lit list list
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
  let pp_lit = SLiteral.pp T.pp in
  let pp_c = Fmt.(within "(" ")" @@ list ~sep:(return "@<1>∨@ ") pp_lit) in
  begin match f with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "∧{@[%a@]}" (Util.pp_list ~sep:"," pp_c) f
  end

module Narrow : sig
  val default_limit: int
  val check_form: limit:int -> form -> res
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

  (* is it impossible for these terms to be equal? check if a cstor-only
     path leads to distinct constructors/constants *)
  let rec cannot_be_eq (t1:term)(t2:term): bool =
    begin match t_view t1, t_view t2 with
      | T_Z z1, T_Z z2 -> not (Z.equal z1 z2)
      | T_Q n1, T_Q n2 -> not (Q.equal n1 n2)
      | T_bool b1, T_bool b2 -> not (b1 = b2)
      | T_cstor (id1,l1), T_cstor (id2,l2) ->
        not (ID.equal id1 id2) ||
        (List.length l1 = List.length l2 &&
         List.exists2 cannot_be_eq l1 l2)
      | T_Z _, _
      | T_Q _, _
      | T_bool _, _
      | T_cstor _, _
      | T_app _, _
      | T_fun_app _, _
      | T_builtin _, _
      | T_var _, _ -> false
    end

  (* is the literal [false]? *)
  let lit_is_false lit: bool = match lit with
    | Lit.Atom (t, true) -> T.equal t T.false_
    | Lit.Atom (t, false) -> T.equal t T.true_
    | Lit.False -> true
    | Lit.True -> false
    | Lit.Neq (a,b) -> T.equal a b
    | Lit.Eq (a, b) -> cannot_be_eq a b

  let form_is_false (f:form): bool =
    List.exists (List.for_all lit_is_false) f

  (* free variables of the form *)
  let vars_of_form (f:form): var list =
    Sequence.of_list f
    |> Sequence.flat_map Sequence.of_list
    |> Sequence.flat_map SLiteral.to_seq
    |> Sequence.flat_map T.Seq.vars
    |> T.VarSet.of_seq |> T.VarSet.to_list

  (* apply substitution to literal *)
  let subst_lit ~renaming (subst:Subst.t)(lit:lit Scoped.t): lit =
    let lit, sc = lit in
    Lit.map lit ~f:(fun t -> Subst.FO.apply ~renaming subst (t,sc))

  let normalize_form (f:form): form =
    List.map (List.map (Lit.map ~f:RT.normalize_term_fst)) f

  (* perform term narrowing in [f] *)
  let narrow_term (acc:subst_acc) (f:form): (subst_acc*form) list =
    let sc_rule = 1 in
    let sc_c = 0 in
    (* find the various pairs (rule,subst) that can apply *)
    let subst_rule_l =
      Sequence.of_list f
      |> Sequence.flat_map Sequence.of_list
      |> Sequence.flat_map Lit.to_seq
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.flat_map
        (fun t -> RT.narrow_term ~scope_rules:sc_rule (t,sc_c))
      |> Sequence.to_rev_list
      |> CCList.sort_uniq ~cmp:(CCOrd.pair RT.Rule.compare Subst.compare)
    in
    (* now do one step for each *)
    List.rev_map
      (fun (rule,subst) ->
         let renaming = Subst.Renaming.create() in
         (* evaluate new formula by substituting and evaluating *)
         let f' =
           List.map
             (List.map (fun lit -> subst_lit ~renaming subst (lit,sc_c))) f
           |> normalize_form
         in
         (* make new formula *)
         Util.incr_stat stat_narrow_step;
         Util.debugf ~section 5
           "(@[<2>test_prop.narrow@ :from %a@ :to %a@ :rule %a@ :subst %a@])"
           (fun k->k pp_form f pp_form f' RT.Rule.pp rule Subst.pp subst);
         let new_acc = compose ~renaming subst (acc,sc_c) in
         new_acc, f')
      subst_rule_l

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
        let new_f_l = narrow_term subst f in
        List.iter
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
