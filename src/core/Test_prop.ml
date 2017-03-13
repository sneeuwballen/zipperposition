
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module TI = InnerTerm
module T = FOTerm
module Lit = SLiteral
module Fmt = CCFormat
module RT = Rewrite_term

let section = Util.Section.(make "test_prop")
let stat_small_check = Util.mk_stat "small_check.calls"
let stat_small_check_fail = Util.mk_stat "small_check.fails"
let stat_small_check_ok = Util.mk_stat "small_check.ok"
let prof_small_check = Util.mk_profiler "small_check"

type term = T.t
type lit = term SLiteral.t
type form = lit list list
type var = Type.t HVar.t

type res =
  | R_ok
  | R_fail of Subst.t (* counter-example *)

(* default depth for small check *)
let default_depth = 3

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

(* does the literal evaluate to [false]? *)
let lit_is_false ~renaming (subst:Subst.t) (lit:lit Scoped.t): bool =
  let lit, sc = lit in
  let lit = Lit.map lit
      ~f:(fun t ->
        let t = Subst.FO.apply ~renaming subst (t,sc) in
        RT.normalize_term_fst t)
  in
  begin match lit with
    | Lit.Atom (t, true) -> T.equal t T.false_
    | Lit.Atom (t, false) -> T.equal t T.true_
    | Lit.False -> true
    | Lit.True -> false
    | Lit.Neq (a,b) -> T.equal a b
    | Lit.Eq (a, b) -> cannot_be_eq a b
  end

let pp_form out (f:form): unit =
  let pp_lit = SLiteral.pp T.pp in
  let pp_c = Fmt.(within "(" ")" @@ list ~sep:(return "@<1>∨@ ") pp_lit) in
  begin match f with
    | [c] -> pp_c out c
    | _ -> Fmt.fprintf out "∧{@[%a@]}" (Util.pp_list ~sep:"," pp_c) f
  end

(* is the formula false under [subst]? *)
let form_is_false ~renaming (subst:Subst.t) (f:form Scoped.t): bool =
  let clauses, sc = f in
  List.exists
    (fun c ->
       List.for_all (fun lit -> lit_is_false ~renaming subst (lit,sc)) c)
    clauses

(* free variables of the form *)
let vars_of_form (f:form): var list =
  Sequence.of_list f
  |> Sequence.flat_map Sequence.of_list
  |> Sequence.flat_map SLiteral.to_seq
  |> Sequence.flat_map T.Seq.vars
  |> T.VarSet.of_seq |> T.VarSet.to_list

(* check lemma on small instances. It returns [true] iff none of the
   instances reduces to [false] *)
let small_check_ ~max_depth (f:form): res =
  let sc = 0 in
  let subst_add subst v t =
    Subst.FO.bind subst ((v:Type.t HVar.t:>TI.t HVar.t),sc) (t,sc)
  and subst_add_ty subst v ty =
    Subst.Ty.bind subst ((v:Type.t HVar.t:>TI.t HVar.t),sc) (ty,sc)
  and subst_mem subst v =
    Subst.mem subst ((v:Type.t HVar.t:>TI.t HVar.t),sc)
  and subst_apply_ty subst ty =
    Subst.Ty.apply_no_renaming subst (ty,sc)
  in
  (* generate substitutions on [vars] up to given [depth] *)
  let gen_instances vars: Subst.t Sequence.t =
    (* recurse: find an exntension of [subst] that maps each
       of [vars] to a ground constructor term.
       [offset] is the offset for allocating new variables
       [vars] is the list of variables to instantiate along with their
         depth
    *)
    let rec aux offset (subst:Subst.t) (vars:(var*int) list) = match vars with
      | [] -> Sequence.return subst
      | (v,depth) :: vars' when subst_mem subst v ->
        (* ignore bound variables *)
        assert (depth <= max_depth);
        aux offset subst vars'
      | (v,depth) :: vars_tail ->
        assert (depth <= max_depth);
        begin match Ind_ty.as_inductive_type (HVar.ty v) with
          | None when Type.equal (HVar.ty v) Type.prop ->
            (* try [true] and [false] *)
            Sequence.of_list [T.true_; T.false_]
            |> Sequence.flat_map
              (fun b ->
                 let subst = subst_add subst v b in
                 aux offset subst vars_tail)
          | None -> aux offset subst vars_tail (* ignore [v] *)
          | Some ({ Ind_ty.ty_constructors; ty_vars; _ }, ind_ty_args)
            when depth < max_depth ->
            (* not yet at max depth, try each constructors *)
            assert (List.length ty_vars = List.length ind_ty_args);
            let ind_ty_args' = List.map (subst_apply_ty subst) ind_ty_args in
            (* try to replace [v] by each constructor *)
            Sequence.of_list ty_constructors
            |> Sequence.flat_map
              (fun {Ind_ty.cstor_ty=c_ty; cstor_name=c_id} ->
                 let n, _, _ = Type.open_poly_fun c_ty in
                 assert (n = List.length ty_vars);
                 let c_ty_args, _ =
                   Type.apply c_ty ind_ty_args'
                   |> Type.open_fun
                 in
                 if depth=max_depth && c_ty_args <> []
                 then Sequence.empty (* fail *)
                 else (
                   (* fresh variables as arguments to the constructor *)
                   let sub_vars =
                     List.mapi
                       (fun i ty' -> HVar.make ~ty:ty' (i+offset))
                       c_ty_args
                   in
                   let t =
                     T.app_full
                       (T.const ~ty:c_ty c_id)
                       ind_ty_args'
                       (List.map T.var sub_vars)
                   in
                   let subst = subst_add subst v t in
                   (* instantiate the other variables, as well as the
                      new [sub_vars] that are a step deeper *)
                   aux (offset+List.length c_ty_args) subst
                     (vars_tail @ List.map (fun v->v, depth+1) sub_vars)
                 )
              )
          | Some _ ->
            Sequence.empty (* too deep *)
        end
    in
    (* initial subst: replace type variables by [prop], easy to test *)
    let ty_vars = List.filter (fun v -> Type.is_tType (HVar.ty v)) vars in
    let subst =
      List.fold_left
        (fun subst v -> subst_add_ty subst v Type.prop)
        Subst.empty
        ty_vars
    in
    (* offset to allocate new variables without collision *)
    let offset = 1 + T.Seq.max_var (Sequence.of_list vars) in
    aux offset subst (List.map (fun v->v,0) vars)
  in
  Util.debugf ~section 3 "@[<hv2>small_check@ :form @[%a@]@]" (fun k->k pp_form f);
  Util.incr_stat stat_small_check;
  (* try to find a substitution which falsifies the formula *)
  let subst_opt =
    let vars = vars_of_form f in
    let renaming = Subst.Renaming.create() in
    gen_instances vars
    |> Sequence.find_pred
      (fun subst ->
         Subst.Renaming.clear renaming;
         form_is_false ~renaming subst (f,sc))
  in
  begin match subst_opt with
    | None ->
      Util.incr_stat stat_small_check_ok;
      R_ok (* could not find a counter-ex *)
    | Some subst ->
      Util.debugf ~section 5
        "@[<hv2>... counter-example: %a@]" (fun k->k Subst.pp subst);
      Util.incr_stat stat_small_check_fail;
      R_fail subst
  end

let small_check ?depth:(max_depth=default_depth) (f:form): res =
  Util.with_prof prof_small_check (small_check_ ~max_depth) f

(* [t] head symbol is a function that is not a constructor *)
let starts_with_fun (t:T.t): bool = match T.head t with
  | None -> false
  | Some id -> not (Ind_ty.is_constructor id)
