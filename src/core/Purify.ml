
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Purification} *)

module T = Term
module Fmt = CCFormat

type term = T.t

let section = Util.Section.make "purify"

let type_is_purifiable (ty:Type.t): bool = match Type.view ty with
  | Type.Forall _ | Type.Fun _ -> true
  | Type.Builtin (Type.Int | Type.Rat) -> true
  | _ -> false

let var_is_purifiable v: bool = type_is_purifiable (HVar.ty v)

let is_value (t:term): bool = match T.view t with
  | T.AppBuiltin (b, []) when Builtin.is_numeric b ->
    true (* do not purify numeric constants *)
  | T.DB _ | T.Var _ -> true
  | T.AppBuiltin _ | T.Const _ | T.App _ ->
    not (type_is_purifiable @@ T.ty t)

type context =
  | C_root
  | C_under_uninterpreted
  | C_under_builtin
  | C_under_purifiable

let pp_context out = function
  | C_root -> Fmt.string out "root"
  | C_under_uninterpreted -> Fmt.string out "under-uninterpreted"
  | C_under_builtin -> Fmt.string out "under-builtin"
  | C_under_purifiable -> Fmt.string out "under-purifiable"

(* replace arith subterms with fresh variable + constraint *)
let purify (lits:Literals.t) =
  (* set of new literals *)
  let new_lits = ref [] in
  let cache_ = T.Tbl.create 8 in  (* cache for term->var *)
  let add_lit_ lit = new_lits := lit :: !new_lits in
  (* index of the next fresh variable *)
  let varidx =
    Literals.Seq.terms lits
    |> Sequence.flat_map T.Seq.vars
    |> T.Seq.max_var |> succ
    |> CCRef.create
  in
  (* replace term by a fresh var + constraint; return the variable *)
  let extract_into_var t =
    try T.Tbl.find cache_ t
    with Not_found ->
      (* [v]: fresh var that will replace [t] *)
      let ty = T.ty t in
      let v = T.var_of_int ~ty (CCRef.get_then_incr varidx) in
      let lit = Literal.mk_neq v t in
      add_lit_ lit;
      v
  (* should we purify the term t? Yes if it's not a value,
     and if it's not a HO term in a HO constraint *)
  and should_purify ~in_constr ~ctx t =
    let ty_t = T.ty t in
    ctx = C_under_uninterpreted &&
    type_is_purifiable ty_t &&
    not (is_value t) &&
    (not in_constr || not (Type.needs_args ty_t))
  in
  let purify_sub ~ctx t =
    Util.debugf ~section 5
      "(@[<2>must_purify@ :term `@[%a@]`@ :ctx %a@])"
      (fun k->k T.pp t pp_context ctx);
    extract_into_var t
  in
  (* purify a term (adding constraints to the list).
     @param in_constr if true, we are in a HO constraint, do not purify HO stuff
  *)
  let rec purify_term ~in_constr ~ctx t = match T.view t with
    | T.DB _
    | T.Var _ -> t
    | T.Const _ | T.AppBuiltin (_, []) ->
      if should_purify ~in_constr ~ctx t
      then purify_sub ~ctx t
      else t
    | T.AppBuiltin (b, l) ->
      let t =
        let ctx_args = C_under_builtin in
        T.app_builtin ~ty:(T.ty t) b
          (List.map (purify_term ~in_constr ~ctx:ctx_args) l)
      in
      if should_purify ~in_constr ~ctx t
      then purify_sub ~ctx t
      else t
    | T.App (f, l) ->
      let t =
        let ctx_args = match T.view f with
          | T.Var _ -> C_under_purifiable
          | _ -> C_under_uninterpreted
        in
        T.app
          (purify_term ~in_constr ~ctx:C_root f)
          (List.map (purify_term ~in_constr ~ctx:ctx_args) l)
      in
      if should_purify ~in_constr ~ctx t
      then purify_sub ~ctx t
      else t
  in
  let purify_lit lit = match lit with
    | Literal.Equation (t, u, false) ->
      let is_ho_t = T.is_ho_app t in
      let is_ho_u = T.is_ho_app u in
      Literal.mk_neq
        (purify_term ~in_constr:is_ho_t ~ctx:C_root t)
        (purify_term ~in_constr:is_ho_u ~ctx:C_root u)
    | _ ->
      Literal.map (purify_term ~in_constr:false ~ctx:C_root) lit
  in
  (* try to purify *)
  let lits' = Array.map purify_lit lits in
  begin match !new_lits with
    | [] -> None
    | _::_ ->
      (* replace! *)
      let all_lits = !new_lits @ (Array.to_list lits') in
      Some (Array.of_list all_lits)
  end

let is_shielded var (lits:Literals.t) : bool =
  let rec shielded_by_term ~root t = match T.view t with
    | T.Var v' when HVar.equal Type.equal v' var -> not root
    | T.Var _
    | T.DB _
    | T.Const _ -> false
    | T.AppBuiltin (_, l) ->
      List.exists (shielded_by_term ~root:false) l
    | T.App (f, l) ->
      begin match T.view f with
        | T.Var v' when HVar.equal Type.equal var v' -> not root
        | T.Var _
        | T.Const _ ->
          List.exists (shielded_by_term ~root:false) l
        | T.DB _ | T.AppBuiltin _ | T.App _ -> assert false
      end
  in
  (* is there a term, directly under a literal, that shields the variable? *)
  begin
    lits
    |> Literals.Seq.terms
    |> Sequence.exists (shielded_by_term ~root:true)
  end

let unshielded_vars lits: _ list =
  Literals.vars lits
  |> List.filter
    (fun var ->
       type_is_purifiable (HVar.ty var) &&
       not (is_shielded var lits))
