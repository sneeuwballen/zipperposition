
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

let is_value (t:term): bool = match T.view t with
  | T.AppBuiltin (b, []) when Builtin.is_numeric b ->
    true (* do not purify numeric constants *)
  | T.DB _ | T.Var _ -> true
  | T.AppBuiltin _ | T.Const _ | T.App _ ->
    not (type_is_purifiable @@ T.ty t)

type context =
  | C_root
  | C_under_uninterpreted
  | C_under_purifiable

let pp_context out = function
  | C_root -> Fmt.string out "root"
  | C_under_uninterpreted -> Fmt.string out "under-uninterpreted"
  | C_under_purifiable -> Fmt.string out "under-purifiable"

(* replace arith subterms with fresh variable + constraint *)
let purify (lits:Literals.t) =
  (* set of new literals *)
  let new_lits = ref [] in
  let cache_ = T.Tbl.create 16 in  (* cache for term->var *)
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
  (* should we purify the term t? Yes if it's not a value *)
  and should_purify ~ctx t =
    ctx = C_under_uninterpreted &&
    type_is_purifiable (T.ty t) &&
    not (is_value t)
  in
  let purify_sub ~ctx t =
    Util.debugf ~section 5
      "(@[<2>must_purify@ :term `@[%a@]`@ :ctx %a@])"
      (fun k->k T.pp t pp_context ctx);
    extract_into_var t
  in
  (* purify a term (adding constraints to the list).  *)
  let rec purify_term ~ctx t = match T.view t with
    | T.DB _
    | T.Var _ -> t
    | T.Const _ | T.AppBuiltin (_, []) ->
      if should_purify ~ctx t
      then purify_sub ~ctx t
      else t
    | T.AppBuiltin (b, l) ->
      let t =
        let ctx_args =
          if type_is_purifiable (T.ty t)
          then C_under_purifiable
          else C_under_uninterpreted
        in
        T.app_builtin ~ty:(T.ty t) b
          (List.map (purify_term ~ctx:ctx_args) l)
      in
      if should_purify ~ctx t
      then purify_sub ~ctx t
      else t
    | T.App (f, l) ->
      let t =
        let ctx_args = match T.view f with
          | T.Var _ -> C_root
          | _ -> C_under_uninterpreted
        in
        T.app
          (purify_term ~ctx:C_root f)
          (List.map (purify_term ~ctx:ctx_args) l)
      in
      if should_purify ~ctx t
      then purify_sub ~ctx t
      else t
  in
  (* try to purify *)
  let lits' = Literals.map (purify_term ~ctx:C_root) lits in
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
      shielded_by_term ~root f ||
      List.exists (shielded_by_term ~root:false) l
  in
  (* is there a term, directly under a literal, that shields the variable? *)
  lits
  |> Literals.Seq.terms
  |> Sequence.exists (shielded_by_term ~root:true)

let unshielded_vars lits: _ list =
  Literals.vars lits
  |> List.filter
    (fun var ->
       type_is_purifiable (HVar.ty var) &&
       not (is_shielded var lits))
