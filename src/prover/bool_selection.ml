
open Logtk

let section = Util.Section.make ~parent:Const.section "bool_sel"

module Lit = Literal
module Pos = Position
module PB = Pos.Build
module BIn = Builtin
module T = Term

(* context sign *)
let pos_ctx = 1
let neg_ctx = pos_ctx lsl 1
let under_equiv_ctx = neg_ctx lsl 1
let sgn_ctx_mask = pos_ctx lor neg_ctx lor under_equiv_ctx

let clear_sgn_ctx ctx = ctx land (lnot sgn_ctx_mask)
let get_sgn_ctx ctx = ctx land sgn_ctx_mask


(* direct argument *)
let premise_ctx = under_equiv_ctx lsl 1
let consequent_ctx = premise_ctx lsl 1
let and_ctx = consequent_ctx lsl 1
let or_ctx = and_ctx lsl 1
let neg_sym_ctx = or_ctx lsl 1
let direct_arg_mask = premise_ctx lor consequent_ctx lor and_ctx lor or_ctx lor neg_ctx

let get_arg_ctx ctx = ctx land direct_arg_mask
let clear_arg_ctx ctx = ctx land (lnot direct_arg_mask)

(** As described in FBoolSup paper, Boolean selection function
    selects positions in the clause that are non-interpreted 
    Boolean subterms. *)

type t = Literal.t array -> (Term.t * Pos.t) list

type parametrized = strict:bool -> ord:Ordering.t -> t

(* Zipperposition interprets argument positions
   inverted, so we need to convert
   them before calling PositionBuilder *)
let inv_idx args idx = 
    List.length args - idx - 1

let is_selectable ~forbidden ~top t =
  let module VS = T.VarSet in
  Type.is_prop (T.ty t) && 
  not top && 
  not (T.is_var t) && not (T.is_true_or_false t) &&
  (
    T.is_ground t || (* avoid computing the set of vars for t *)
    VS.is_empty forbidden || (* avoid computing the set of vars for t *)
    VS.is_empty (VS.inter (VS.of_iter (T.Seq.vars t)) forbidden)
  )

let get_forbidden_vars ~ord lits =
  (* Check the HOSup paper for the definition of forbidden vars *)
  Literals.maxlits_l ~ord lits 
  |> CCList.map (fun (l,_) -> l) 
  |> Array.of_list
  |> CCArray.fold (
      fun vars lit ->
        Lit.fold_terms ~vars:true ~ty_args:false
                       ~which:`Max ~ord ~subterms:false lit
        |> Iter.filter_map (fun (t,_) -> (T.as_var @@ T.head_term t))
        |> T.VarSet.of_iter
        |> T.VarSet.union vars
    ) T.VarSet.empty

let select_leftmost ~ord ~kind lits =
  let open CCOpt in
  let forbidden = get_forbidden_vars ~ord lits in

  let rec aux_lits idx =
    if idx >= Array.length lits then None
    else (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation(lhs,rhs,_) ->
        let in_literal = 
          match Ordering.compare ord lhs rhs with 
          | Comparison.Lt ->
            (aux_term ~top:true ~pos_builder:(PB.right pos_builder) rhs)
          | Comparison.Gt ->
            (aux_term ~top:true ~pos_builder:(PB.left pos_builder) lhs)
          | _ ->
            (aux_term ~top:true ~pos_builder:(PB.left pos_builder) lhs)
              <+>
            (aux_term ~top:true ~pos_builder:(PB.right pos_builder) rhs)
        in
        in_literal <+> (aux_lits (idx+1))
      | _ -> aux_lits (idx+1))
  and aux_term ~top ~pos_builder t =
    (* if t is app_var then return the term itself, but do not recurse  *)
    if T.is_app_var t then (
      CCOpt.return_if (is_selectable ~top ~forbidden t) (t, PB.to_pos pos_builder)
    ) else (
      match T.view t with
      | T.App(_, args)
      | T.AppBuiltin(_, args) ->
        (* reversing the argument indices *)
        let inner = aux_term_args args ~idx:(List.length args - 1) ~pos_builder in
        let outer =
          if not (is_selectable ~top ~forbidden t) then None
          else Some (t, PB.to_pos pos_builder) 
        in
        if kind == `Inner then inner <+> outer
        else outer <+> inner
      | T.Const _ when is_selectable ~top ~forbidden t -> Some (t, PB.to_pos pos_builder)
      | _ -> None
    )
  and aux_term_args ~idx ~pos_builder = function
  | [] -> None
  | x :: xs ->
    assert (idx >= 0);
    (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x)
      <+>
    (aux_term_args ~idx:(idx-1) ~pos_builder xs) 
  in
  CCOpt.map_or ~default:[] (fun x -> [x]) (aux_lits 0)

let collect_green_subterms_ ~forbidden ~filter ~ord ~pos_builder t k = 
  let rec aux_term ~top ~pos_builder t k =
    if filter ~forbidden ~top t then (k (t, PB.to_pos pos_builder));

    if T.is_app_var t then ( (* only green subterms are eligible *) )
    else (
      match T.view t with
      | T.AppBuiltin((BIn.Eq|BIn.Neq|BIn.Xor|BIn.Equiv),( ([a;b] | [_;a;b]) as l)) 
          when Type.is_prop (T.ty a) ->
        (* only going to the larger side of the (dis)equation *)
        let offset = List.length l - 2 in (*skipping possible tyarg*)
        (match Ordering.compare ord a b with 
          | Comparison.Lt ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b k
          | Comparison.Gt ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a k
          | _ ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b k;
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a k)
      | T.App(_, args)
      | T.AppBuiltin(_, args)->
        aux_term_args ~idx:(List.length args - 1) ~pos_builder args k
      | _ -> ())
  and aux_term_args ~idx ~pos_builder args k = 
    match args with
    | [] -> ()
    | x :: xs ->
      assert (idx >= 0);
      (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x k);
      (aux_term_args ~idx:(idx-1) ~pos_builder xs k) in
  aux_term ~top:true ~pos_builder t k

let all_selectable_subterms ?(forbidden=T.VarSet.empty) ~ord ~pos_builder =  
  collect_green_subterms_ ~forbidden ~filter:is_selectable ~ord ~pos_builder


let all_eligible_subterms ~ord ~pos_builder =
  (* make sure you select only subterms *)
  let filter ~forbidden ~top t  = 
    not top &&
    not (Type.is_tType (T.ty t)) in
  collect_green_subterms_ ~forbidden:(T.VarSet.empty) ~filter ~ord ~pos_builder 

let get_selectable_w_ctx ~ord lits = 
  let open CCOpt in
  let forbidden = get_forbidden_vars ~ord lits in

  let selectable_with_ctx ?(forbidden=T.VarSet.empty) ~block_top ~ord ~pos_builder t ctx log_depth k =
    let negate_sgn ctx =
      let sgn = get_sgn_ctx ctx in
      if sgn == pos_ctx then neg_ctx 
      else if sgn == neg_ctx then pos_ctx 
      else under_equiv_ctx 
    in

    let rec aux_term ~top ~pos_builder t ctx log_depth k =
      if is_selectable ~forbidden ~top:(top && block_top) t 
      then (k (t, (ctx, log_depth), PB.to_pos pos_builder));

      if T.is_app_var t then ( (* only green subterms are eligible *) )
      else (
        match T.view t with
        | T.AppBuiltin((BIn.Eq|BIn.Neq|BIn.Xor|BIn.Equiv),( ([a;b] | [_;a;b]) as l)) ->
          (* only going to the larger side of the (dis)equation *)
          let ctx = under_equiv_ctx in
          let offset = List.length l - 2 in (*skipping possible tyarg*)
          (match Ordering.compare ord a b with 
            | Comparison.Lt ->
              aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b ctx (log_depth + 1) k
            | Comparison.Gt ->
              aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a ctx (log_depth + 1) k
            | _ ->
              aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b ctx (log_depth + 1) k;
              aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a ctx (log_depth + 1) k)
        | T.App(_, args) -> 
          aux_term_args ~idx:(List.length args - 1) ~pos_builder args under_equiv_ctx log_depth k
        | T.AppBuiltin(Builtin.Not, [t]) ->
          let ctx = (negate_sgn ctx) lor neg_sym_ctx in
          aux_term ~top:false ~pos_builder:(PB.arg 0 pos_builder) t ctx (log_depth + 1) k
        | T.AppBuiltin(Builtin.And, args) ->
          let ctx = (get_sgn_ctx ctx) lor and_ctx in
          aux_term_args ~idx:(List.length args - 1) ~pos_builder args ctx  (log_depth + 1)k
        | T.AppBuiltin(Builtin.Or, args) ->
          let ctx = (get_sgn_ctx ctx) lor or_ctx in
          aux_term_args ~idx:(List.length args - 1) ~pos_builder args ctx (log_depth + 1) k
        | T.AppBuiltin(Builtin.Imply, [p;c]) ->
          let ctx_p = (negate_sgn ctx) lor premise_ctx in
          aux_term ~top:false ~pos_builder:(PB.arg 1 pos_builder) p ctx_p (log_depth + 1) k;
          let ctx_c = (get_sgn_ctx ctx) lor consequent_ctx in
          aux_term ~top:false ~pos_builder:(PB.arg 0 pos_builder) c ctx_c (log_depth + 1) k;
        | T.AppBuiltin(hd, args) when not (Builtin.is_quantifier hd) ->
          aux_term_args ~idx:(List.length args - 1) ~pos_builder args ctx log_depth k
        | _ -> ())
    and aux_term_args ~idx ~pos_builder args ctx log_depth k = 
      match args with
      | [] -> ()
      | x :: xs ->
        assert (idx >= 0);
        (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x ctx log_depth k);
        (aux_term_args ~idx:(idx-1) ~pos_builder xs ctx log_depth k)
    in
    aux_term ~top:true ~pos_builder t ctx log_depth k
  in

  let rec aux_lits idx k =
    if idx < Array.length lits then (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation (lhs,rhs,sign) as lit ->
        let ctx_sign = if Lit.is_predicate_lit lit then 
                          (if Lit.is_positivoid lit then pos_ctx else neg_ctx) 
                       else under_equiv_ctx in
        begin match Ordering.compare ord lhs rhs with
          | Comparison.Lt ->
            selectable_with_ctx ~block_top:sign ~ord ~forbidden ~pos_builder:(PB.right pos_builder) rhs ctx_sign 0 k
          | Comparison.Gt ->
            selectable_with_ctx ~block_top:sign ~ord ~forbidden ~pos_builder:(PB.left pos_builder) lhs ctx_sign 0 k
          | _ ->
            selectable_with_ctx ~block_top:sign ~ord ~forbidden ~pos_builder:(PB.left pos_builder) lhs ctx_sign 0 k;
            selectable_with_ctx ~block_top:sign ~ord ~forbidden ~pos_builder:(PB.right pos_builder) rhs ctx_sign 0 k
        end;
        aux_lits (idx+1) k
      | _ -> aux_lits (idx+1) k)
  in
  aux_lits 0

let by_size ~ord ~kind lits =
  let selector =
    if kind = `Max then Iter.max else Iter.min in
  get_selectable_w_ctx ~ord lits
  |> selector ~lt:(fun (s,_,_) (t,_,_) -> Term.ho_weight s < Term.ho_weight t)
  |> CCOpt.map_or ~default:[] (fun (t,ctx,pos) -> [(t,pos)])

let by_context_weight_combination ~ord ~ctx_fun ~weight_fun lits =
    let bin_of_int d =
    if d < 0 then invalid_arg "bin_of_int" else
    if d = 0 then "0" else
    let rec aux acc d =
      if d = 0 then acc else
      aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    String.concat "" (aux [] d)
    in

  get_selectable_w_ctx ~ord lits
  |> Iter.map (fun ((t,(ctx,_),_) as arg) ->
    Util.debugf ~section 2 "selectable @[%a/%8s@]@." (fun k -> k T.pp t (bin_of_int ctx)); arg)
  |> Iter.min ~lt:(fun (s,ctx_s,_) (t,ctx_t,_) -> 
    CCOrd.(<?>) (ctx_fun ctx_s ctx_t) (weight_fun lits, s, t) < 0)
  |> CCOpt.map_or ~default:[] (fun (t,ctx,pos) -> [(t,pos)])

let is_eq t = 
  match T.view t with
  | T.AppBuiltin((Eq|Neq), _) -> true
  | _ -> false

let sel1 lits =
  let w_tbl = ID.Tbl.create 16 in
  let incr_id = ID.Tbl.incr ~by:1 w_tbl in
  let id_w = ID.Tbl.get_or ~default:0 w_tbl in

  Iter.of_array lits
  |> Iter.flat_map (Lit.Seq.symbols ~include_types:false)
  |> Iter.iter incr_id;

  fun s t ->
    let (<?>) = CCOrd.(<?>) in
    compare (T.is_appbuiltin s) (T.is_appbuiltin t) (* defer formulas *)
    <?> (compare, (not (T.is_ground s)), (not (T.is_ground t))) (* prefer ground *)
    <?> (compare, not (is_eq s), not (is_eq t)) (*prefer equations*)
    <?> (compare, T.weight ~var:1 ~sym:id_w s,
                  T.weight ~var:1 ~sym:id_w t) (* by weight, where the weight of symbol
                                                  is its number of occs*)
let sel2 lits s t =
  let (<?>) = CCOrd.(<?>) in
  compare (T.is_appbuiltin s) (T.is_appbuiltin t) (* defer formulas *)
  <?> (compare, - (T.depth s),  - (T.depth t)) (* prefer deeper *)
  <?> (compare, T.VarSet.cardinal (T.vars s),
                T.VarSet.cardinal (T.vars t)) (* prefer less distinct vars *)
  <?> (compare, T.ho_weight s, T.ho_weight t) (* by weight, where the weight of symbol
                                                 is its number of occs*)


let sel3 =
  let is_type_pred t =
    match T.view t with
    | T.App(hd, args) -> T.is_const hd && List.for_all T.is_var args
    | _ -> false
  in

  fun lits s t ->
    let (<?>) = CCOrd.(<?>) in
    compare (T.is_appbuiltin s) (T.is_appbuiltin t) (* defer formulas *)
    <?> (compare, is_type_pred s, is_type_pred t) (* defer p(x) where x are variables *)
    <?> (compare, T.is_app_var s, T.is_app_var t) (* defer app vars *)
    <?> (compare, T.ho_weight s, T.ho_weight t) (* by weight *)

let prefer_pos_ctx (ctx1,_) (ctx2,_) =
  compare (get_sgn_ctx ctx1 != pos_ctx)
          (get_sgn_ctx ctx2 != pos_ctx)

let prefer_neg_ctx (ctx1,_) (ctx2,_) =
  compare (get_sgn_ctx ctx1 != neg_ctx)
          (get_sgn_ctx ctx2 != neg_ctx)

let prefer_and_arg (ctx1,_) (ctx2,_) =
  compare (get_arg_ctx ctx1 != and_ctx)
          (get_arg_ctx ctx2 != and_ctx)

let prefer_or_arg (ctx1,_) (ctx2,_) =
  compare (get_arg_ctx ctx1 != or_ctx)
          (get_arg_ctx ctx2 != or_ctx)

let prefer_neg_arg (ctx1,_) (ctx2,_) =
  compare (get_arg_ctx ctx1 != neg_sym_ctx)
          (get_arg_ctx ctx2 != neg_sym_ctx)

let prefer_premises_arg (ctx1,_) (ctx2,_) =
  compare (get_arg_ctx ctx1 != premise_ctx)
          (get_arg_ctx ctx2 != premise_ctx)

let prefer_concl_arg (ctx1,_) (ctx2,_) =
  compare (get_arg_ctx ctx1 != consequent_ctx)
          (get_arg_ctx ctx2 != consequent_ctx)

let prefer_deep_log (_,log_d1) (_, log_d2) =
  compare (-log_d1) (-log_d2)

let prefer_shallow_log (_,log_d1) (_, log_d2) =
  compare (log_d1) (log_d2)

let discard_ctx _ _ = 0


let leftmost_innermost ~ord lits =
  select_leftmost ~ord ~kind:`Inner lits
let leftmost_outermost ~ord lits =
  select_leftmost ~ord ~kind:`Outer lits
let smallest ~ord lits =
  by_size ~ord ~kind:`Min lits
let largest ~ord lits =
  by_size ~ord ~kind:`Max lits

let none ~ord lits = []

let fun_names = 
  [ ("LI", leftmost_innermost);
    ("LO", leftmost_outermost);
    ("smallest", smallest);
    ("largest", largest);
    ("none", none) ]

let parse_combined_function ~ord s =
  let sel_funs = [sel1; sel2; sel3] in
  let ctx_funs = [
    ("any_ctx", discard_ctx);
    ("pos_ctx", prefer_pos_ctx);
    ("neg_ctx", prefer_neg_ctx);
    ("and_arg_ctx", prefer_and_arg);
    ("or_arg_ctx", prefer_or_arg);
    ("neg_arg_ctx", prefer_neg_arg);
    ("antecedent_ctx", prefer_premises_arg);
    ("consequent_ctx", prefer_concl_arg);
    ("deep_log_ctx", prefer_deep_log);
    ("shallow_log_ctx", prefer_shallow_log);
  ] in

  let or_lmax_regex =
    Str.regexp ("sel\\([1-3]\\)(\\(.+\\))") in
    try
      ignore(Str.search_forward or_lmax_regex s 0);
      let sel_id = CCOpt.get_exn (CCInt.of_string (Str.matched_group 1 s)) in
      let ctx_fun_name = Str.matched_group 2 s in

      let weight_fun = List.nth sel_funs (sel_id-1) in
      let ctx_fun = List.assoc ctx_fun_name ctx_funs in

      by_context_weight_combination ~ord ~ctx_fun ~weight_fun

      
      
    with Not_found | Invalid_argument _ ->
      Util.invalid_argf
        "expected sel[123](%a)" 
          (CCList.pp ~pp_sep:(CCFormat.return "|") CCString.pp)
          (List.map fst ctx_funs)

let from_string ~ord name lits =
  let res =
    (try 
      (List.assoc name fun_names) ~ord
    with _ ->
      try
        parse_combined_function ~ord name
      with _ ->
        invalid_arg (name ^ " is not a valid bool selection name"))
  in
  res lits
  |> CCFun.tap (fun l ->
    Util.debugf ~section 1 "bool_select(@[%a@])=@.@[%a@]@."
      (fun k-> k Literals.pp lits (CCList.pp Term.pp) (List.map fst l))
  )

let () =
  let set_bselect s = Params.bool_select := s in
  Params.add_opts
    [ "--bool-select", Arg.String(set_bselect), " set boolean literal selection function"];
