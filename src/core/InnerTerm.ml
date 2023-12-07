
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Inner Terms} *)

module I = Int32

let zero = I.zero

let (<<<) = I.shift_left
let (>>>) = I.shift_right_logical
let (&&&) = I.logand
let (|||) = I.logor
let (~~~) = I.lognot

(* flags for propetries
   flag is set if for any subterm the corresponding property holds *)
let f_has_freevars = I.one 
let f_is_beta_reducible = f_has_freevars <<< 1

(* If DB has more than 16 bits (very unlikely),
   then we set a bit that forces to recompute the property *)
let f_db_overflowed = f_is_beta_reducible <<< 1

let f_has_lams = f_db_overflowed <<< 1
let f_has_quant = f_has_lams <<< 1

(* From 16th bit onwards, we will keep the maximum 
   De Bruijn variable seen so far. *)
let f_db_mask = (~~~ zero) <<< 16
let max_db = I.to_int (f_db_mask >>> 16)


let set_property props prop_flag =
  props ||| prop_flag

let unset_property props prop_flag =
  props &&& (~~~ prop_flag)

let get_property props prop_flag = 
  not @@ I.equal zero (props &&& prop_flag)

let dec_max_db props =
  let max_db_val = I.to_int ((props &&& f_db_mask) >>> 16) in
  let cleared = props &&& (~~~ f_db_mask) in
  let max_db_val = (I.of_int @@ max (max_db_val - 1) 0) <<< 16 in
  cleared ||| max_db_val

let update_max_db props new_db =
  if new_db > max_db then (
    set_property props f_db_overflowed 
  ) else (
    let max_db_val = 
      max (I.to_int ((props &&& f_db_mask) >>> 16)) new_db in
    (props &&& (~~~ f_db_mask)) ||| ((I.of_int max_db_val) <<< 16))

let get_max_db props =
  I.to_int ((props &&& f_db_mask) >>> 16)

(* Properties that should be set if they are set for ANY of the subterms *)
let any_props = f_is_beta_reducible ||| f_has_freevars ||| f_has_lams ||| f_has_quant
(* Properties that should be set if they are set for ALL of the subterms *)
let all_props = zero (* currently no props like that -- is_closed computed
                        by looking at the value of max_db *)

let debug_props out props =
  CCFormat.fprintf out "db:%d" (get_max_db props);
  CCFormat.fprintf out " h_fv:%b" (get_property props f_has_freevars);
  CCFormat.fprintf out " h_l:%b" (get_property props f_has_lams);
  CCFormat.fprintf out " h_q:%b" (get_property props f_has_quant);
  CCFormat.fprintf out " beta_r:%b@." (get_property props f_is_beta_reducible);

type t = {
  term : view;
  ty : type_result;
  mutable id : int;
  mutable payload: exn;
  props: I.t;
  ho_weight : int lazy_t;
}

(* head form *)
and view =
  | Var of t HVar.t (** Free or bound variable *)
  | DB of int
  | Bind of Binder.t * t * t (** Type, sub-term *)
  | Const of ID.t (** Constant *)
  | App of t * t list (** Uncurried application *)
  | AppBuiltin of Builtin.t * t list (** For representing special constructors *)

and type_result =
  | NoType
  | HasType of t

type term = t

let any_props_for_ts =
  List.fold_left (fun acc t -> 
      let new_props = (any_props &&& t.props) ||| acc in
      update_max_db new_props (get_max_db t.props)
    ) zero

let add_ty_vars props ty_props =
  if get_property ty_props f_has_freevars then (
    set_property props f_has_freevars
  ) else props

let add_ty_vars props ty_props =
  if get_property ty_props f_has_freevars then (
    set_property props f_has_freevars
  ) else props

let[@inline] view t = t.term
let[@inline] ty t = t.ty
let[@inline] ty_exn t = match t.ty with
  | NoType -> invalid_arg "InnerTerm.ty_exn"
  | HasType ty -> ty

let[@inline] hash t = Hash.int t.id
let[@inline] equal : t -> t -> bool = fun t1 t2 -> t1 == t2
let[@inline] compare t1 t2 = Stdlib.compare t1.id t2.id

let rec same_l_rec l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> assert false
  | x1 :: tail1, x2 :: tail2 ->
    equal x1 x2 && same_l_rec tail1 tail2

let same_l l1 l2 = match l1, l2 with
  | [], [] -> true
  | [t1], [t2] -> equal t1 t2
  | [t1;u1], [t2;u2] -> equal t1 t2 && equal u1 u2
  | _ -> same_l_rec l1 l2

let [@inline] ty_is_fun ty = match view ty with
  | AppBuiltin (Builtin.Arrow, _ret :: args) -> List.length args != 0
  | _ -> false

let same_l_gen l1 l2 =
  List.length l1 == List.length l2 && same_l l1 l2

let _hash_ty t = match t.ty with
  | NoType -> 1
  | HasType ty -> Hash.combine2 2 ty.id

let _hash_norec t = match view t with
  | Var v -> Hash.combine2 1 (HVar.hash v)
  | DB v -> Hash.combine2 2 (Hash.int v)
  | Bind (b, varty, t') ->
    Hash.combine4 3 (Binder.hash b)(hash varty)(hash t')
  | Const s -> Hash.combine2 4 (ID.hash s)
  | App (f, l) -> Hash.combine3 10 (hash f) (Hash.list hash l)
  | AppBuiltin (b, l) -> Hash.combine3 20 (Builtin.hash b) (Hash.list hash l)

let hash_mod_alpha t : int =
  let rec aux (d:int) t =
    let h_t =
      if d=0 then 10 (* fuel is exhausted *)
      else match t.term with
        | Var _ -> 1 (* ignore variable's name *)
        | DB v -> Hash.combine2 2 (Hash.int v)
        | Bind (b, varty, t') ->
          Hash.combine4 3 (Binder.hash b) (aux (d-1) varty) (aux (d-1) t')
        | Const s -> Hash.combine2 4 (ID.hash s)
        | App (f, l) -> Hash.combine3 10 (aux (d-1) f) (Hash.list (aux (d-1)) l)
        | AppBuiltin (b, l) ->
          Hash.combine3 20 (Builtin.hash b) (Hash.list (aux (d-1)) l)
    and h_ty = match t.ty with
      | NoType -> 0
      | HasType ty -> aux d ty
    in
    Hash.combine3 42 h_t h_ty
  in
  aux 2 t

let _eq_ty t1 t2 = match t1.ty, t2.ty with
  | NoType, NoType -> true
  | HasType ty1, HasType ty2 -> equal ty1 ty2
  | _ -> false

let rec _eq_norec_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> equal t1 t2 && _eq_norec_list l1' l2'

let rec _eq_record_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | (n1,t1)::l1', (n2,t2)::l2' -> n1=n2 && equal t1 t2 && _eq_record_list l1' l2'

let _eq_norec t1 t2 =
  _eq_ty t1 t2 &&
  match t1.term, t2.term with
  | Var i, Var j -> HVar.equal equal i j
  | DB i, DB j -> i = j
  | Const s1, Const s2 -> ID.equal s1 s2
  | Bind (b1, varty1, t1'), Bind (b2, varty2, t2') ->
    Binder.equal b1 b2 && equal varty1 varty2 && equal t1' t2'
  | App (f1, l1), App (f2, l2) ->
    equal f1 f2 && _eq_norec_list l1 l2
  | AppBuiltin (b1, l1), AppBuiltin (b2, l2) ->
    Builtin.equal b1 b2 && _eq_norec_list l1 l2
  | _ -> false

(** {3 Constructors} *)

(* XXX: use cppo?
   module H = Hashcons.MakeNonWeak(struct
*)
module H = Hashcons.Make(struct
    type t = term
    let equal = _eq_norec
    let hash = _hash_norec
    let[@inline] tag i t = assert (t.id = ~-1); t.id <- i
  end)

let hashcons_stats () = H.stats ()

exception No_payload

exception IllFormedTerm of string

type nat = int

let rec open_bind b t = match view t with
  | Bind (b', ty, t') when b=b' ->
    let args, ret = open_bind b t' in
    ty :: args, ret
  | _ -> [], t

let[@inline] is_var t = match view t with | Var _ -> true | _ -> false
let[@inline] is_lam t = match view t with | Bind(Binder.Lambda,_,_) -> true | _ -> false

let open_fun ty = match view ty with
  | AppBuiltin (Builtin.Arrow, ret :: args) -> args, ret
  | _ -> [], ty

let ho_weight_ t t_ty = 
  let rec aux t t_ty = 
    let init_w s_ty = 
      match s_ty with 
      | NoType -> 0
      | HasType ty -> 
        begin match view ty with 
          | AppBuiltin (Builtin.Arrow, l) -> List.length l - 1
          | _ -> 0 end in
    begin match t with 
      | Var _  | DB _  | Const _ -> init_w t_ty +  1
      | Bind (Binder.Lambda, _, t') ->
        let _, t' = open_bind Binder.Lambda t' in
        init_w (ty t') + aux (view t') (ty t')
      | Bind (_,_, t') -> 
        aux (view t') (ty t')
      | App (f, l) ->
        if is_var f then 1
        else init_w t_ty + aux_l (aux (view f) (ty f)) l
      | AppBuiltin (_, l) -> aux_l (init_w t_ty + 1) l end
  and aux_l acc = function
    | [] -> acc
    | x :: xs -> aux_l (acc + aux (view x) (ty x))  xs 
  in
  aux t t_ty  

let[@inline] ho_weight t = Lazy.force t.ho_weight

let make_ ~props ~ty term  =
  { term; ty; id = ~-1; payload=No_payload;
    props; ho_weight = lazy (ho_weight_ term ty) }

let const ~ty s =
  let my_t = make_ ~props:(add_ty_vars zero ty.props) ~ty:(HasType ty) (Const s) in 
  H.hashcons my_t

let builtin ~ty b =
  let my_t = make_ ~props:(add_ty_vars zero ty.props) ~ty:(HasType ty) (AppBuiltin (b,[])) in
  H.hashcons my_t

let tType =
  let my_t = make_ ~props:zero ~ty:NoType (AppBuiltin(Builtin.TType, [])) in
  H.hashcons my_t

let open_fun ty = match view ty with
  | AppBuiltin (Builtin.Arrow, ret :: args) -> args, ret
  | _ -> [], ty

let[@inline] is_a_type t = match ty t with
  | HasType ty -> equal ty tType
  | NoType -> assert false

let expected_args t = 
  match view t with 
  | AppBuiltin(Builtin.Arrow, l) -> CCList.length l-1
  | _ -> 0

let rec debugf out t = match view t with
  | AppBuiltin (b,[]) -> Builtin.pp out b
  | AppBuiltin (b,l) ->
    Format.fprintf out "(@[<1>%a@ %a@])" Builtin.pp b (Util.pp_list debugf) l
  | Var i -> HVar.pp out i
  | DB i -> Format.fprintf out "Y%d" i
  | Const s -> ID.pp out s
  | App (_, []) -> assert false
  | App (s, l) ->
    Format.fprintf out "(@[<1>%a@ %a@])" debugf s (Util.pp_list debugf) l
  | Bind (b, varty,t') ->
    Format.fprintf out "(@[<1>%a@ %a@ %a@])"
      Binder.pp b debugf varty debugf t'

let flatten_and_or b l =
  let rec aux acc = function 
    | [] -> List.rev acc
    | x :: xs ->
      match view x with 
      | AppBuiltin(b', ys) when Builtin.equal b b' ->
        aux acc (ys @ xs)
      | _ -> aux (x::acc) xs 
  in
  if Builtin.is_flattened_logical b then aux [] l
  else l

let rec app_builtin ~ty b l = 
  let prop = builtin ~ty:tType Builtin.prop in
  let l = flatten_and_or b l in
  
  match b, l with
  | Builtin.Arrow, [] -> assert false
  | Builtin.Arrow, [ret] -> ret
  | Builtin.Arrow, ({term=AppBuiltin(Builtin.Arrow, ret::l1); _} :: l2) ->
    (* flatten *)
    app_builtin ~ty Builtin.Arrow (ret :: l2 @ l1)
  | Builtin.Not, [] ->
    let ty = app_builtin ~ty:tType Builtin.arrow [prop;prop] in
    let my_t = make_ ~props:zero ~ty:(HasType ty) (AppBuiltin (b,[])) in
    H.hashcons my_t
  | (Builtin.And | Builtin.Or), l 
    when CCList.length l < 2 && 
         expected_args ty < 2 ->
    let args = (if CCList.is_empty l then [prop] else []) @ [prop;prop] in
    let ty = app_builtin ~ty:tType Builtin.arrow args in
    let props = add_ty_vars (any_props_for_ts l) ty.props in
    let my_t = make_ ~props ~ty:(HasType ty) (AppBuiltin (b,l)) in
    H.hashcons my_t
  | _ ->
    assert (not (List.mem b Builtin.[Eq;Neq;ForallConst;ExistsConst]) ||
            List.length l >= 1 &&
            is_a_type (List.hd l));
    let ty = 
      if Builtin.is_quantifier b && List.length l = 2 then (
        (* reassing the type if other parts of the code assigned it wrong *)
        prop
      ) else ty in

    let props = add_ty_vars (any_props_for_ts l) ty.props in
    let props = 
      if Builtin.is_quantifier b && List.length l = 2 then (
        set_property props f_has_quant
      ) else props in
    let my_t = make_ ~props ~ty:(HasType ty) (AppBuiltin (b,l)) in
    H.hashcons my_t

let arrow l r =
  app_builtin ~ty:tType Builtin.arrow (r :: l)

let app ~ty f l = match f.term, l with
  | _, [] -> f
  | App (f1, l1), _::_ ->
    (* flatten *)
    let flattened = l1 @ l in
    let props = add_ty_vars (any_props_for_ts (f1 :: flattened)) ty.props in
    let props =
      if is_lam f1 then set_property props f_is_beta_reducible else props in
    let my_t = make_ ~props ~ty:(HasType ty) (App (f1,flattened)) in
    H.hashcons my_t
  | AppBuiltin (f1, l1), _ ->
    let t_args = flatten_and_or f1 l1 @ l in
    let ty =
      if Builtin.is_logical_op f1 && not (Builtin.is_quantifier f1) then (
        let prop = builtin ~ty:tType Builtin.Prop in

        let args,_ = open_fun ty in
        if List.length args > 0 then (
          ty
        ) else if Builtin.is_logical_binop f1 then (
          if List.length t_args >= 2 then prop
          else (if List.length t_args = 1 then arrow [prop] prop
                else arrow [prop;prop] prop)
        ) else (
          if List.length t_args = 1 then prop
          else arrow [prop] prop
        ))
      else ty in
    let props = add_ty_vars (any_props_for_ts t_args) ty.props in
    let props = 
      if Builtin.is_quantifier f1 && List.length t_args = 2 then (
        set_property props f_has_quant
      ) else props 
    in
    let my_t = make_ ~props ~ty:(HasType ty) (AppBuiltin (f1,t_args)) in
    H.hashcons my_t
  | _ ->
    let props = add_ty_vars (any_props_for_ts (f :: l)) ty.props in
    let props =
      if is_lam f then set_property props f_is_beta_reducible else props in
    let my_t = make_ ~props ~ty:(HasType ty) (App (f,l)) in
    H.hashcons my_t

let var v = 
  let props = set_property zero f_has_freevars in
  H.hashcons (make_ ~props ~ty:(HasType (HVar.ty v)) (Var v))

let bvar ~ty i =
  if i<0 then raise (IllFormedTerm "bvar");
  let max_db = i+1 in
  let props = add_ty_vars (update_max_db zero max_db) ty.props in
  H.hashcons (make_ ~props ~ty:(HasType ty) (DB i))

let bind ~ty ~varty s t' =
  let props = add_ty_vars (add_ty_vars t'.props ty.props) varty.props in
  let props = 
    if Binder.equal Binder.Lambda s || Binder.equal Binder.ForallTy s 
    then dec_max_db props 
    else props in
  let props = 
    if Binder.equal Binder.Lambda s
    then set_property props f_has_lams
    else props in
  H.hashcons (make_ ~props ~ty:(HasType ty) (Bind (s, varty, t')))

let cast ~ty old = match old.term with
  | Var v -> var (HVar.cast v ~ty)
  | DB i -> bvar ~ty i
  | Const s -> const ~ty s
  | Bind (s, varty, t') -> bind ~ty s ~varty t'
  | App (f,l) -> app ~ty f l
  | AppBuiltin (s,l) -> app_builtin ~ty s l

let[@inline] is_bvar t = match view t with | DB _ -> true | _ -> false
let[@inline] is_const t = match view t with | Const _ -> true | _ -> false
let[@inline] is_bind t = match view t with | Bind _ -> true | _ -> false
let[@inline] is_app t = match view t with | App _ -> true | _ -> false
let[@inline] is_tType t = match view t with AppBuiltin (Builtin.TType, _) -> true | _ -> false

let[@inline] is_lambda t = match view t with Bind (Binder.Lambda, _, _) -> true | _ -> false


(** {3 Payload} *)

let payload t = t.payload

let set_payload_erase t e = t.payload <- e

let set_payload t e = match t.payload with
  | No_payload -> t.payload <- e
  | _ -> invalid_arg "Term.set_payload: collision"

(** {3 Containers} *)

module AsKey = struct
  type t = term
  let equal = equal
  let hash = hash
  let compare = compare
end

module Set = CCSet.Make(AsKey)
module Map = CCMap.Make(AsKey)
module Tbl = CCHashtbl.Make(AsKey)

module HVarKey = struct
  type t = term HVar.t
  let compare = HVar.compare compare
  let equal = HVar.equal equal
  let hash = HVar.hash
end

module VarMap = CCMap.Make(HVarKey)
module VarSet = CCSet.Make(HVarKey)
module VarTbl = CCHashtbl.Make(HVarKey)

(** {3 Basic Printer} *)

let[@inline] has_lambda t =
  get_property t.props f_has_lams

let[@inline] is_eta_reducible t =
  (* if it has a quantifier -- we have to expand *)
  get_property t.props f_has_quant ||
  get_property t.props f_has_lams

let[@inline] is_beta_reducible t =
  let res = get_property t.props f_is_beta_reducible in
  assert(not res || has_lambda t);
  res

(** {3 De Bruijn} *)

module DB = struct
  type env = t DBEnv.t

  (* sequence2 of [De Bruijn, depth] pairs *)
  let rec _to_iter ~depth t k =
    begin match t.ty with
      | NoType -> ()
      | HasType ty -> _to_iter ~depth ty k
    end;
    match view t with
    | DB v -> k (v,depth)
    | Var _
    | Const _ -> ()
    | Bind (_, varty, t') ->
      _to_iter ~depth varty k;
      _to_iter ~depth:(depth+1) t' k
    | AppBuiltin (_, l) ->
      List.iter (fun t -> _to_iter ~depth t k) l
    | App (f, l) ->
      _to_iter ~depth f k;
      List.iter (fun t -> _to_iter ~depth t k) l

  let[@inline] _id x = x

  let closed t =
    let db_calc t = 
      _to_iter ~depth:0 t
      |> Iter.map (fun (bvar,depth) -> bvar < depth)
      |> Iter.for_all _id in
    if get_property t.props f_db_overflowed then (
      db_calc t 
    ) else (
      let res = (get_max_db t.props) = 0 in
      (* if(res != db_calc t) then (
         CCFormat.printf "t:@[%a@];max_db:%d@." debugf t (get_max_db t.props);
         assert(false);
         ); *)
      res
    )

  (* check whether t contains the De Bruijn symbol n *)
  let contains t n =
    _to_iter ~depth:0 t
    |> Iter.map (fun (bvar,depth) -> bvar=n+depth)
    |> Iter.exists _id

  let unbound t =
    _to_iter ~depth:0 t
    |> Iter.filter_map
      (fun (bvar,depth) -> if bvar >= depth then Some (bvar - depth) else None)
    |> Iter.to_rev_list

  (* maps the term to another term, calling [on_binder acc t]
     when it meets a binder, and [on_bvar acc t] when it meets a
     bound variable. *)
  let _fold_map ?(depth=0) acc ~on_bvar ~on_binder t =
    let rec recurse ~depth acc t = match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = recurse ~depth acc ty in
        match view t with
        | Var v -> var (HVar.cast ~ty v)
        | DB i -> bvar ~ty (on_bvar ~depth acc i)
        | Const s -> const ~ty s
        | Bind (s, varty, t') ->
          let acc' = on_binder ~ty ~depth acc s varty in
          let varty' = recurse ~depth acc varty in
          let t' = recurse ~depth:(depth+1) acc' t' in
          bind ~ty ~varty:varty' s t'
        | App (f, l) ->
          app ~ty (recurse ~depth acc f) (List.map (recurse ~depth acc) l)
        | AppBuiltin (s,l) ->
          app_builtin ~ty s (List.map (recurse ~depth acc) l)
    in
    recurse ~depth acc t

  (* shift the non-captured De Bruijn indexes in the term by n *)
  let shift_real ?depth n t =
    assert (n >= 0);
    _fold_map ?depth ()
      ~on_bvar:(
        fun ~depth () i ->
          if i >= depth
          then i + n  (* shift *)
          else i
      )
      ~on_binder:(fun ~ty:_ ~depth:_ () _ _ -> ())
      t

  let shift ?(depth=0) n t = if depth=0 && n=0 then t else shift_real ~depth n t

  let unshift_real ?depth n t =
    _fold_map ?depth ()
      ~on_bvar:(
        fun ~depth () i ->
          if i >= depth+n then (
            i - n  (* unshift *)
          ) else i
      )
      ~on_binder:(fun ~ty:_ ~depth:_ () _ _ -> ())
      t

  let unshift ?(depth=0) n t =
    assert (n>=0);
    if depth=0 && n=0 then t else unshift_real ~depth n t

  (* recurse and replace elements of l. *)
  let rec _replace depth ~to_replace t =
    match t.ty with
    | NoType ->
      assert (t == tType);
      t
    | HasType ty ->
      let ty = _replace depth ty ~to_replace in
      match view t with
      | _ when CCList.exists (equal t) to_replace ->
        begin match CCList.find_idx (equal t) to_replace with
          | None -> assert false
          | Some (i, t') ->
            assert (equal t t');
            bvar ~ty (depth+List.length to_replace-i-1) (* replace *)
        end
      | Var v -> var (HVar.cast ~ty v)
      | DB i ->
        if i<depth
        then bvar ~ty i
        else bvar ~ty (i + List.length to_replace) (* shift *)
      | Const s -> const ~ty s
      | Bind (s, varty, t') ->
        let varty' = _replace depth ~to_replace varty in
        let t' = _replace (depth+1) t' ~to_replace in
        bind ~ty ~varty:varty' s t'
      | App (f, l) ->
        app ~ty
          (_replace depth ~to_replace f)
          (List.map (_replace depth ~to_replace) l)
      | AppBuiltin (s,l) ->
        app_builtin ~ty s (List.map (_replace depth ~to_replace) l)

  let replace_l t ~l = _replace 0 t ~to_replace:l

  let replace t ~sub = _replace 0 t ~to_replace:[sub]

  let from_var t ~var =
    assert (is_var var);
    replace t ~sub:var

  let _eval env0 t =
    let rec _eval env t = match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = _eval env ty in
        match view t with
        | Var v -> var (HVar.cast ~ty v)
        | DB i ->
          begin match DBEnv.find env i with
            | None ->
              if i >= DBEnv.size env
              then bvar ~ty (i - DBEnv.size env0) (* unshift *)
              else bvar ~ty i
            | Some t' ->
              (* type might not be exactly equal, e.g. might be equal
                 up to unifier *)
              (*assert (equal (ty_exn t') ty);*)
              (* [t'] is defined in scope 0, but there are [i-1] binders
                 between the scope where its open variables live, and
                 the current scope.
                 Therefore we must lift by [i-1].
                 The depth is the number of binders between the original [env0]
                 and current [env]. *)
              shift (DBEnv.size env - DBEnv.size env0) t'
          end
        | Const s -> const ~ty s
        | Bind (s, varty, t') ->
          let varty' = _eval env varty in
          let t' = _eval (DBEnv.push_none env) t' in
          bind ~ty ~varty:varty' s t'
        | App (f, l) ->
          app ~ty (_eval env f) (List.map (_eval env) l)
        | AppBuiltin (s,l) ->
          app_builtin ~ty s (List.map (_eval env) l)
    in
    _eval env0 t

  let eval env t =
    if DBEnv.is_empty env then t else _eval env t

  let apply_subst subst t =
    let rec aux depth t =
      match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = aux depth ty in
        aux' depth ty t
    and aux' depth ty t = match view t with
      | Var v ->
        begin
          try shift depth (VarMap.find v subst)
          with Not_found -> var (HVar.cast ~ty v)
        end
      | DB i -> bvar ~ty i
      | Const s -> const ~ty s
      | Bind (s, varty, t') ->
        let varty' = aux depth varty in
        let t' = aux (depth+1) t' in
        bind ~ty ~varty:varty' s t'
      | App (f, l) ->
        app ~ty (aux depth f) (List.map (aux depth) l)
      | AppBuiltin (s,l) ->
        app_builtin ~ty s (List.map (aux depth) l)
    in
    aux 0 t
end

let bind_vars ~ty b vars t =
  (* subst: bind vars_i to a De Bruijn (reverse list so that last element is 0) *)
  let subst =
    CCList.foldi
      (fun s i v -> VarMap.add v (bvar ~ty:(HVar.ty v) i) s)
      VarMap.empty (List.rev vars)
  in
  List.fold_right
    (fun v t ->
       let varty = HVar.ty v |> DB.apply_subst subst in
       bind ~ty ~varty b t)
    vars
    (DB.apply_subst subst t)

(** {3 Iterators} *)

module Seq = struct
  let vars t k =
    let rec vars t = match view t with
      | Var v -> k v
      | DB _
      | Const _ -> ()
      | App (head, l) -> vars head; List.iter vars l
      | AppBuiltin (_,l) -> List.iter vars l
      | Bind (_, varty, t') -> vars varty; vars t'
    in
    vars t

  let subterms t k =
    let rec subterms t =
      k t;
      match view t with
      | Var _
      | DB _
      | Const _ -> ()
      | Bind (_, varty, t') -> subterms varty; subterms t'
      | AppBuiltin (_, l) -> List.iter subterms l
      | App(f, l) -> subterms f; List.iter subterms l
    in
    subterms t

  let subterms_depth t k =
    let rec recurse depth t =
      k (t, depth);
      match view t with
      | App (_,l) ->
        let depth' = depth + 1 in
        List.iter (fun t' -> recurse depth' t') l
      | AppBuiltin (_,l) -> List.iter (recurse (depth+1)) l
      | Bind (_, varty, t') -> recurse depth varty; recurse (depth+1) t'
      | Const _
      | DB _
      | Var _ -> ()
    in
    recurse 0 t

  let symbols t k =
    let rec symbols t = match view t with
      | DB _
      | Var _ -> ()
      | Const s -> k s
      | App (head, l) -> symbols head; List.iter symbols l
      | AppBuiltin (_,l) -> List.iter symbols l
      | Bind (_, varty, t') -> symbols varty; symbols t'
    in
    symbols t

  let types t k =
    let rec types t =
      begin match t.ty with
        | NoType -> ()
        | HasType ty -> k ty
      end;
      match view t with
      | Var _ | DB _ | Const _ -> ()
      | App (head, l) -> types head; List.iter types l
      | AppBuiltin (_,l) -> List.iter types l
      | Bind (_, _, t') -> types t'
    in types t

  let max_var seq =
    let r = ref 0 in
    seq (fun i -> r := max (HVar.id i) !r);
    !r

  let min_var seq =
    let r = ref max_int in
    seq (fun i -> r := min (HVar.id i) !r);
    !r

  let add_set = Iter.fold (fun set t -> Set.add t set)

  let add_tbl tbl = Iter.iter (fun t -> Tbl.replace tbl t ())
end

(** {3 Positions} *)

module Pos = struct
  module P = Position

  let fail_ t pos =
    Util.errorf ~where:"Term.Pos"
      "@[<2>invalid position `@[%a@]`@ in term `@[%a@]`@]"
      P.pp pos debugf t

  let rec at t pos = match view t, pos with
    | _, P.Type pos' ->
      begin match t.ty with
        | NoType -> fail_ t pos
        | HasType ty -> at ty pos'
      end
    | _, P.Stop -> t
    | Var _ , _ -> fail_ t pos
    | Bind(_, _, t'), P.Body subpos -> at t' subpos
    | App (t, _), P.Head subpos -> at t subpos
    | App (_, l), P.Arg (n,subpos) when n < List.length l ->
      at (List.nth l (List.length l - 1 - n)) subpos
    | AppBuiltin (_, l), P.Arg(n,subpos) when n < List.length l ->
      at (List.nth l (List.length l - 1 - n)) subpos
    | _ -> fail_ t pos

  let rec replace t pos ~by = match t.ty, view t, pos with
    | _, _, P.Stop -> by
    | NoType, _, P.Type _ -> fail_ t pos
    | HasType ty, _, P.Type pos' ->
      let ty = replace ty pos' ~by in
      cast ~ty t
    | _, Var _, _ -> fail_ t pos
    | HasType ty, Bind(s, varty, t'), P.Body subpos ->
      bind ~ty ~varty s (replace t' subpos ~by)
    | HasType ty, App (f, l), P.Head subpos ->
      app ~ty (replace f subpos ~by) l
    | HasType ty, App (f, l), P.Arg (n,subpos) when n < List.length l ->
      let n' = List.length l - 1 - n in
      let t' = replace (List.nth l n') subpos ~by in
      let l' = CCList.set_at_idx n' t' l in
      app ~ty f l'
    | HasType ty, AppBuiltin (s,l), P.Arg (n,subpos) when n < List.length l ->
      let n' = List.length l - 1 - n in
      let t' = replace (List.nth l n') subpos ~by in
      let l' = CCList.set_at_idx n' t' l in
      app_builtin ~ty s l'
    | _ -> fail_ t pos
end

let replace_m t m =
  let rec aux depth t = match Map.get (DB.shift depth t) m with
    | Some u ->
      assert (ty_exn u == ty_exn t);
      DB.shift depth u
    | None ->
      begin match t.ty, view t with
        | HasType ty, Bind (s, varty, t') ->
          let ty = aux depth ty in
          bind ~ty ~varty s (aux (depth+1) t')
        | HasType ty, App (f, l) ->
          let ty = aux depth ty in
          let f' = aux depth f in
          let l' = List.map (aux depth) l in
          app ~ty f' l'
        | HasType ty, AppBuiltin (s,l) ->
          let ty = aux depth ty in
          let l' = List.map (aux depth) l in
          app_builtin ~ty s l'
        | NoType, _ -> t
        | _, (Var _ | DB _ | Const _) -> t
      end
  in
  aux 0 t

(* [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let replace t ~old ~by =
  let m = Map.singleton old by in
  replace_m t m

(** {3 Variables} *)

(* TODO: sort variables, so that type  variables come first *)

let close_vars ~ty s t =
  let vars = Seq.vars t |> VarSet.of_iter |> VarSet.elements in
  bind_vars ~ty s vars t

(* make the function closing over all the arguments *)
let mk_fun ~ty_l (t:t) : t =
  if ty_l=[] then t
  else (
    (* close over environment *)
    List.fold_right
      (fun varty body ->
         let ty = arrow [varty] (ty_exn body) in
         bind ~ty ~varty Binder.Lambda body)
      ty_l t
  )

let fun_ (ty_arg:t) body =
  let ty = arrow [ty_arg] (ty_exn body) in
  bind ~ty ~varty:ty_arg Binder.Lambda body

let fun_l ty_args body = List.fold_right fun_ ty_args body

let fun_of_fvars vars body =
  if vars=[] then body
  else (
    let body = DB.replace_l body ~l:(List.map var vars) in
    List.fold_right
      (fun v body -> fun_ (HVar.ty v) body)
      vars body
  )

let open_bind_fresh b t =
  let rec aux env vars t = match view t with
    | Bind (b', ty_var, body) when b=b' ->
      let v = HVar.fresh ~ty:ty_var () in
      let env = DBEnv.push env (var v) in
      aux env (v::vars) body
    | _ ->
      let t' = DB.eval env t in
      List.rev vars, t'
  in
  aux DBEnv.empty [] t

let open_bind_fresh2 ?(eq_ty=equal) b t1 t2 =
  let rec aux env vars t1 t2 = match view t1, view t2 with
    | Bind (b1, ty_var1, body1), Bind (b2, ty_var2, body2)
      when b1=b && b2=b && eq_ty ty_var1 ty_var2 ->
      let v = HVar.fresh ~ty:ty_var1 () in
      let env = DBEnv.push env (var v) in
      aux env (v::vars) body1 body2
    | _ ->
      let t1 = DB.eval env t1 in
      let t2 = DB.eval env t2 in
      List.rev vars, t1, t2
  in
  aux DBEnv.empty [] t1 t2

let rec open_poly_fun ty = match view ty with
  | Bind (Binder.ForallTy, _, ty') ->
    let i, args, ret = open_poly_fun ty' in
    i+1, args, ret
  | _ ->
    let args, ret = open_fun ty in
    0, args, ret

let rec returns ty = match view ty with
  | Bind (Binder.ForallTy, _, ty') ->
    returns ty'
  | AppBuiltin (Builtin.Arrow, ret :: _) -> ret
  | _ -> ty

let rec expected_ty_vars ty = match view ty with
  | Bind (Binder.ForallTy, _, ty') -> 1 + expected_ty_vars ty'
  | _ -> 0

let is_ground t = 
  not @@ get_property t.props f_has_freevars 

(** {3 Misc} *)

let rec size t = match view t with
  | Const _
  | Var _
  | DB _ -> 1
  | Bind (_, _, t') -> 1 + size t'
  | AppBuiltin (_,l) -> List.fold_left (fun s t -> s+size t) 1 l
  | App (head, l) -> _size_list (1 + size head) l
and _size_list acc l = match l with
  | [] -> acc
  | t::l' -> _size_list (acc + size t) l'

let depth t =
  Seq.subterms_depth t |> Iter.map snd |> Iter.fold max 0

let rec head t = match view t with
  | Const s -> Some s
  | DB _ | Var _ | Bind (_, _, _) | AppBuiltin (_, _) -> None
  | App (h, _) -> head h

let type_is_unifiable (ty:t): bool = match view ty with
  | AppBuiltin ((Builtin.TyInt | Builtin.TyRat), _)
  | Bind (Binder.ForallTy, _, _) -> false
  | _ -> true

let type_non_unifiable_tags (ty:t): _ list = match view ty with
  | AppBuiltin (Builtin.TyInt,_) -> [Builtin.Tag.T_lia; Builtin.Tag.T_cannot_orphan]
  | AppBuiltin (Builtin.TyRat,_) -> [Builtin.Tag.T_lra; Builtin.Tag.T_cannot_orphan]
  | Bind (Binder.ForallTy, _, _) -> [Builtin.Tag.T_ho]
  | _ -> []

let type_is_prop t = match view t with AppBuiltin (Builtin.Prop, _) -> true | _ -> false

let [@inline] get_type t = match ty t with
  | HasType ty -> ty
  | NoType -> invalid_arg "must have type!"

let[@inline] as_app t = match view t with
  | App (f,l) -> 
    begin match view f with 
      | AppBuiltin(b, l') -> app_builtin b ~ty:(ty_exn t) (l'@l), []
      | _ -> f, l 
    end
  | AppBuiltin(b, l) when (not (Builtin.is_quantifier b)) ->
    let ty_args, args = CCList.partition is_a_type l in
    let ty = arrow (List.map ty_exn args) (ty_exn t) in 
    app_builtin ~ty b ty_args, args
  | _ -> t, []

let[@inline] as_var t = match view t with Var v -> Some v | _ -> None
let[@inline] as_var_exn t = match view t with Var v -> v | _ -> invalid_arg "as_var_exn"

let as_const t = match view t with Const v -> Some v | _ -> None
let as_const_exn t = match view t with Const v -> v | _ -> invalid_arg "as_const_exn"

let[@inline] as_bvar_exn t = match view t with DB i -> i | _ -> invalid_arg "as_bvar_exn"
let[@inline] is_bvar_i i t = match view t with DB j -> i=j | _ -> false

(** {3 IO} *)

let print_hashconsing_ids = ref false
let print_all_types = ref false

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

let _hooks = ref []
let add_default_hook h = _hooks := h :: !_hooks
let default_hooks() = !_hooks

let needs_args (t:t): bool = match view t with
  | AppBuiltin (Builtin.Arrow, _) -> true
  | Bind (Binder.ForallTy, _, _) -> true
  | _ -> false

let show_type_arguments = ref false

let rec open_bind2 b t1 t2 = match view t1, view t2 with
  | Bind (b1', ty1, t1'), Bind (b2', ty2, t2') when b=b1' && b=b2' ->
    let args1, ret1, args2, ret2 = open_bind2 b t1' t2' in
    ty1 :: args1, ret1, ty2 :: args2, ret2
  | _ -> [], t1, [], t2

let rec pp_depth ?(hooks=[]) depth out t =
  let rec _pp depth out t =
    if List.exists (fun h -> h depth (_pp depth) out t) hooks
    then () (* hook took control *)
    else (
      _pp_root depth out t ;
      if !print_hashconsing_ids then (
        Format.fprintf out "/%d" t.id
      );
    )
  and _pp_root depth out t = match view t with
    | Var v ->
      pp_var out v;
      if !print_all_types then (
        Format.fprintf out ":%a" (_pp_surrounded depth) (ty_exn t)
      );
    | DB i ->
      Format.fprintf out "Y%d" (depth-i-1);
      if !print_all_types then (
        Format.fprintf out ":%a" (_pp_surrounded depth) (ty_exn t)
      );
    | Const s ->
      begin match ID.as_prefix s with
        | Some s -> CCFormat.string out s
        | None -> ID.pp out s
      end;
    | Bind (b, _, _) ->
      (* unfold *)
      let varty_l, t' = open_bind b t in
      let pp_tyvar out (i,varty) =
        Format.fprintf out "(@[Y%d:@[%a@])@]" (depth+i) (_pp depth) varty
      in
      Format.fprintf out "@[<1>%a@ @[%a@].@ %a@]"
        Binder.pp b
        (Util.pp_iter ~sep:" " pp_tyvar)
        (Iter.of_array_i (Array.of_list varty_l))
        (_pp_surrounded (depth+List.length varty_l)) t'
    | AppBuiltin (Builtin.Arrow, ([] | [_])) -> assert false
    | AppBuiltin (Builtin.Arrow, ret::args) ->
      Format.fprintf out "@[%a@ → %a@]"
        (Util.pp_list ~sep:" → " (_pp_surrounded depth)) args
        (_pp_surrounded depth) ret
    | AppBuiltin((Builtin.ExistsConst | Builtin.ForallConst) as b, [x;body]) ->
      Format.fprintf out "%a %a. %a" Builtin.pp b (_pp depth) x (_pp depth) body;
    | AppBuiltin((Builtin.Eq | Builtin.Neq) as b,  x :: rest) ->
      let sep, l = 
        if is_a_type x then (
          CCFormat.sprintf "(%a::%a) " Builtin.pp b (_pp depth) x,rest
        ) else (
          CCFormat.sprintf "%a " Builtin.pp b, x :: rest
        ) in
      if CCList.length l = 1 then Format.fprintf out "(%a @[%a@])" Builtin.pp b (_pp depth) (List.hd l)
      else Format.fprintf out " @[%a@]" (Util.pp_list ~sep (_pp depth)) l
    | AppBuiltin (b, ([_;a] | [a])) when Builtin.is_prefix b ->
      Format.fprintf out "@[<1>%a %a@]" Builtin.pp b (_pp depth) a
    | AppBuiltin (b, [t1;t2]) when Builtin.is_infix b ->
      Format.fprintf out "(@[%a %s@ %a@])" (_pp depth) t1 (Builtin.to_string b) (_pp depth) t2
    | AppBuiltin (b, [_ty;t1;t2]) when Builtin.is_infix b && is_tType (ty_exn _ty) ->
      (* always drop the type argument, it's always inferrable for builtins *)
      Format.fprintf out "(@[%a %s@ %a@])" (_pp depth) t1 (Builtin.to_string b) (_pp depth) t2
    | AppBuiltin (b, l) when Builtin.is_infix b && List.length l >= 2 ->
      let sep = CCFormat.sprintf " %s " (Builtin.to_string b) in
      Format.fprintf out "(@[%a@])" (Util.pp_list ~sep (_pp depth)) l
    | AppBuiltin (b, []) -> Builtin.pp out b
    | AppBuiltin (b, l) ->
      let l = 
        if Builtin.is_combinator b && not !show_type_arguments
        then List.filter (fun t -> not (is_tType @@ ty_exn t)) l
        else l in
      if CCList.is_empty l then Format.fprintf out "@[%a@]" Builtin.pp b
      else Format.fprintf out "@[%a(%a)@]" Builtin.pp b (Util.pp_list (_pp depth)) l
    | App (f, l) ->
      (* remove type arguments unless required,
         or unless we are already printing a type *)
      let l =
        if !show_type_arguments || is_tType (ty_exn t) then l
        else List.filter (fun t -> not (is_tType @@ ty_exn t)) l
      in
      let as_infix = match view f with Const id -> ID.as_infix id | _ -> None in
      let as_prefix = match view f with Const id -> ID.as_prefix id | _ -> None in
      begin match as_infix, as_prefix, l with
        | _, _, [] -> _pp depth out f
        | Some s, _, [a;b] ->
          Format.fprintf out "@[<1>%a@ %s@ %a@]"
            (_pp_surrounded depth) a s (_pp_surrounded depth) b
        | _, Some s, [a] ->
          Format.fprintf out "@[<1>%s@ %a@]" s (_pp_surrounded depth) a
        | _ ->
          Format.fprintf out "@[<1>%a@ %a@]"
            (_pp_surrounded depth) f (Util.pp_list ~sep:" " (_pp_surrounded depth)) l
      end
  and _pp_surrounded depth out t = match view t with
    | App (_, l)
      when not !show_type_arguments &&
           not (is_tType (ty_exn t)) &&
           List.for_all (fun t -> is_tType (ty_exn t)) l ->
      _pp depth out t
    | Bind _
    | AppBuiltin (_,_::_)
    | App (_,_::_) -> Format.fprintf out "(@[%a@])" (_pp depth) t
    | _ -> _pp depth out t
  in
  _pp depth out t
and pp_var out v =
  let ty = HVar.ty v in
  begin match view ty with
    | AppBuiltin (Builtin.TType, []) -> Format.fprintf out "A%d" (HVar.id v)
    | AppBuiltin (Builtin.TyInt, []) -> Format.fprintf out "I%d" (HVar.id v)
    | AppBuiltin (Builtin.TyRat, []) -> Format.fprintf out "Q%d" (HVar.id v)
    | AppBuiltin (Builtin.Prop, []) -> Format.fprintf out "P%d" (HVar.id v)
    | _ when needs_args ty -> Format.fprintf out "F%d" (HVar.id v)
    | _ -> HVar.pp out v
  end

let pp out t = pp_depth ~hooks:!_hooks 0 out t
let to_string t = CCFormat.to_string pp t

let rec pp_zf out t =
  let rec pp_ depth out t = match view t with
    | Var v -> pp_var_zf out v
    | DB i -> Format.fprintf out "Y%d" (depth-i-1)
    | Const s -> ID.pp_zf out s
    | Bind (b, _, _) ->
      (* unfold *)
      let varty_l, t' = open_bind b t in
      let pp_tyvar out (i,varty) =
        Format.fprintf out "(@[Y%d:@[%a@])@]" (depth+i) (pp_ depth) varty
      in
      Format.fprintf out "@[<1>%a@ @[%a@].@ %a@]"
        Binder.ZF.pp b
        (Util.pp_iter ~sep:" " pp_tyvar)
        (Iter.of_array_i (Array.of_list varty_l))
        (_pp_surrounded (depth+List.length varty_l)) t'
    | AppBuiltin (Builtin.Arrow, ([] | [_])) -> assert false
    | AppBuiltin (Builtin.Arrow, ret::args) ->
      Format.fprintf out "@[%a@ -> %a@]"
        (Util.pp_list ~sep:" -> " (_pp_surrounded depth)) args
        (_pp_surrounded depth) ret
    | AppBuiltin(b, [x;body]) when Builtin.equal b Builtin.ExistsConst ||
                                   Builtin.equal b Builtin.ForallConst ->
      Format.printf "%a %a. %a" Builtin.pp b pp_zf x pp_zf body;
    | AppBuiltin (b, ([_;a] | [a])) when Builtin.is_prefix b ->
      Format.fprintf out "@[<1>%a %a@]" Builtin.ZF.pp b (pp_ depth) a
    | AppBuiltin (b, [t1;t2]) when Builtin.is_infix b ->
      Format.fprintf out "(@[%a %a@ %a@])" (pp_ depth) t1 Builtin.ZF.pp b (pp_ depth) t2
    | AppBuiltin (b, [_ty;t1;t2]) when Builtin.is_infix b && is_tType (ty_exn _ty) ->
      (* always drop the type argument, it's always inferrable for builtins *)
      Format.fprintf out "(@[%a %a@ %a@])" (pp_ depth) t1 Builtin.ZF.pp b (pp_ depth) t2
    | AppBuiltin (b, l) when List.length l >= 2 && Builtin.is_infix b ->
      let sep = CCFormat.sprintf " %s " (Builtin.to_string b) in
      Format.fprintf out "(@[%a@])" (Util.pp_list ~sep pp_zf) l
    | AppBuiltin (b, []) -> Builtin.ZF.pp out b
    | AppBuiltin (b, l) ->
      Format.fprintf out "@[%a(%a)@]" Builtin.ZF.pp b (Util.pp_list (pp_ depth)) l
    | App (f, l) ->
      begin match l with
        | [] -> pp_ depth out f
        | _::_ ->
          Format.fprintf out "@[<1>%a@ %a@]"
            (_pp_surrounded depth) f (Util.pp_list ~sep:" " (_pp_surrounded depth)) l
      end
  and _pp_surrounded depth out t = match view t with
    | Bind _
    | AppBuiltin (_,_::_)
    | App (_,_::_) -> Format.fprintf out "(@[%a@])" (pp_ depth) t
    | _ -> pp_ depth out t
  in
  pp_ 0 out t
and pp_var_zf out v =
  let ty = HVar.ty v in
  begin match view ty with
    | AppBuiltin (Builtin.TType, []) -> Format.fprintf out "A%d" (HVar.id v)
    | AppBuiltin (Builtin.TyInt, []) -> Format.fprintf out "I%d" (HVar.id v)
    | AppBuiltin (Builtin.TyRat, []) -> Format.fprintf out "Q%d" (HVar.id v)
    | AppBuiltin (Builtin.Prop, []) -> Format.fprintf out "P%d" (HVar.id v)
    | _ when needs_args ty -> Format.fprintf out "F%d" (HVar.id v)
    | _ -> HVar.pp out v
  end

let pp_in = function
  | Output_format.O_zf -> pp_zf
  | Output_format.O_tptp -> assert false
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent
