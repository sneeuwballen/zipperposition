(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms} *)

module PB = Position.Build
module PW = Position.With
module T = InnerTerm

let prof_ac_normal_form = ZProf.make "term.AC_normal_form"

(** {2 Term} *)

type t = T.t
type term = t
type var = Type.t HVar.t

type view =
   | AppBuiltin of Builtin.t * t list
   | DB of int  (** Bound variable (De Bruijn index) *)
   | Var of var  (** Term variable *)
   | Const of ID.t  (** Typed constant *)
   | App of t * t list  (** Application to a list of terms (cannot be left-nested) *)
   | Fun of Type.t * t  (** Lambda abstraction *)

let view t =
   match T.view t with
      | T.AppBuiltin (b, l) -> AppBuiltin (b, l)
      | T.Var v -> Var (Type.cast_var_unsafe v)
      | T.DB i -> DB i
      | T.App (_, []) -> assert false
      | T.App (f, l) -> App (f, l)
      | T.Const s -> Const s
      | T.Bind (Binder.Lambda, ty, t') -> Fun (Type.of_term_unsafe ty, t')
      | _ -> assert false

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
   let rec check t =
      T.equal sub t
      ||
      match T.view t with
         | T.Var _ | T.DB _ | T.Const _ -> false
         | T.App (f, l) -> check f || List.exists check l
         | T.Bind (_, _, t') -> check t'
         | T.AppBuiltin (_, l) -> List.exists check l
   in
      check t

let equal = T.equal
let hash = T.hash
let compare = T.compare
let[@inline] ty t = match T.ty t with T.NoType -> assert false | T.HasType ty -> Type.of_term_unsafe ty
let hash_mod_alpha = T.hash_mod_alpha
let same_l = T.same_l
let same_l_gen = T.same_l_gen

(* split list between types, terms.
   [ty] is the type of the function, [l] the arguments *)
let rec split_args_ ~ty l =
   match (Type.view ty, l) with
      | Type.Forall ty', x :: l' ->
         let l1, l2 = split_args_ ~ty:ty' l' in
            (x :: l1, l2)
      | _ -> ([], l)

module Classic = struct
  type view =
     | Var of var
     | DB of int
     | App of ID.t * t list  (** covers Const and App *)
     | AppBuiltin of Builtin.t * t list
     | NonFO  (** any other case *)

  let view t : view =
     match T.view t with
        | T.Var v -> Var (Type.cast_var_unsafe v)
        | T.DB i -> DB i
        | _ when not (Type.is_unifiable @@ ty t) -> NonFO
        | T.Const s -> App (s, [])
        | T.AppBuiltin (b, l) -> AppBuiltin (b, l)
        | T.App (f, l) -> ( match T.view f with T.Const id -> App (id, l) | _ -> NonFO)
        | T.Bind (Binder.Lambda, _, _) -> NonFO
        | T.Bind (_, _, _) -> assert false
end

module IntMap = Map.Make (CCInt)
(** {2 Containers} *)

module Tbl = T.Tbl
module Set = T.Set
module Map = T.Map
module VarSet = Type.VarSet
module VarMap = Type.VarMap
module VarTbl = Type.VarTbl

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags). *)

let var = (T.var :> var -> T.t)

let var_of_int ~ty i =
   let ty = (ty : Type.t :> T.t) in
      T.var (HVar.make ~ty i)

let builtin ~ty b = T.builtin ~ty:(ty : Type.t :> T.t) b
let app_builtin ~ty b l = T.app_builtin ~ty:(ty : Type.t :> T.t) b l

let bvar ~ty i =
   assert (i >= 0);
   T.bvar ~ty:(ty : Type.t :> T.t) i

let const ~ty s = T.const ~ty:(ty : Type.t :> T.t) s

let tyapp t args =
   match args with
      | [] -> t
      | _ :: _ ->
         let args' = (args : Type.t list :> T.t list) in
         let ty = (Type.apply (ty t) args : Type.t :> T.t) in
            T.app ~ty t args'

let app f l =
   match l with
      | [] -> f
      | _ :: _ ->
         (* first; compute type *)
         (*Printf.printf "we got f be %s\n" (Type.to_string (ty f)) ;*)
         let ty_result = Type.apply_unsafe (ty f) l in
         (* apply constant to type args and args *)
         let res = T.app ~ty:(ty_result : Type.t :> T.t) f l in
            res

let app_w_ty ~ty f l =
   match l with
      | [] -> f
      | _ :: _ ->
         (* first; compute type *)
         let ty_result = Type.apply_unsafe ty l in
         (* apply constant to type args and args *)
         let res = T.app ~ty:(ty_result : Type.t :> T.t) f l in
            res

let app_full f tyargs l =
   let l = (tyargs : Type.t list :> T.t list) @ l in
      app f l

let fun_ (ty_arg : Type.t) body = T.fun_ (ty_arg :> T.t) body
let fun_l ty_args body = List.fold_right fun_ ty_args body

let fun_of_fvars vars body =
   let vars = (vars : Type.t HVar.t list :> T.t HVar.t list) in
      T.fun_of_fvars vars body

let open_fun t =
   let tys, bod = T.open_bind Binder.Lambda t in
      (Type.of_terms_unsafe tys, bod)

let open_fun_offset ~offset t =
   let rec aux offset env vars t =
      match view t with
         | Fun (ty_var, body) ->
            let v = HVar.make offset ~ty:ty_var in
            let env = DBEnv.push env (var v) in
               aux (offset + 1) env (v :: vars) body
         | _ ->
            let t' = T.DB.eval env t in
               (List.rev vars, t', offset)
   in
      aux offset DBEnv.empty [] t

let true_ = builtin ~ty:Type.prop Builtin.True
let false_ = builtin ~ty:Type.prop Builtin.False
let grounding ty = builtin ~ty Builtin.Grounding

let is_formula t =
   match T.view t with
      | T.AppBuiltin (hd, _) ->
         List.mem hd
           [
             Builtin.And;
             Builtin.Or;
             Builtin.Not;
             Builtin.Imply;
             Builtin.Equiv;
             Builtin.Xor;
             Builtin.ForallConst;
             Builtin.ExistsConst;
           ]
      | _ -> false

let is_var t = match T.view t with T.Var _ -> true | _ -> false
let is_bvar t = match T.view t with T.DB _ -> true | _ -> false
let is_const t = match T.view t with T.Const _ -> true | _ -> false
let is_appbuiltin t = match T.view t with T.AppBuiltin _ -> true | _ -> false
let is_fun t = match T.view t with T.Bind (Binder.Lambda, _, _) -> true | _ -> false
let hd_is_comb = Builtin.is_combinator
let[@inline] is_comb t = match view t with AppBuiltin (hd, _) when hd_is_comb hd -> true | _ -> false
let is_app t = match T.view t with T.Const _ | T.App _ -> true | _ -> false
let is_type t = Type.equal Type.tType (ty t)

let as_const_exn t =
   match T.view t with T.Const c -> c | _ -> invalid_arg (CCFormat.sprintf "as_const_exn: %a" T.pp t)

let as_const t = try Some (as_const_exn t) with Invalid_argument _ -> None
let as_var_exn t = match T.view t with T.Var v -> Type.cast_var_unsafe v | _ -> invalid_arg "as_var_exn"
let as_var t = try Some (as_var_exn t) with Invalid_argument _ -> None
let as_app = T.as_app
let as_bvar_exn = T.as_bvar_exn

let rec as_fun t =
   match view t with
      | Fun (ty_arg, bod) ->
         let args, ret = as_fun bod in
            (ty_arg :: args, ret)
      | _ -> ([], t)

let head_term t = fst (as_app t)
let args t = snd (as_app t)


let is_app_var t = (is_var @@ head_term t) && List.length @@ args t > 0

let head_term_mono t =
   match view t with
      | App (f, l) ->
         let l1 = CCList.take_while is_type l in
            app f l1 (* re-apply to type parameters *)
      | AppBuiltin (b, l) ->
         let ty_args, args = CCList.partition is_type l in
         let ty = Type.arrow (List.map ty args) (ty t) in
            app_builtin ~ty b ty_args
      | _ -> t

let ty_args t = 
   (*function symbol and type arguments*)
   let instantiated_fun_sym = head_term_mono t in
   let ty_args = args instantiated_fun_sym in
   List.map Type.of_term_unsafe ty_args


let as_app_mono t =
   match view t with
      | App (f, l) ->
         assert (match view f with AppBuiltin _ -> false | _ -> true);
         let l1, l2 = CCList.partition is_type l in
            (app f l1, l2 (* re-apply to type parameters *))
      | AppBuiltin (b, l) ->
         let ty_args, args = CCList.partition is_type l in
         let ty = Type.arrow (List.map ty args) (ty t) in
            (app_builtin ~ty b ty_args, args)
      | _ -> (t, [])

let is_ho_var t = match view t with Var v -> Type.needs_args (HVar.ty v) | _ -> false

let as_ho_app t =
   let hd, args = as_app t in
      match as_var hd with Some v when args <> [] -> Some (v, args) | _ -> None

let is_ho_app t = CCOpt.is_some (as_ho_app t)
let is_ho_pred t = is_ho_app t && Type.is_prop (ty t)
let is_ho_at_root t = is_ho_var t || is_ho_app t

let rec all_combs = function
   | [] -> []
   | x :: xs ->
      let rest_combs = all_combs xs in
         if CCList.is_empty rest_combs then CCList.map (fun t -> [ t ]) x
         else CCList.flat_map (fun i -> CCList.map (fun comb -> i :: comb) rest_combs) x

let rec cover_with_terms ?(depth = 0) ?(recurse = true) t ts =
   let n = List.length ts in
   let db =
      CCList.mapi (fun i x -> if CCOpt.is_some x then (i, CCOpt.get_exn x) else (-1, false_)) ts
      |> CCList.filter_map (fun (i, x) ->
             if i != -1 && equal x t then (
               assert (Type.equal (ty x) (ty t));
               Some (bvar ~ty:(ty t) (n - 1 - i + depth)))
             else None)
   in
   let rest =
      if recurse then
        match view t with
           | AppBuiltin (hd, args) ->
              if CCList.is_empty args then [ app_builtin ~ty:(ty t) hd [] ]
              else
                let args' = List.map (fun a -> cover_with_terms ~depth a ts) args in
                let args_combined = all_combs args' in
                   List.map (fun args -> app_builtin ~ty:(ty t) hd args) args_combined
           | App (_, args) ->
              assert (not (CCList.is_empty args));
              let hd, args = (head_term_mono t, CCList.drop_while is_type args) in
              let hd' = cover_with_terms ~recurse:false hd ts in
              let args' = List.map (fun a -> cover_with_terms ~depth a ts) args in
              let args_combined = all_combs (hd' :: args') in
                 List.map (fun l -> app (List.hd l) (List.tl l)) args_combined
           | Fun (ty_var, body) ->
              let bodies = cover_with_terms ~depth:(depth + 1) body ts in
                 assert (not (CCList.is_empty bodies));
                 List.map (fun b -> fun_ ty_var b) bodies
           | _ -> [ t ]
      else [ t ]
   in
      db @ rest

let max_cover t ts =
   let rec aux depth t =
      let hit = CCList.find_mapi (fun i x -> match x with Some t' when equal t t' -> Some i | _ -> None) ts in
         match hit with
            | Some idx -> bvar ~ty:(ty t) (List.length ts - 1 - idx + depth)
            | None -> (
               match view t with
                  | AppBuiltin (hd, args) ->
                     let args' = List.map (fun arg -> aux depth arg) args in
                        app_builtin ~ty:(ty t) hd args'
                  | App (hd, args) ->
                     let args' = List.map (fun arg -> aux depth arg) args in
                        app (aux depth hd) args'
                  | Fun (ty_var, body) ->
                     let body' = aux (depth + 1) body in
                        fun_ ty_var body'
                  | DB _ | Var _ | Const _ -> t)
   in
      aux 0 t

module Seq = struct
  let vars t k =
     let rec aux t =
        Type.Seq.vars (ty t) k;
        aux_term t
     and aux_term t =
        match view t with
           | Var v -> k v
           | Const _ | DB _ -> ()
           | Fun (_, u) -> aux_term u
           | App (f, l) ->
              aux f;
              List.iter aux l
           | AppBuiltin (_, l) -> List.iter aux l
     in
        aux t

  let subterms ?(include_builtin = false) ?(include_app_vars = true) ?(ignore_head = false) t k =
     let rec aux t =
        k t;
        match view t with
           | AppBuiltin (_, l) -> if include_builtin then List.iter aux l
           | Const _ | Var _ | DB _ -> ()
           | App (f, l) when (not include_app_vars) && T.is_var f -> ()
           | App (f, l) ->
              if not ignore_head then aux f;
              List.iter aux l
           | Fun (_, u) -> aux u
     in
        aux t

  let subterms_depth ?(filter_term = fun _ -> true) t k =
     let rec recurse depth t =
        if filter_term t then (
          k (t, depth);
          match view t with
             | Const _ | DB _ | Var _ -> ()
             | Fun (_, u) -> recurse (depth + 1) u
             | AppBuiltin (_, l) -> List.iter (recurse (depth + 1)) l
             | App (_, l) ->
                let depth' = depth + 1 in
                   List.iter (recurse depth') l)
     in
        recurse 0 t

  let symbols ?(include_types = false) ?(filter_term = fun _ -> true) t k =
     if include_types then Type.Seq.symbols (ty t) k;

     let rec aux t =
        if filter_term t then
          match view t with
             | AppBuiltin (_, l) -> List.iter aux l
             | Const s -> k s
             | Var _ | DB _ -> ()
             | Fun (ty, u) ->
                if include_types then Type.Seq.symbols ty k;
                  aux u
             | App (f, l) ->
                aux f; List.iter aux l
     in
        aux t

  let max_var = Type.Seq.max_var
  let min_var = Type.Seq.min_var
  let add_set set xs = Iter.fold (fun set x -> Set.add x set) set xs
  let ty_vars t = subterms ~include_builtin:true t |> Iter.flat_map (fun t -> Type.Seq.vars (ty t))

  let typed_symbols ?(include_types = false) t =
     (*Printf.printf "our lad t: %s\n" (T.to_string t);*)
     let non_ty_syms = subterms t |> Iter.filter_map (fun t -> match T.view t with T.Const s -> Some (s, ty t) | _ -> None) in
     let ty_syms =
        if include_types then 
         Iter.flat_map (fun t -> Type.Seq.symbols (ty t)) (subterms t) |> Iter.map (fun ty_id -> ty_id, Type.tType)
        else Iter.empty
     in
     (*Printf.printf "the type of our lad t: %s\n" (Type.to_string (ty t));*)
     (*Iter.iter (fun (id, _) -> Printf.printf "id: %s\n" (ID.to_string id)) ty_syms;*)
        Iter.append non_ty_syms ty_syms

  let common_contexts a b k =
     let common_arg xs ys =
        let rec aux acc xs ys =
           match xs with
              | x :: xs' -> (
                 match ys with
                    | y :: ys' ->
                       if (not (equal x y)) && CCOpt.is_none acc then aux (Some (x, y)) xs' ys'
                       else if equal x y then aux acc xs' ys'
                       else None
                    | _ -> assert false)
              | _ ->
                 assert (CCList.is_empty ys);
                 acc
        in
           aux None xs ys
     in

     let rec aux a b =
        match (view a, view b) with
           | App (hda, argsa), App (hdb, argsb) when Type.equal (ty a) (ty b) -> (
              if is_const hda && equal hda hdb then
                match common_arg argsa argsb with
                   | Some (a, b) ->
                      k (a, b);
                      aux a b
                   | None -> ())
           | _ -> ()
     in
        aux a b
end

let has_ho_subterm t =
   (not (equal true_ t))
   && (not (equal false_ t))
   && Seq.subterms ~include_builtin:true ~ignore_head:true t
      |> Iter.exists (fun st -> (not (T.equal st t)) && (Type.is_fun (ty st) || Type.is_prop (ty st)))

let close_quantifier b ty_args body =
   CCList.fold_right
     (fun ty acc -> app_builtin ~ty:Type.prop b [ (ty : Type.t :> T.t); fun_ ty acc ])
     ty_args body

let var_occurs ~var t = Iter.exists (HVar.equal Type.equal var) (Seq.vars t)

let rec size t =
   match view t with
      | Var _ | DB _ -> 1
      | AppBuiltin (_, l) | App (_, l) -> List.fold_left (fun s t' -> s + size t') 1 l
      | Fun (_, u) -> 1 + size u
      | Const _ -> 1

let weight ?(var = 1) ?(sym = fun _ -> 1) t =
   let rec weight t =
      if Type.is_tType (ty t) then 0
      else
        match view t with
           | Var _ | DB _ -> var
           | AppBuiltin ((ForallConst | ExistsConst), [ _; body ]) ->
              let _, body = open_fun body in
                 1 + weight body
           | AppBuiltin (_, l) -> List.fold_left (fun s t' -> s + weight t') 1 l
           | App (hd, l) -> List.fold_left (fun s t' -> s + weight t') (weight hd) l
           | Fun (_, u) -> 1 + weight u
           | Const s -> sym s
   in
      weight t

let ho_weight = T.ho_weight
let is_ground t = T.is_ground t
let is_beta_reducible t = T.is_beta_reducible t
let has_lambda t = T.has_lambda t
let of_term_unsafe t = t
let of_term_unsafe_l l = l
let of_ty t = (t : Type.t :> T.t)

let is_linear t =
   let var_set = VarTbl.create 8 in

   let rec aux t =
      match view t with
         | AppBuiltin (_, args) -> List.for_all aux args
         | App (hd, args) -> List.for_all aux (hd :: args)
         | Fun (_, body) -> aux body
         | Var v ->
            if VarTbl.mem var_set v then false
            else (
              VarTbl.add var_set v ();
              true)
         | _ -> true
   in

   let res = aux t in
      VarTbl.clear var_set;
      res

let rec in_pfho_fragment t =
   match view t with
      | Var _ ->
         if not (type_ok (ty t)) then
           raise (Failure (CCFormat.sprintf "Variable has out-of-fragment type [%a]" T.pp t))
         else true
      | Const sym ->
         if top_level_exception t || type_ok (ty t) then true
         else raise (Failure (CCFormat.sprintf "Constant has out-of-fragment type [%a] " ID.pp sym))
      | AppBuiltin (_, l) | App (_, l) ->
         if
           (top_level_exception t || type_ok (ty t))
           && List.map ty l |> List.for_all type_ok
           && List.for_all in_pfho_fragment l
         then true
         else
           raise
             (Failure
                (CCFormat.sprintf "Argument of a term has out-of-fragment type [%a:%a]" T.pp t Type.pp (ty t)))
      | Fun (var_t, body) ->
         if type_ok var_t && type_ok (ty body) && in_pfho_fragment body then true
         else raise (Failure (CCFormat.sprintf "Lambda body has out-of-fragment type [%a]" T.pp t))
      | DB _ -> if type_ok (ty t) then true else raise (Failure "Bound variable has out-of-fragment type")

and top_level_exception t =
   (* If the head is a variable or skolem, the type must be ok.
      But if the head is a constant, we want to allow predicate symbols. *)
   let hd = head_term t in
   let hd_is_skolem = match as_const hd with Some sym -> ID.is_skolem sym | None -> false in
      if is_var hd || hd_is_skolem then false else if Type.equal (ty t) Type.prop then true else false

and type_ok ty_ =
   not
     (Type.Seq.sub ty_
     |> Iter.exists (fun t -> Type.equal t Type.prop || Type.equal t Type.rat || Type.equal t Type.int))

let in_lfho_fragment t =
   in_pfho_fragment t
   && Seq.subterms t |> fun subts ->
      if Iter.for_all (fun subt -> not (is_fun subt)) subts then true
      else raise (Failure "Term contains a lambda")

let rec is_fo_term t =
   match view t with
      | Var _ -> not (Type.is_fun (ty t) || Type.is_prop (ty t))
      | AppBuiltin _ -> equal t true_ || equal t false_
      | App (hd, l) ->
         (not (Type.is_fun (ty t)))
         && T.is_const hd
         && List.for_all (fun t -> (not (Type.is_prop (ty t) || Type.is_fun (ty t))) && is_fo_term t) l
      | Const _ -> not (Type.is_fun (ty t))
      | _ -> false

let in_fool_fragment t =
   let fool_subterm_found = ref false in

   let rec aux ~top t =
      if (not top) && Type.is_prop (ty t) then fool_subterm_found := true;

      (not (Type.is_fun (ty t)))
      &&
      match view t with
         | AppBuiltin (b, l) ->
            if Builtin.is_logical_op b || Builtin.equal Builtin.Eq b || Builtin.equal Builtin.Neq b then (
              fool_subterm_found := true;
              List.for_all (aux ~top:false) l)
            else Builtin.equal Builtin.True b || Builtin.equal Builtin.False b
         | App (hd, l) -> T.is_const hd && List.for_all (aux ~top:false) l
         | Var _ ->
            if Type.is_prop (ty t) then fool_subterm_found := true;
            true
         | Const _ -> true
         | _ -> false
   in
      (if not (aux ~top:true t) then
         let err_msg = CCFormat.sprintf "%a is not a fool term" T.pp t in
            raise (Failure err_msg));
      (true, !fool_subterm_found)

let is_true_or_false t =
   match view t with
      | AppBuiltin (b, _) -> CCList.mem ~eq:Builtin.equal b [ Builtin.True; Builtin.False ]
      | _ -> false

let inc_depth = function None -> Some 0 | Some x -> Some (x + 1)
let max_d a b = match a with None -> b | Some x -> ( match b with Some y when y > x -> b | _ -> a)

let max_d_l =
   let rec max_d_l_aux acc = function [] -> acc | x :: xs -> max_d_l_aux (max_d x acc) xs in
      max_d_l_aux None

let lambda_depth t =
   let rec aux acc t =
      match view t with
         | AppBuiltin (_, l) -> max_d_l (List.map (aux acc) l)
         | App (hd, l) -> max_d_l (List.map (aux acc) (hd :: l))
         | Fun (_, u) -> aux (inc_depth acc) u
         | Var _ | DB _ | Const _ -> acc
   in
   let res = aux None t in
      (* CCFormat.printf "l_depth(@[%a@])=@[%a@]@." T.pp t (CCOpt.pp CCInt.pp) res; *)
      res

let comb_depth t =
   (* comb streak is true if while traversing the term, we went
      only through terms that have combinators for heads *)
   let rec aux ~comb_streak acc t =
      match view t with
         | AppBuiltin (b, l) when Builtin.is_combinator b ->
            let acc, comb_streak =
               if comb_streak then
                 ( (* if up to this point we have been seeing only combinators,
                      do not increase the depth *)
                   acc,
                   comb_streak )
               else ((* new comb_streak begins *)
                     inc_depth acc, true)
            in

            max_d_l (List.map (aux ~comb_streak acc) l)
         | AppBuiltin (_, l) -> max_d_l (List.map (aux ~comb_streak:false acc) l)
         | App (hd, l) -> max_d_l (List.map (aux ~comb_streak:false acc) (hd :: l))
         | Fun (_, u) -> invalid_arg "lambdas should have been removed."
         | Var _ | DB _ | Const _ -> acc
   in

   let res = aux ~comb_streak:false None t in
      (* CCFormat.printf "c_depth(@[%a@])=@[%a@]@." T.pp t (CCOpt.pp CCInt.pp) res; *)
      res

let monomorphic t = Iter.is_empty (Seq.ty_vars t)
let max_var set = VarSet.to_iter set |> Seq.max_var
let min_var set = VarSet.to_iter set |> Seq.min_var
let add_vars tbl t = Seq.vars t (fun v -> VarTbl.replace tbl v ())
let vars ts = Seq.vars ts |> VarSet.of_iter

let vars_prefix_order t =
   Seq.vars t |> Iter.fold (fun l x -> if not (List.memq x l) then x :: l else l) [] |> List.rev

let depth t = Seq.subterms_depth t |> Iter.map snd |> Iter.fold max 0

(* @param vars the free variables the parameter must depend upon
   @param ty_ret the return type *)
let n = ref 0

let mk_fresh_skolem ?(prefix = "_fresh_sk") vars ty_ret =
   let i = CCRef.incr_then_get n in
   (** fresh skolem **)
   let id = ID.makef "#%s%d" prefix i in
      ID.set_payload id (ID.Attr_skolem ID.K_after_cnf);
      let ty_vars, vars = List.partition (fun v -> Type.is_tType (HVar.ty v)) vars in
      let ty = Type.forall_fvars ty_vars (Type.arrow (List.map HVar.ty vars) ty_ret) in
         ((id, ty), app_full (const id ~ty) (List.map Type.var ty_vars) (List.map var vars))

let mk_tmp_cst ~counter ~ty =
   let idx = CCRef.get_then_incr counter in
   let id = ID.makef "#tmp%d" idx in
      const id ~ty

let rec head_exn t =
   match T.view t with T.Const s -> s | T.App (hd, _) -> head_exn hd | _ -> invalid_arg "Term.head"

let head t = try Some (head_exn t) with Invalid_argument _ -> None
let ty_vars t = Seq.ty_vars t |> Type.VarSet.of_iter

(** {2 Subterms and positions} *)

let replace t ~old ~by =
   assert (Type.equal (ty by) (ty old));
   of_term_unsafe (T.replace (t : t :> T.t) ~old:(old : t :> T.t) ~by:(by : t :> T.t))

let replace_m t m = of_term_unsafe (T.replace_m (t : t :> T.t) (m : t Map.t :> T.t T.Map.t))
let symbols ?(init = ID.Set.empty) t = ID.Set.add_iter init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t = Iter.exists (ID.equal f) (Seq.symbols t)

(** {2 Fold} *)

let all_positions ?(filter_formula_subterms = fun _ _ -> None) ?(vars = false) ?(ty_args = true)
    ?(var_args = true) ?(fun_bodies = true) ?(pos = Position.stop) t f =
   let rec aux pb t =
      match view t with
         | Var _ | DB _ -> if vars && (ty_args || not (Type.is_tType (ty t))) then f (PW.make t (PB.to_pos pb))
         | Const _ -> if ty_args || not (Type.is_tType (ty t)) then f (PW.make t (PB.to_pos pb))
         | Fun (_, u) ->
            f (PW.make t (PB.to_pos pb));
            if fun_bodies then aux (PB.body pb) u
         | App (head, _) when (not var_args) && T.is_var head -> f (PW.make t (PB.to_pos pb))
         | AppBuiltin (hd, args) when CCOpt.is_some (filter_formula_subterms hd args) ->
            f (PW.make t (PB.to_pos pb));
            let taken_args = CCOpt.get_exn (filter_formula_subterms hd args) in
            let len = List.length args in
            let invi i = len - 1 - i in
               List.iter
                 (fun i ->
                   (* if [t'] is a type parameter and [not ty_args], ignore *)
                   aux (PB.arg (invi i) pb) (List.nth args i))
                 taken_args
         | AppBuiltin (_, args) | App (_, args) ->
            if ty_args || not (Type.is_tType (ty t)) then f (PW.make t (PB.to_pos pb));
            let len = List.length args in
            let invi i = len - 1 - i in
               List.iteri
                 (fun i t' ->
                   (* if [t'] is a type parameter and [not ty_args], ignore *)
                   if ty_args || not (Type.is_tType (ty t')) then aux (PB.arg (invi i) pb) t')
                 args
   in
      aux (PB.of_pos pos) t

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : ID.t -> bool
  val is_comm : ID.t -> bool
end

module AC (A : AC_SPEC) = struct
  let flatten f l =
     let rec flatten acc l = match l with [] -> acc | x :: l' -> flatten (deconstruct acc x) l'
     and deconstruct acc t =
        match T.view t with
           | T.App (f', l') -> (
              match head f' with
                 | Some id when ID.equal id f ->
                    let _, args = split_args_ ~ty:(ty f') l' in
                       flatten acc args
                 | Some _ | None -> t :: acc)
           | _ -> t :: acc
     in
        flatten [] l

  let normal_form t =
     let _span = ZProf.enter_prof prof_ac_normal_form in
     let rec normalize t =
        match T.view t with
           | T.Const _ | T.Var _ | T.DB _ -> t
           | T.App (f, l)
             when T.is_const f && A.is_ac (head_exn f) && not (Type.is_fun @@ Type.of_term_unsafe @@ T.ty_exn t)
             -> (
              let l = flatten (head_exn f) l in
              let tyargs, l = split_args_ ~ty:(ty f) l in
              let l = List.map normalize l in
              let l = List.sort compare l in
                 match l with
                    | x :: l' ->
                       let ty = T.ty_exn t in
                       let tyargs = (tyargs :> T.t list) in
                          List.fold_left (fun subt x -> T.app ~ty f (tyargs @ [ x; subt ])) x l'
                    | [] -> assert false)
           | T.App (f, l)
             when (not (Type.is_fun @@ Type.of_term_unsafe @@ T.ty_exn t))
                  && T.is_const f
                  && A.is_comm (head_exn f) -> (
              let tyargs, l = split_args_ ~ty:(ty f) l in
                 match l with
                    | [ a; b ] ->
                       let a' = normalize a in
                       let b' = normalize b in
                          if compare a' b' > 0 then T.app ~ty:(ty t :> T.t) f (tyargs @ [ b'; a' ])
                          else if T.equal a a' && T.equal b b' then t
                          else T.app ~ty:(ty t :> T.t) f (tyargs @ [ a'; b' ])
                    | _ -> t (* partially applied *))
           | T.App (f, l) ->
              let l = List.map normalize l in
                 T.app ~ty:(T.ty_exn t) f l
           | T.AppBuiltin (b, l) ->
              let l = List.map normalize l in
                 T.app_builtin ~ty:(T.ty_exn t) b l
           | T.Bind (b, varty, body) -> T.bind ~ty:(T.ty_exn t) ~varty b (normalize body)
     in
     let t' = normalize t in
        ZProf.exit_prof _span;
        t'

  let equal t1 t2 =
     let t1' = normal_form t1 and t2' = normal_form t2 in
        equal t1' t2'

  let seq_symbols t = Seq.symbols t |> Iter.filter A.is_ac
  let symbols seq = seq |> Iter.flat_map seq_symbols |> ID.Set.add_iter ID.Set.empty
end

(** {2 Printing/parsing} *)

let print_all_types = T.print_all_types

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

(* lightweight printing *)
let pp_depth = T.pp_depth
let pp_var out (v : Type.t HVar.t) = T.pp_var out (v :> T.t HVar.t)
let add_hook = T.add_default_hook
let default_hooks = T.default_hooks
let pp out t = pp_depth 0 out t
let to_string = CCFormat.to_string pp

(** {2 Form} *)

module Form = struct
  let pp_hook _depth pp_rec out t =
     match Classic.view t with
        | Classic.AppBuiltin (Builtin.Not, [ a ]) ->
           Format.fprintf out "(@[<1>¬@ %a@])" pp_rec a;
           true
        | _ -> false (* default *)

  let () = add_hook pp_hook

  let not_ t : t =
     if not (Type.is_prop (ty t)) then (
       CCFormat.printf "t:@[%a@]@." T.pp t;
       CCFormat.printf "ty:@[%a@]@." Type.pp (ty t));
     assert (Type.is_prop (ty t));
     match view t with AppBuiltin (Builtin.Not, [ u ]) -> u | _ -> app_builtin ~ty:Type.prop Builtin.not_ [ t ]

  let eq a b =
     assert (Type.equal (ty a) (ty b));
     app_builtin ~ty:Type.prop Builtin.eq [ of_ty (ty a); a; b ]

  let neq a b =
     assert (Type.equal (ty a) (ty b));
     app_builtin ~ty:Type.prop Builtin.neq [ of_ty (ty a); a; b ]

  let and_ a b =
     assert (Type.is_prop (ty a) && Type.is_prop (ty b));
     app_builtin ~ty:Type.prop Builtin.and_ [ a; b ]

  let or_ a b =
     assert (Type.is_prop (ty a) && Type.is_prop (ty b));
     app_builtin ~ty:Type.prop Builtin.or_ [ a; b ]

  let and_l = function
     | [] -> true_
     | [ t ] ->
        assert (Type.is_prop (ty t));
        t
     | a :: tail -> List.fold_left and_ a tail

  let or_l = function
     | [] -> false_
     | [ t ] ->
        assert (Type.is_prop (ty t));
        t
     | a :: tail -> List.fold_left or_ a tail

  let forall t =
     assert (Type.is_fun (ty t) && Type.returns_prop (ty t));
     let ty_args, ret_ty = Type.open_fun (ty t) in
        assert (List.length ty_args = 1);
        app_builtin ~ty:Type.prop Builtin.ForallConst [ of_ty (List.hd ty_args); t ]

  let exists t =
     assert (Type.is_fun (ty t) && Type.returns_prop (ty t));
     let ty_args, ret_ty = Type.open_fun (ty t) in
        assert (List.length ty_args = 1);
        app_builtin ~ty:Type.prop Builtin.ExistsConst [ of_ty (List.hd ty_args); t ]

  let choice t =
     let ty = ty t in
        assert (Type.is_fun ty);
        let args, ret = Type.open_fun ty in
           assert (Type.is_prop ret);
           let alpha = List.hd args in
              app_builtin Builtin.ChoiceConst ~ty:alpha [ of_ty alpha; t ]

  let equiv f g = app_builtin ~ty:Type.prop Builtin.Equiv [ f; g ]
  let xor f g = app_builtin ~ty:Type.prop Builtin.Xor [ f; g ]
  let imply f g = app_builtin ~ty:Type.prop Builtin.Imply [ f; g ]
end

(** {2 Arith} *)

module Arith = struct
  let ty1 = Type.(forall ([ int ] ==> bvar 0))
  let floor = builtin ~ty:ty1 Builtin.Arith.floor
  let ceiling = builtin ~ty:ty1 Builtin.Arith.ceiling
  let truncate = builtin ~ty:ty1 Builtin.Arith.truncate
  let round = builtin ~ty:ty1 Builtin.Arith.round
  let prec = builtin ~ty:Type.([ int ] ==> int) Builtin.Arith.prec
  let succ = builtin ~ty:Type.([ int ] ==> int) Builtin.Arith.succ
  let ty2 = Type.(forall ([ bvar 0; bvar 0 ] ==> bvar 0))
  let ty2i = Type.([ int; int ] ==> int)
  let sum = builtin ~ty:ty2 Builtin.Arith.sum
  let difference = builtin ~ty:ty2 Builtin.Arith.difference
  let uminus = builtin ~ty:ty2 Builtin.Arith.uminus
  let product = builtin ~ty:ty2 Builtin.Arith.product
  let quotient = builtin ~ty:ty2 Builtin.Arith.quotient
  let quotient_e = builtin ~ty:ty2i Builtin.Arith.quotient_e
  let quotient_t = builtin ~ty:ty2i Builtin.Arith.quotient_t
  let quotient_f = builtin ~ty:ty2i Builtin.Arith.quotient_f
  let remainder_e = builtin ~ty:ty2i Builtin.Arith.remainder_e
  let remainder_t = builtin ~ty:ty2i Builtin.Arith.remainder_t
  let remainder_f = builtin ~ty:ty2i Builtin.Arith.remainder_f
  let ty2o = Type.(forall ([ bvar 0; bvar 0 ] ==> prop))
  let less = builtin ~ty:ty2o Builtin.Arith.less
  let lesseq = builtin ~ty:ty2o Builtin.Arith.lesseq
  let greater = builtin ~ty:ty2o Builtin.Arith.greater
  let greatereq = builtin ~ty:ty2o Builtin.Arith.greatereq

  (* hook that prints arithmetic expressions *)
  let pp_hook _depth pp_rec out t =
     let pp_surrounded buf t =
        match view t with
           | AppBuiltin (s, [ _; _ ]) when Builtin.is_infix s -> Format.fprintf buf "(@[<hv>%a@])" pp_rec t
           | _ -> pp_rec buf t
     in
        match view t with
           | Var v when Type.equal (ty t) Type.int ->
              Format.fprintf out "I%d" (HVar.id v);
              true
           | Var v when Type.equal (ty t) Type.rat ->
              Format.fprintf out "Q%d" (HVar.id v);
              true
           | AppBuiltin (Builtin.Less, [ _; a; b ]) ->
              Format.fprintf out "%a < %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Lesseq, [ _; a; b ]) ->
              Format.fprintf out "%a ≤ %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Greater, [ _; a; b ]) ->
              Format.fprintf out "%a > %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Greatereq, [ _; a; b ]) ->
              Format.fprintf out "%a ≥ %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Sum, [ _; a; b ]) ->
              Format.fprintf out "%a + %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Difference, [ _; a; b ]) ->
              Format.fprintf out "%a - %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Product, [ _; a; b ]) ->
              Format.fprintf out "%a × %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Quotient, [ _; a; b ]) ->
              Format.fprintf out "%a / %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Quotient_e, [ _; a; b ]) ->
              Format.fprintf out "%a // %a" pp_surrounded a pp_surrounded b;
              true
           | AppBuiltin (Builtin.Uminus, [ _; a ]) ->
              Format.fprintf out "-%a" pp_surrounded a;
              true
           | AppBuiltin (Builtin.Remainder_e, [ _; a; b ]) ->
              Format.fprintf out "%a mod %a" pp_surrounded a pp_surrounded b;
              true
           | _ -> false (* default *)

  let () = add_hook pp_hook
end

module DB = struct
  let is_closed = T.DB.closed
  let shift = T.DB.shift
  let eval = T.DB.eval
  let unshift = T.DB.unshift
  let unbound = T.DB.unbound

  let skolemize_loosely_bound ?(already_sk = IntMap.empty) t =
     let rec aux skolemized depth subt =
        match view subt with
           | Const _ | Var _ -> (subt, skolemized)
           | DB i ->
              if i >= depth then
                try
                  let sk = IntMap.find (i - depth) skolemized in
                     (sk, skolemized)
                with _ ->
                  let new_sk = snd @@ mk_fresh_skolem [] (ty subt) in
                  let skolemized = IntMap.add (i - depth) new_sk skolemized in
                     (new_sk, skolemized)
              else (subt, skolemized)
           | Fun (v_ty, body) ->
              let b', s' = aux skolemized (depth + 1) body in
                 (fun_ v_ty b', s')
           | App (f, l) ->
              let hd', s' = aux skolemized depth f in
              let args, s'' = sk_args l s' depth in
                 (app hd' args, s'')
           | AppBuiltin (hd, l) ->
              let args, s' = sk_args l skolemized depth in
                 (app_builtin ~ty:(ty subt) hd args, s')
     and sk_args l subst depth =
        List.fold_right
          (fun arg (acc, s) ->
            let arg', s_new = aux s depth arg in
               (arg' :: acc, s_new))
          l ([], subst)
     in
        aux already_sk 0 t

  let unskolemize sk_to_vars t =
     let rec aux depth subt =
        match Map.find_opt subt sk_to_vars with
           | Some i -> bvar ~ty:(ty subt) (depth + i)
           | None -> (
              match view subt with
                 | Const _ | Var _ | DB _ -> subt
                 | Fun (v_ty, body) -> fun_ v_ty (aux (depth + 1) body)
                 | App (f, l) ->
                    let f' = aux depth f in
                       app f' (List.map (aux depth) l)
                 | AppBuiltin (hd, l) -> app_builtin ~ty:(ty subt) hd (List.map (aux depth) l))
     in
        aux 0 t

  let rec map_vars_shift ?(depth = 0) var_map t =
     match view t with
        | Const _ | DB _ -> t
        | Var _ -> ( match Map.find_opt t var_map with Some i -> bvar ~ty:(ty t) (i + depth) | None -> t)
        | Fun (v_ty, body) ->
           let depth = depth + 1 in
              fun_ v_ty (map_vars_shift ~depth var_map body)
        | App (f, l) ->
           let f' = map_vars_shift ~depth var_map f in
              app f' (List.map (map_vars_shift ~depth var_map) l)
        | AppBuiltin (hd, l) -> app_builtin ~ty:(ty t) hd (List.map (map_vars_shift ~depth var_map) l)
end

let debugf = pp

(** {2 TPTP} *)

module TPTP = struct
  let pp_depth ?hooks:_ depth out t =
     let depth = ref depth in

     (* recursive printing *)
     let rec pp_rec out t =
        match view t with
           | DB i -> Format.fprintf out "Y%d" (!depth - i - 1)
           (* print type of term *)
           | AppBuiltin (b, []) -> Builtin.TPTP.pp out b
           | AppBuiltin (b, [ tyarg; t; u ]) when Builtin.TPTP.is_infix b && is_type tyarg ->
              Format.fprintf out "(@[(%a) %a@ (%a)@])" pp_rec t Builtin.TPTP.pp b pp_rec u
           | AppBuiltin (b, [ t; u ]) when Builtin.TPTP.is_infix b ->
              Format.fprintf out "(@[(%a) %a@ (%a)@])" pp_rec t Builtin.TPTP.pp b pp_rec u
           | AppBuiltin (b, l) when List.length l >= 2 && Builtin.is_infix b ->
              let sep = CCFormat.sprintf " %s " (Builtin.TPTP.to_string b) in
                 Format.fprintf out "(@[%a@])" (Util.pp_list ~sep pp_enclosed) l
           | AppBuiltin (b, l) ->
              (* erasing types for TH0 *)
              let l = CCList.filter (fun t -> not (Type.is_tType (ty t))) l in
                 if CCList.is_empty l then Format.fprintf out "@[%a@]" Builtin.TPTP.pp b
                 else
                   Format.fprintf out "(@[(%a) @@ %a@])" Builtin.TPTP.pp b (Util.pp_list ~sep:" @ " pp_enclosed)
                     l
           | Const s -> ID.pp_tstp out s
           | App (f, l) -> Format.fprintf out "%a" (Util.pp_list ~sep:" @ " pp_enclosed) (f :: l)
           | Fun _ ->
              let ty_args, bod = as_fun t in
              let vars = List.mapi (fun i ty -> (i + !depth, ty)) ty_args in
              let pp_db out (i, ty) = Format.fprintf out "Y%d : %a" i (Type.TPTP.pp_ho ~depth:!depth) ty in
              let old_d = !depth in
                 depth := !depth + List.length ty_args;
                 Format.fprintf out "(@[<hv2>^[@[%a@]]:@ (@[%a@])@])" (Util.pp_list ~sep:"," pp_db) vars pp_rec
                   bod;
                 depth := old_d
           | Var i -> Format.fprintf out "X%d" (HVar.id i)
     and pp_enclosed out t =
        if Type.is_tType (ty t) then
          let ty = Type.of_term_unsafe (t :> T.t) in
             Format.printf "(@[%a@])" (Type.TPTP.pp_ho ~depth:!depth) ty
        else
          match view t with App _ | AppBuiltin _ -> Format.fprintf out "(@[%a@])" pp_rec t | _ -> pp_rec out t
     in
        pp_rec out t

  let pp buf t = Format.fprintf buf "(@[%a@])" (pp_depth 0) t
  let to_string = CCFormat.to_string pp
end

module ZF = struct
  let pp = T.pp_zf
  let to_string = CCFormat.to_string pp
end

let pp_in = function
   | Output_format.O_zf -> ZF.pp
   | Output_format.O_tptp -> TPTP.pp
   | Output_format.O_normal -> pp
   | Output_format.O_none -> CCFormat.silent

(** {2 Conversions} *)

module Pos = struct
  let at t pos = of_term_unsafe (T.Pos.at (t :> T.t) pos)

  let replace t pos ~by =
     if not (Type.equal (at t pos |> ty) (ty by)) then (
       CCFormat.printf "t:@[%a:%a@]@." pp (at t pos) Type.pp (at t pos |> ty);
       CCFormat.printf "by:@[%a:%a@]@." pp by Type.pp (ty by);
       assert false);
     of_term_unsafe (T.Pos.replace (t :> T.t) pos ~by:(by :> T.t))
end

module Conv = struct
  module PT = TypedSTerm

  type ctx = Type.Conv.ctx

  let create = Type.Conv.create
  let[@inline] var_to_simple_var ?(prefix = "X") ctx v = Type.Conv.var_to_simple_var ~prefix ctx v

  let of_simple_term_exn ctx t =
     let tbl = PT.Var_tbl.create 8 in
     let depth = ref 0 in
     let rec aux t =
        match PT.view t with
           | PT.Var v -> (
              (* is the variable bound? *)
              match PT.Var_tbl.get tbl v with
                 | Some (i, ty) -> bvar ~ty (!depth - i - 1)
                 | None -> var (Type.Conv.var_of_simple_term ctx v))
           | PT.AppBuiltin (Builtin.Wildcard, []) ->
              (* fresh type variable *)
              var (Type.Conv.fresh_ty_var ctx)
           | PT.Const id ->
              let ty = Type.Conv.of_simple_term_exn ctx (PT.ty_exn t) in
                 const ~ty id
           | PT.Bind (Binder.ForallTy, _, _)
           | PT.AppBuiltin (Builtin.Arrow, _)
           | PT.AppBuiltin (Builtin.Term, [])
           | PT.AppBuiltin (Builtin.Prop, [])
           | PT.AppBuiltin (Builtin.TType, [])
           | PT.AppBuiltin (Builtin.TyInt, [])
           | PT.AppBuiltin (Builtin.TyRat, []) ->
              let t = Type.Conv.of_simple_term_exn ctx t in
                 of_ty t
           | PT.App (f, l) ->
              let f = aux f in
              let l = List.map aux l in
                 app f l
           | PT.AppBuiltin (b, l) ->
              let ty = Type.Conv.of_simple_term_exn ctx (PT.ty_exn t) in
              let l = List.map aux l in
                 app_builtin ~ty b l
           | PT.Bind (Binder.Lambda, v, body) ->
              let ty_arg = Type.Conv.of_simple_term_exn ctx (Var.ty v) in
                 PT.Var_tbl.add tbl v (!depth, ty_arg);
                 incr depth;
                 let body = aux body in
                    decr depth;
                    PT.Var_tbl.remove tbl v;
                    fun_ ty_arg body
           | PT.Bind (b, v, body) when Binder.equal b Binder.Forall || Binder.equal b Binder.Exists ->
              if TypedSTerm.Ty.is_tType (Var.ty v) then
                (* we are ignoring the types, since the conversion will take care of itself *)
                aux body
              else
                let b = if Binder.equal b Binder.Forall then Builtin.ForallConst else Builtin.ExistsConst in
                let ty_arg = Type.Conv.of_simple_term_exn ctx (Var.ty v) in
                let previous = if PT.Var_tbl.mem tbl v then Some (PT.Var_tbl.find tbl v) else None in
                   PT.Var_tbl.replace tbl v (!depth, ty_arg);
                   incr depth;
                   let ty_b = Type.Conv.of_simple_term_exn ctx (PT.ty_exn body) in
                      assert (Type.is_prop ty_b);
                      let body = fun_ ty_arg (aux body) in
                         decr depth;
                         if CCOpt.is_some previous then PT.Var_tbl.replace tbl v (CCOpt.get_exn previous)
                         else PT.Var_tbl.remove tbl v;
                         app_builtin ~ty:ty_b b [ of_ty ty_arg; body ]
           | PT.Meta _ | PT.Record _ | PT.Ite _ | PT.Let _ | PT.Match _ | PT.Multiset _ | _ ->
              raise (Type.Conv.Error t)
     in
        (* CCFormat.printf "converting:@[%a@]@." TypedSTerm.pp t; *)
        aux t

  let of_simple_term ctx t = try Some (of_simple_term_exn ctx t) with Type.Conv.Error _ -> None

  let to_simple_term ?(allow_free_db = false) ?(env = DBEnv.empty) ctx t =
     let module ST = TypedSTerm in
     let n = ref 0 in
     let max_t = max ((Seq.vars t |> Seq.max_var) + 1) (Type.Conv.get_maxvar ctx) in
        Type.Conv.set_maxvar ctx max_t;
        let orig_term = t in
        let rec aux_t env t =
           match view t with
              | Var i -> ST.var (aux_var i)
              | DB i -> (
                 match DBEnv.find env i with
                    | Some v -> ST.var v
                    | None when allow_free_db ->
                       (* encode DB index *)
                       ST.builtin ~ty:(aux_ty @@ ty t) (Builtin.Pseudo_de_bruijn i)
                    | None -> Util.errorf ~where:"Term" "cannot find `Y%d`@ @[:in [%a]@]" i (DBEnv.pp Var.pp) env
                 )
              | Const id -> ST.const ~ty:(aux_ty (ty t)) id
              | App (f, l) -> ST.app ~ty:(aux_ty (ty t)) (aux_t env f) (List.map (aux_t env) l)
              | AppBuiltin (b, [ _; body ])
                when Builtin.equal b Builtin.ForallConst || Builtin.equal b Builtin.ExistsConst ->
                 let b = if Builtin.equal b Builtin.ForallConst then Binder.Forall else Binder.Exists in
                 let ty_args, fun_body = open_fun body in

                 if is_true_or_false fun_body then
                   if T.equal fun_body true_ then ST.app_builtin ~ty:(aux_ty Type.prop) Builtin.True []
                   else ST.app_builtin ~ty:(aux_ty Type.prop) Builtin.False []
                 else if not (Type.returns_prop (ty fun_body)) then
                   let err_msg = CCFormat.sprintf "quantifier wrongly encoded: %a(%a)" T.pp t T.pp orig_term in
                      Util.error ~where:"Term" err_msg
                 else
                   let fresh_vars =
                      List.map
                        (fun ty ->
                          Type.Conv.incr_maxvar ctx;
                          var_of_int ~ty (Type.Conv.get_maxvar ctx))
                        ty_args
                   in
                   let replacement = DBEnv.push_l_rev DBEnv.empty fresh_vars in
                   let body = DB.eval replacement fun_body in
                   let remaining_vars =
                      List.map
                        (fun ty ->
                          Type.Conv.incr_maxvar ctx;
                          var_of_int ~ty (Type.Conv.get_maxvar ctx))
                        (Type.expected_args (ty fun_body))
                   in
                   let body = app body remaining_vars in
                   let vars_converted = List.map convert_var (fresh_vars @ remaining_vars) in
                      List.fold_right
                        (fun v acc -> ST.bind ~ty:(aux_ty Type.prop) b v acc)
                        vars_converted (aux_t env body)
              | AppBuiltin (b, l) ->
                 let res = ST.app_builtin ~ty:(aux_ty (ty t)) b (List.map (aux_t env) l) in
                    res
              | Fun (ty_arg, body) ->
                 let v = Var.makef ~ty:(aux_ty ty_arg) "v_%d" (CCRef.incr_then_get n) in
                 let body = aux_t (DBEnv.push env v) body in
                    ST.bind Binder.Lambda ~ty:(aux_ty (ty t)) v body
        and aux_var v = Type.Conv.var_to_simple_var ~prefix:"X" ctx v
        and aux_ty ty = Type.Conv.to_simple_term ~env ctx ty
        and convert_var v = match view v with Var v -> aux_var v | _ -> invalid_arg "expected variable" in

        let res = aux_t env t in
           res
end

let rebuild_rec t =
   let rec aux env t =
      let ty = Type.rebuild_rec ~env (ty t) in
         match view t with
            | Var v -> var (HVar.cast ~ty v)
            | DB i ->
               assert (
                 if i >= 0 && i < List.length env then true
                 else (
                   Format.printf "%d not in %a@." i (CCFormat.Dump.list Type.pp) env;
                   false));
               assert (
                 if Type.equal ty (List.nth env i) then true
                 else (
                   Format.printf "@[%a@ has type %a@ but bound with type %a@]@." pp t Type.pp ty Type.pp
                     (List.nth env i);
                   false));
               bvar ~ty i
            | Const id -> const ~ty id
            | App (f, l) -> app (aux env f) (List.map (aux env) l)
            | AppBuiltin (b, l) -> app_builtin ~ty b (List.map (aux env) l)
            | Fun (ty_arg, bod) ->
               let ty_arg = Type.rebuild_rec ~env ty_arg |> Type.unsafe_eval_db env in
                  fun_ ty_arg (aux (ty_arg :: env) bod)
   in
      aux [] t

(*mangle the given type such that it becomes a monomorphic constant*)
let rec convert_type ty_list ty =
   let open Type in
   (*Printf.printf "converting type: %s\n" (to_string ty);*)
   match List.find_opt (fun elem -> Type.equal (fst elem) ty) ty_list with
      | Some (_, new_ty) -> ty_list, new_ty
      | None -> 
         let args_nb, args, ret = open_poly_fun ty in
         if (List.length args) != 0 then
            let new_list, new_args = List.fold_left_map (fun acc_list ty -> convert_type acc_list ty) ty_list args in 
            let new_list, new_ret = convert_type new_list ret in
            let arrow_ty = new_args ==> new_ret in
            new_list, arrow_ty
         else
            match Type.view ty with
               | App(ty_id, []) -> ty_list, ty
               | App (ty_f_id, ty_args) ->
                     let str_ty_list, new_ty_args = List.fold_left_map (fun acc_list ty -> convert_type acc_list ty) ty_list ty_args in
                     let new_ty_str = Type.mangle (Type.app ty_f_id new_ty_args) in
                     let new_type = Type.const (ID.make new_ty_str) in
                     (ty, new_type)::str_ty_list, new_type
               | Builtin _ -> ty_list, ty
               | _ -> Printf.printf "that's not supposed to happen\n"; assert false


(* similar to List.fold_left_map for two accumulators *)
let fold_left_map2 f acc_1 acc_2 l =
   let acc_pair, res = List.fold_left_map (fun (acc_1, acc_2) elem ->
      let new_acc_1, new_acc_2, new_elem = f acc_1 acc_2 elem in
      (new_acc_1, new_acc_2), new_elem) (acc_1, acc_2) l in
   fst acc_pair, snd acc_pair, res

(* convert term such that it has mangled types, basically a copy of rebuild_rec
 * if we keep this approach, we will need TODO some refactoring (should be easy) *)
let mangle_term ty_list str_term_list t =
   let rec aux env ty_list str_term_list t =
      let ty_list, new_ty = convert_type ty_list (ty t) in
         (*Printf.printf "overall term: %s\n" (T.to_string (t:>term));*)
         (*Printf.printf "new type        : %s\n" (Type.to_string new_ty);*)
         (*Printf.printf "old type        : %s\n" (Type.to_string (ty t));*)
         match view t with
            | Var _ | DB _ | Const _ -> ty_list, str_term_list, (T.cast ~ty:(new_ty:> t) t)
            | App (f, l) ->
               (*Printf.printf "oooohhhaahooo\n"; *)
               (*Printf.printf "old f type : %s\n" (Type.to_string (ty f));*)
               begin
               let type_args, term_args = List.partition (fun x -> Type.is_tType (ty x)) l in
               match view f with
                  | Const f_id ->
                     (*List.iter (fun x -> Printf.printf "new l type : %s\n" (Type.to_string x)) (List.map Type.of_term_unsafe type_args);*)
                     let ty_list, str_term_list, new_term_args = fold_left_map2 (aux env) ty_list str_term_list term_args in
                     let ty_list, new_f_type = convert_type ty_list (Type.apply (ty f) (List.map Type.of_term_unsafe type_args)) in
                     (*Printf.printf "new f type : %s\n" (T.to_string (new_f_type:> term));*)


                     (*let new_f_name = (ID.name f_id) ^ "_" ^ (Type.mangle new_f_type) in*)
                     (*List.iter (fun x -> Printf.printf "type args : %s\n" (to_string (x))) type_args;*)
                     (*List.iter (fun x -> Printf.printf "new type args : %s\n" (T.to_string (x))) (new_type_args:> term list);*)

                     let str_term_list, new_f_term = 
                        if List.length type_args = 0 then str_term_list, T.cast ~ty:(new_f_type:> term) f
                        else
                           match List.find_opt (fun ((old_f_id, old_f_ty), _) -> ID.equal old_f_id f_id && Type.equal old_f_ty new_f_type) str_term_list with
                           | Some (_, new_f) ->
                              str_term_list, new_f
                           | None ->
                              let new_f = const ~ty:new_f_type (ID.make (ID.name f_id)) in
                              ((f_id, new_f_type), new_f) :: str_term_list, new_f
                     in
                     (*let new_f_term = T.cast ~ty:(new_f_type:> term) f in*)

                     (*Printf.printf "old f type : %s\n" (Type.to_string (Type.apply (ty f) new_type_args));*)
                     (*Printf.printf "f     term : %s\n" (to_string f);*)
                     (*Printf.printf "args length : %d\n" (List.length l);*)
                     (*Printf.printf "new type after AAARGH: %s\n" (Type.to_string new_type);*)
                     (*List.iter (fun x -> Printf.printf "new l terms type : %s\n" (Type.to_string (ty x))) (List.map (aux env) l);*)
                     (*Printf.printf "the type of new f: %s\n" (T.to_string ((ty new_f_term):> term));*)
                     (*Printf.printf "AAAAAAAAAAAAAARRRRGH\n";*)
                     (*List.iter (fun x -> Printf.printf "new l terms type : %s\n" (T.to_string ((ty x):>term))) new_term_args;*)
                     (*List.iter (fun x -> Printf.printf "new l terms : %s\n" (to_string x)) new_term_args;*)

                     (*List.iter (fun (old_ty, ty) -> Printf.printf "str_ty_list, old_ty: %s; ty: %s\n" (T.to_string old_ty) (T.to_string ty)) (ty_list:>(term * term) list);*)
                     (*List.iter (fun (str, term) -> Printf.printf "str_ty_list, id: %s; ty: %s\n" str (T.to_string term)) (str_term_list:>(string * term) list);*)
                     
                     let res = app new_f_term new_term_args in
                     (*Printf.printf "res type : %s\n" (Type.to_string (ty res));*)
                     (*Printf.printf "new type : %s\n" (Type.to_string new_ty);*)
                     (*assert (Type.equal (ty res) new_ty);*)
                     (*Printf.printf "new app type : %s\n" (Type.to_string new_app_type);*)
                     (*Printf.printf "new args length : %d\n" (List.length new_term_args);*)
                     (*let res = T.app ~ty:(new_app_type:> term) (T.cast ~ty:(new_f_type:> t) new_f_term) new_term_args in*)
                     (*Printf.printf "result : %s\n" (to_string res);*)
                     (*Printf.printf "result type : %s\n" (Type.to_string (ty res));*)
                     ty_list, str_term_list, res
                     (*app (const ~ty:new_f_type f_id) (List.map (aux env) term_args)*)
                  | _ ->
                        assert (type_args = []);
                        (*let str_ty_list, str_term_list, new_f = aux env str_ty_list str_term_list f in*)
                        (*Printf.printf "WOHOOOO when i getadimeno WOOOHOO when i gadenomeno\n";*)
                        let str_ty_list, str_term_list, new_l = fold_left_map2 (aux env) ty_list str_term_list l in
                        let str_ty_list, new_f_type = convert_type str_ty_list (ty f) in
                        let new_f = T.cast ~ty:(new_f_type:> t) f in
                        (*Printf.printf "FFFFFFFFFFFFFFFFFFnew_f type : %s\n" (Type.to_string (ty f));*)
                        (*let str_ty_list, str_term_list, new_f = (aux env str_ty_list str_term_list f) in*)
                        (*List.iter (fun x -> Printf.printf "new_l type : %s\n" (Type.to_string (ty x))) new_l;*)
                        str_ty_list, str_term_list, app new_f new_l
               end
            | AppBuiltin (b, l) ->
                  (*Printf.printf "AppBuiltin DEEZE NUTS\n";*)
                  let str_ty_list, str_term_list, new_l = fold_left_map2 (aux env) ty_list str_term_list l in
                  str_ty_list, str_term_list, app_builtin ~ty:new_ty b new_l
            | Fun (var_ty, bod) -> 
               (*Printf.printf "FUNction\n";*)
               (*let new_var_ty = Type.rebuild_rec ~env var_ty |> Type.unsafe_eval_db env in*)
               let str_ty_list, new_var_ty = convert_type ty_list var_ty in
               let str_ty_list, str_term_list, new_body = aux (new_var_ty :: env) str_ty_list str_term_list bod in
                  str_ty_list, str_term_list, fun_ new_var_ty new_body
   in
      aux [] ty_list str_term_list t

let rec normalize_bools t =
   let weight_cmp s t =
      let ( <?> ) = CCOrd.( <?> ) in
         T.ho_weight s - T.ho_weight t <?> (CCInt.compare, T.hash s, T.hash t)
   in

   match view t with
      | DB _ | Const _ | Var _ -> t
      | Fun (ty, body) ->
         let body' = normalize_bools body in
            if equal body body' then t else fun_ ty body'
      | App (hd, args) ->
         let hd' = normalize_bools hd and args' = List.map normalize_bools args in
            if equal hd hd' && same_l args args' then t else app hd' args'
      | AppBuiltin (((Builtin.And | Builtin.Or) as b), l) ->
         let l' = List.map normalize_bools l in
         let sorted = CCList.sort_uniq ~cmp:weight_cmp l' in
            if List.length l = List.length sorted && same_l l sorted then t
            else if Type.is_prop (ty t) && List.length sorted = 1 then List.hd sorted
            else app_builtin ~ty:Type.prop b sorted
      | AppBuiltin (((Builtin.Eq | Builtin.Neq | Builtin.Xor | Builtin.Equiv) as b), ([ _; x; y ] as l))
      | AppBuiltin (((Builtin.Eq | Builtin.Neq | Builtin.Xor | Builtin.Equiv) as b), ([ x; y ] as l))
        when not (is_type x) ->
         let rec swap_last_two l =
            match l with [] | [ _ ] -> l | [ x; y ] -> [ y; x ] | x :: xs -> x :: swap_last_two xs
         in
         let x', y' = (normalize_bools x, normalize_bools y) in
         let l = if List.length l = 3 then List.hd l :: x' :: [ y' ] else x' :: [ y' ] in
            if weight_cmp x' y' < 0 then app_builtin ~ty:Type.prop b (swap_last_two l)
            else if T.equal x x' && T.equal y y' then t
            else app_builtin ~ty:Type.prop b l
      | AppBuiltin (hd, l) ->
         let l' = List.map normalize_bools l in
            if same_l l' l then t else app_builtin ~ty:(ty t) hd l'

let rec is_properly_encoded t =
   match view t with
      | Var _ | DB _ | Const _ -> true
      | AppBuiltin (hd, l) when Builtin.equal hd Builtin.ForallConst || Builtin.equal hd Builtin.ExistsConst ->
         let res =
            match l with
               | [ tyarg ] -> Type.is_tType (ty tyarg)
               | [ tyarg; body ] ->
                  let t_ty = ty body in
                  let tyargs, ret_ty = Type.open_fun t_ty in
                     Type.is_tType (ty tyarg)
                     && List.length tyargs = 1
                     && Type.equal (Type.of_term_unsafe (tyarg :> InnerTerm.t)) (List.hd tyargs)
                     && Type.is_prop ret_ty
               | _ -> false
         in
            (* if not res then CCFormat.printf "Failed for %a.\n" T.pp t; *)
            res
      | AppBuiltin (Builtin.(Eq | Neq), l) -> List.length l >= 1 && Type.is_tType (ty (List.hd l))
      | AppBuiltin (_, l) -> List.for_all is_properly_encoded l
      | App (hd, l) -> List.for_all is_properly_encoded (hd :: l)
      | Fun (_, u) -> is_properly_encoded u

let () = Options.add_opts [ ("--print-types", Arg.Set print_all_types, " print type annotations everywhere") ]
