
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Scoped Terms} *)

module Hash = CCHash

type t = {
  term : view;
  ty : type_result;
  mutable id : int;
}

(* head form *)
and view =
  | Var of t HVar.t (** Free or bound variable *)
  | DB of int
  | Bind of Binder.t * t * t (** Type, sub-term *)
  | Const of ID.t (** Constant *)
  | Record of (string * t) list * t HVar.t option (** Extensible record *)
  | Multiset of t list (** Multiset of terms *)
  | App of t * t list (** Uncurried application *)
  | SimpleApp of ID.t * t list
  | AppBuiltin of Builtin.t * t list (** For representing special constructors *)

and type_result =
  | NoType
  | HasType of t

type term = t

let view t = t.term
let ty t = t.ty
let ty_exn t = match t.ty with
  | NoType -> invalid_arg "ScopedTerm.ty_exn"
  | HasType ty -> ty

let hash_fun t s = Hash.int_ t.id s
let hash t = Hash.apply hash_fun t
let equal t1 t2 = t1 == t2
let compare t1 t2 = Pervasives.compare t1.id t2.id

let _hash_ty t h =
  match t.ty with
  | NoType -> h
  | HasType ty -> Hash.int_ ty.id (Hash.string_ "type" h)

let _hash_norec t h =
  let h = match view t with
    | Var v -> h |> Hash.string_ "var" |> HVar.hash_fun v
    | DB v -> Hash.int_ v h
    | Bind (b, varty, t') ->
        h |> Hash.string_ "bind" |> Binder.hash_fun b |> hash_fun varty |> hash_fun t'
    | Const s -> h |> Hash.string_ "const" |> ID.hash_fun s
    | Record (l, rest) ->
        h
        |> Hash.string_ "record"
        |> Hash.list_ (fun (s,t') h -> h |> Hash.string_ s |> hash_fun t') l
        |> Hash.opt HVar.hash_fun rest
    | Multiset l -> h |> Hash.string_ "ms" |> Hash.list_ hash_fun l
    | App (f, l) -> h |> Hash.string_ "app" |> hash_fun f |> Hash.list_ hash_fun l
    | SimpleApp (b, l) ->
        h |> Hash.string_ "sapp" |> ID.hash_fun b |> Hash.list_ hash_fun l
    | AppBuiltin (b, l) ->
        h |> Hash.string_ "sapp" |> Builtin.hash_fun b |> Hash.list_ hash_fun l
  in
  _hash_ty t h

let rec _eq_norec t1 t2 =
  _eq_ty t1 t2 &&
  match t1.term, t2.term with
  | Var i, Var j -> HVar.equal i j
  | DB i, DB j -> i = j
  | Const s1, Const s2 -> ID.equal s1 s2
  | Bind (b1, varty1, t1'), Bind (b2, varty2, t2') ->
      Binder.equal b1 b2 && equal varty1 varty2 && equal t1' t2'
  | App (f1, l1), App (f2, l2) ->
      equal f1 f2 && _eq_list l1 l2
  | Multiset l1, Multiset l2 ->
      _eq_list l1 l2
  | Record (l1, None), Record (l2, None) ->
      _eq_record_list l1 l2
  | Record (l1, Some r1), Record (l2, Some r2) ->
      equal r1 r2 && _eq_record_list l1 l2
  | AppBuiltin (b1, l1), AppBuiltin (b2, l2) ->
      Builtin.equal b1 b2 && _eq_list l1 l2
  | _ -> false
and _eq_ty t1 t2 = match t1.ty, t2.ty with
  | NoType, NoType -> true
  | HasType ty1, HasType ty2 -> equal ty1 ty2
  | _ -> false
and _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> equal t1 t2 && _eq_list l1' l2'
and _eq_record_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | (n1,t1)::l1', (n2,t2)::l2' -> n1=n2 && equal t1 t2 && _eq_record_list l1' l2'

(** {3 Constructors} *)

module H = Hashcons.Make(struct
    type t = term
    let equal = _eq_norec
    let hash = Hash.apply _hash_norec
    let tag i t = assert (t.id = ~-1); t.id <- i
  end)

let hashcons_stats () = H.stats ()

exception IllFormedTerm of string
type nat = int

let make_ ~ty term = {
  term;
  ty;
  id = ~-1;
}

let make_id_ ~id ~ty term = {
  term;
  ty;
  id;
}

let const ~ty s =
  let my_t = make_ ~ty:(HasType ty) (Const s) in
  H.hashcons my_t

let app ~ty f l =
  match l with
  | [] -> f
  | _::_ ->
      let my_t = make_ ~ty:(HasType ty) (App (f,l)) in
      H.hashcons my_t

let var v = H.hashcons (make_ ~ty:(HasType (HVar.ty v)) (Var v))

let bvar ~ty i =
  if i<0 then raise (IllFormedTerm "bvar");
  H.hashcons (make_ ~ty:(HasType ty) (DB i))

let bind ~ty ~varty s t' =
  H.hashcons (make_ ~ty:(HasType ty) (Bind (s, varty, t')))

(* TODO: move to Subst (only place where it can happen now) *)
(* merge l1 and l2, which are both sorted. If the same key occurs with
   distinct values, fail. *)
let rec merge_records_ l1 l2 = match l1, l2 with
  | [], [] -> []
  | [], _ -> l2
  | _, [] -> l1
  | (n1,t1)::l1', (n2,t2)::l2' ->
      match String.compare n1 n2 with
      | 0 ->
          if equal t1 t2
          then (n1,t1) :: merge_records_ l1' l2'  (* compatible *)
          else failwith ("ill-formed record: field "^n1^" has distinct values")
      | n when n < 0 -> (n1,t1) :: merge_records_ l1' l2
      | _ -> (n2,t2) :: merge_records_ l1 l2'

(* if any string of [l] appears in [seen], fail *)
let rec check_duplicates_ seen l = match l with
  | [] -> ()
  | (s,_) :: l' ->
      if List.mem s seen
      then failwith ("ill-formed record: field " ^ s ^ " appears twice")
      else check_duplicates_ (s::seen) l'

(* TODO: remove computation of ground flag *)

(* actually build the record *)
let make_record_ ~ty l ~rest =
  let my_t = make_ ~ty:(HasType ty) (Record (l,rest)) in
  H.hashcons my_t

(* flatten record: if the rest is a record, merge its fields into [l].
   precondition: [l] is sorted. *)
let flatten_record_ ~ty l ~rest =
  match rest with
  | None -> make_record_ ~ty l ~rest:None
  | Some r ->
      begin match view r with
        | Record (l', rest') ->
            (* here be flattening! *)
            make_record_ ~ty (merge_records_ l l') ~rest:rest'
        | Var v -> make_record_ ~ty l ~rest:(Some v)
        | _ -> invalid_arg "record_flatten: row must be record or variable"
      end

let record ~ty l ~rest =
  let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
  (* check that [l] is well-formed *)
  check_duplicates_ [] l;
  make_record_ ~ty l ~rest

let record_flatten ~ty l ~rest =
  let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
  (* check that [l] is well-formed *)
  check_duplicates_ [] l;
  (* merge with rest, if any *)
  flatten_record_ ~ty l ~rest

let multiset ~ty l =
  let l = List.sort compare l in
  let my_t = make_ ~ty:(HasType ty) (Multiset l) in
  H.hashcons my_t

let simple_app ~ty s l =
  let my_t = make_ ~ty:(HasType ty) (SimpleApp (s,l)) in
  H.hashcons my_t

let builtin ~ty b =
  let my_t = make_ ~ty:(HasType ty) (AppBuiltin (b,[])) in
  H.hashcons my_t

let app_builtin ~ty b l =
  let my_t = make_ ~ty:(HasType ty) (AppBuiltin (b,l)) in
  H.hashcons my_t

let tType =
  let my_t = make_ ~ty:NoType (AppBuiltin(Builtin.TType, [])) in
  H.hashcons my_t

let cast ~ty old = match old.term with
  | Var v -> var (HVar.cast v ~ty)
  | DB i -> bvar ~ty i
  | Const s -> const ~ty s
  | Bind (s, varty, t') -> bind ~ty s ~varty t'
  | Record (l, rest) -> record ~ty l ~rest
  | Multiset l -> multiset ~ty l
  | App (f,l) -> app ~ty f l
  | SimpleApp (s,l) -> simple_app ~ty s l
  | AppBuiltin (s,l) -> app_builtin ~ty s l

let change_kind old =
  let my_t = { old with id= ~-1; } in
  H.hashcons my_t

let is_var t = match view t with | Var _ -> true | _ -> false
let is_bvar t = match view t with | DB _ -> true | _ -> false
let is_const t = match view t with | Const _ -> true | _ -> false
let is_bind t = match view t with | Bind _ -> true | _ -> false
let is_record t = match view t with | Record _ -> true | _ -> false
let is_multiset t = match view t with | Multiset _ -> true | _ -> false
let is_app t = match view t with | App _ -> true | _ -> false

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
  let compare = HVar.compare
  let equal = HVar.equal
  let hash = HVar.hash
end

module VarMap = CCMap.Make(HVarKey)
module VarSet = CCSet.Make(HVarKey)
module VarTbl = CCHashtbl.Make(HVarKey)

(** {3 De Bruijn} *)

module DB = struct
  type env = t DBEnv.t

  (* sequence2 of [De Bruijn, depth] pairs *)
  let rec _to_seq ~depth t k =
    begin match t.ty with
      | NoType -> ()
      | HasType ty -> _to_seq ~depth ty k
    end;
    match view t with
    | DB v -> k v depth
    | Var _
    | Const _ -> ()
    | Bind (_, varty, t') ->
        _to_seq ~depth varty k;
        _to_seq ~depth:(depth+1) t' k
    | Record (l, _rest) ->
        List.iter (fun (_,t) -> _to_seq ~depth t k) l
    | AppBuiltin (_, l)
    | SimpleApp (_,l)
    | Multiset l ->
        List.iter (fun t -> _to_seq ~depth t k) l
    | App (f, l) ->
        _to_seq ~depth f k;
        List.iter (fun t -> _to_seq ~depth t k) l

  let _id x = x

  let closed t =
    _to_seq ~depth:0 t
    |> Sequence.map2
      (fun bvar depth -> bvar < depth)
    |> Sequence.for_all _id

  (* check whether t contains the De Bruijn symbol n *)
  let contains t n =
    _to_seq ~depth:0 t
    |> Sequence.map2
      (fun bvar _ -> bvar=n)
    |> Sequence.exists _id

  (* maps the term to another term, calling [on_binder acc t]
     when it meets a binder, and [on_bvar acc t] when it meets a
     bound variable. *)
  let _fold_map acc ~on_bvar ~on_binder t =
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
          | Record (l, rest) ->
              let l = List.map (fun (s,t') -> s, recurse ~depth acc t') l in
              record ~ty l ~rest
          | Multiset l ->
              let l = List.map (recurse ~depth acc) l in
              multiset ~ty l
          | App (f, l) ->
              app ~ty (recurse ~depth acc f) (List.map (recurse ~depth acc) l)
          | SimpleApp (s,l) ->
              simple_app ~ty s (List.map (recurse ~depth acc) l)
          | AppBuiltin (s,l) ->
              app_builtin ~ty s (List.map (recurse ~depth acc) l)
    in
    recurse ~depth:0 acc t

  (* shift the non-captured De Bruijn indexes in the term by n *)
  let shift n t =
    assert (n >= 0);
    _fold_map ()
      ~on_bvar:(
        fun ~depth () i ->
          if i >= depth
          then i + n  (* shift *)
          else i
      )
      ~on_binder:(fun ~ty:_ ~depth:_ () _ _ -> ())
      t

  let unshift n t =
    _fold_map ()
      ~on_bvar:(
        fun ~depth () i ->
          if i >= depth
          then i - n  (* ushift *)
          else i
      )
      ~on_binder:(fun ~ty:_ ~depth:_ () _ _ -> ())
      t

  (* recurse and replace [sub]. *)
  let rec _replace depth ~sub t =
    match t.ty with
    | NoType ->
        assert (t == tType);
        t
    | HasType ty ->
        let ty = _replace depth ty ~sub in
        match view t with
        | _ when equal t sub ->
            bvar ~ty depth  (* replace *)
        | Var v -> var (HVar.cast ~ty v)
        | DB i -> bvar ~ty i
        | Const s -> const ~ty s
        | Bind (s, varty, t') ->
            let varty' = _replace depth ~sub varty in
            let t' = _replace (depth+1) t' ~sub in
            bind ~ty ~varty:varty' s t'
        | Record (l, rest) ->
            let l = List.map (fun (s,t') -> s, _replace depth t' ~sub) l in
            record ~ty l ~rest
        | Multiset l ->
            let l = List.map (_replace depth ~sub) l in
            multiset ~ty l
        | App (f, l) ->
            app ~ty (_replace depth ~sub f) (List.map (_replace depth ~sub) l)
        | SimpleApp (s,l) ->
            simple_app ~ty s (List.map (_replace depth ~sub) l)
        | AppBuiltin (s,l) ->
            app_builtin ~ty s (List.map (_replace depth ~sub) l)

  let replace t ~sub = _replace 0 t ~sub

  let from_var t ~var =
    assert (is_var var);
    replace t ~sub:var

  let rec _eval env t =
    match t.ty with
    | NoType ->
        assert (t == tType);
        t
    | HasType ty ->
        let ty = _eval env ty in
        match view t with
        | Var v -> var (HVar.cast ~ty v)
        | DB i ->
            begin match DBEnv.find env i with
              | None -> bvar ~ty i
              | Some t' ->
                  (* TODO: we should shift [t'], and make sure shifting is
                     efficient when [t'] is closed (e.g. storing in each
                     term the number of binders it needs to be closed) *)
                  assert (closed t');
                  t'
            end
        | Const s -> const ~ty s
        | Bind (s, varty, t') ->
            let varty' = _eval env varty in
            let t' = _eval (DBEnv.push_none env) t' in
            bind ~ty ~varty:varty' s t'
        | Record (l, rest) ->
            let l = List.map (fun (s,t') -> s, _eval env t') l in
            record ~ty l ~rest
        | Multiset l ->
            let l = List.map (_eval env) l in
            multiset ~ty l
        | App (f, l) ->
            app ~ty (_eval env f) (List.map (_eval env) l)
        | AppBuiltin (s,l) ->
            app_builtin ~ty s (List.map (_eval env) l)
        | SimpleApp (s,l) ->
            simple_app ~ty s (List.map (_eval env) l)

  let eval env t = _eval env t

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
          begin try
              shift depth (VarMap.find v subst)
            with Not_found -> var (HVar.cast ~ty v)
          end
      | DB i -> bvar ~ty i
      | Const s -> const ~ty s
      | Bind (s, varty, t') ->
          let varty' = aux depth varty in
          let t' = aux (depth+1) t' in
          bind ~ty ~varty:varty' s t'
      | Record (l, rest) ->
          let rest = match rest with
            | None -> None
            | Some v ->
                begin try
                    Some (shift depth (VarMap.find v subst))
                  with Not_found -> Some (var (HVar.cast ~ty v))
                end
          in
          let l = List.map (fun (s,t') -> s, aux depth t') l in
          record_flatten ~ty l ~rest
      | Multiset l ->
          let l = List.map (aux depth) l in
          multiset ~ty l
      | App (f, l) ->
          app ~ty (aux depth f) (List.map (aux depth) l)
      | SimpleApp (s,l) ->
          simple_app ~ty s (List.map (aux depth) l)
      | AppBuiltin (s,l) ->
          app_builtin ~ty s (List.map (aux depth) l)
    in
    aux 0 t
end

let bind_vars ~ty b vars t =
  (* subst: bind vars_i to a De Bruijn (reverse list so that last element is 0) *)
  let subst = CCList.Idx.foldi
      (fun s i v -> VarMap.add v (bvar ~ty:(HVar.ty v) i) s)
      VarMap.empty (List.rev vars)
  in
  List.fold_right
    (fun v t -> bind ~ty ~varty:(HVar.ty v) b t)
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
      | Record (l, rest) ->
          CCOpt.iter k rest;
          List.iter (fun (_,t') -> vars t') l
      | AppBuiltin (_,l)
      | SimpleApp (_,l)
      | Multiset l -> List.iter vars l
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
      | Record (l, _) ->
          List.iter (fun (_,t') -> subterms t') l
      | SimpleApp (_,l)
      | AppBuiltin (_, l)
      | Multiset l -> List.iter subterms l
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
      | Record (l,_) ->
          List.iter (fun (_,t') -> recurse depth t') l
      | AppBuiltin (_,l)
      | SimpleApp (_,l)
      | Multiset l -> List.iter (recurse (depth+1)) l
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
      | Record (l, _) ->
          List.iter (fun (_,t') -> symbols t') l
      | Multiset l
      | SimpleApp  (_,l)
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
      | Record (l,_) ->
          List.iter (fun (_,t') -> types t') l
      | SimpleApp (_,l)
      | AppBuiltin (_,l)
      | Multiset l -> List.iter types l
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

  let add_set = Sequence.fold (fun set t -> Set.add t set)

  let add_tbl tbl = Sequence.iter (fun t -> Tbl.replace tbl t ())
end

(** {3 Positions} *)

module Pos = struct
  module P = Position
  let rec at t pos = match view t, pos with
    | _, P.Type pos' ->
        begin match t.ty with
          | NoType -> invalid_arg "wrong position: term has no type"
          | HasType ty -> at ty pos'
        end
    | _, P.Stop -> t
    | Var _ , _ -> invalid_arg "wrong position in term"
    | Bind(_, _, t'), P.Body subpos -> at t' subpos
    | App (t, _), P.Head subpos -> at t subpos
    | App (_, l), P.Arg (n,subpos) when n < List.length l ->
        at (List.nth l n) subpos
    | Multiset l, P.Arg (n,subpos) when n < List.length l ->
        at (List.nth l n) subpos
    | Record (l, _), P.Record_field (name, subpos) ->
        begin try
            let t' = List.assoc name l in
            at t' subpos
          with Not_found ->
            invalid_arg ("wrong position: no such record field: " ^name)
        end
    | AppBuiltin (_, l), P.Arg(n,subpos) when n < List.length l ->
        at (List.nth l n) subpos
    | _ -> invalid_arg
             (CCFormat.sprintf "position %a not valid in term" P.pp pos)

  let rec replace t pos ~by = match t.ty, view t, pos with
    | _, _, P.Stop -> by
    | NoType, _, P.Type _ -> invalid_arg "wrong position: term has no type"
    | HasType ty, _, P.Type pos' ->
        let ty = replace ty pos' ~by in
        cast ~ty t
    | _, Var _, _ -> invalid_arg "wrong position in term"
    | HasType ty, Bind(s, varty, t'), P.Body subpos ->
        bind ~ty ~varty s (replace t' subpos ~by)
    | HasType ty, App (f, l), P.Head subpos ->
        app ~ty (replace f subpos ~by) l
    | HasType ty, App (f, l), P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        app ~ty f l'
    | HasType ty, Multiset l, P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        multiset ~ty l'
    | HasType ty, AppBuiltin (s,l), P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        app_builtin ~ty s l'
    | HasType ty, SimpleApp (s,l), P.Arg (n,subpos) when n < List.length l ->
        let t' = replace (List.nth l n) subpos ~by in
        let l' = CCList.Idx.set l n t' in
        simple_app ~ty s l'
    | HasType ty, Record (l, rest), P.Record_field (name, subpos) ->
        begin try
            let t' = replace (List.assoc name l) subpos ~by in
            let l' = (name,t') :: (List.remove_assoc name l) in
            record ~ty l' ~rest
          with Not_found ->
            raise (Invalid_argument ("wrong position: no such record field: " ^name))
        end
    | _ -> invalid_arg
             (CCFormat.sprintf "position %a not valid in term" P.pp pos)
end

(* [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.ty, view t with
  | _ when equal t old -> by
  | HasType ty, Bind (s, varty, t') ->
      bind ~ty ~varty s (replace t' ~old ~by)
  | HasType ty, Record (l, rest) ->
      let l = List.map (fun (s,t') -> s, replace t' ~old ~by) l in
      record ~ty l ~rest
  | HasType ty, App (f, l) ->
      let f' = replace f ~old ~by in
      let l' = List.map (fun t' -> replace t' ~old ~by) l in
      app ~ty f' l'
  | HasType ty, Multiset l ->
      let l' = List.map (fun t' -> replace t' ~old ~by) l in
      multiset ~ty l'
  | HasType ty, AppBuiltin (s,l) ->
      let l' = List.map (fun t' -> replace t' ~old ~by) l in
      app_builtin ~ty s l'
  | HasType ty, SimpleApp (s,l) ->
      let l' = List.map (fun t' -> replace t' ~old ~by) l in
      simple_app ~ty s l'
  | NoType, _ -> t
  | _, (Var _ | DB _ | Const _) -> t

(** {3 Variables} *)

let close_vars ~ty s t =
  let vars = VarSet.of_seq (Seq.vars t) in
  bind_vars ~ty s (VarSet.elements vars) t

let is_ground t = Sequence.is_empty (Seq.vars t)

(** {3 Misc} *)

let rec size t = match view t with
  | Const _
  | Var _
  | DB _ -> 1
  | Bind (_, _, t') -> 1 + size t'
  | Record (l, rest) ->
      let s = match rest with | None -> 0 | Some _ -> 1 in
      List.fold_left (fun acc (_,t') -> acc + size t') (1+s) l
  | AppBuiltin (_,l)
  | SimpleApp (_,l)
  | Multiset l -> List.fold_left (fun s t -> s+size t) 1 l
  | App (head, l) -> _size_list (1 + size head) l
and _size_list acc l = match l with
  | [] -> acc
  | t::l' -> _size_list (acc + size t) l'

let depth t =
  Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head t = match view t with
  | SimpleApp (s,_)
  | Const s -> Some s
  | DB _ | Var _ | Record _ | Multiset _
  | Bind (_, _, _) | AppBuiltin (_, _) -> None
  | App (h, _) -> head h

(** {3 IO} *)

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

let _hooks = ref []
let add_default_hook h = _hooks := h :: !_hooks

let pp_depth ?(hooks=[]) depth out t =
  let rec _pp depth out t =
    if List.exists (fun h -> h depth (_pp depth) out t) hooks
    then () (* hook took control *)
    else _pp_root depth out t; _pp_ty depth out t
  and _pp_root depth out t = match view t with
    | Var v -> Format.fprintf out "X%d" (HVar.id v)
    | DB i -> Format.fprintf out "Y%d" (depth-i-1)
    | Const s -> ID.pp out s
    | Bind (b, varty, t') ->
        Format.fprintf out "@[%a Y%d:%a.@ %a@]" Binder.pp b depth
          (_pp depth) varty (_pp_surrounded (depth+1)) t'
    | Record ([], None) ->
        CCFormat.string out "{}"
    | Record ([], Some v) ->
        Format.fprintf out "{@[ | X%d@]}" (HVar.id v)
    | Record (l, None) ->
        Format.fprintf out "{@[<hv>%a@]}"
          (Util.pp_list ~sep:", "
             (Util.pp_pair ~sep:": " CCFormat.string (_pp depth)))
          l
    | Record (l, Some v) ->
        Format.fprintf out "{@[<hv>%a@ | X%d@]}"
          (Util.pp_list ~sep:", "
             (Util.pp_pair ~sep:": " CCFormat.string (_pp depth)))
          l (HVar.id v)
    | Multiset l ->
        Format.fprintf out "{|@[<hv>%a@]|}" (Util.pp_list (_pp depth)) l
    | SimpleApp (s,[]) -> ID.pp out s
    | SimpleApp (s,l) ->
        Format.fprintf out "@[%a@ %a@]" ID.pp s (Util.pp_list (_pp depth)) l
    | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
        Format.fprintf out "@[<2>%a@ %a@]" Builtin.pp b (_pp depth) a
    | AppBuiltin (b, [t1;t2]) when Builtin.is_infix b ->
        Format.fprintf out "(@[<2>%a@ %a@ %a@])" (_pp depth) t1 Builtin.pp b (_pp depth) t2
    | AppBuiltin (b, l) ->
        Format.fprintf out "@[%a(%a)@]" Builtin.pp b (Util.pp_list (_pp depth)) l
    | App (f, l) ->
        Format.fprintf out "(@[<2>%a@ %a@])"
          (_pp_surrounded depth) f (Util.pp_list ~sep:" " (_pp_surrounded depth)) l
  and _pp_ty depth out t = match t.ty, view t with
    | HasType ty, (Var _ | DB _) ->
        Format.fprintf out ":%a" (_pp_surrounded depth) ty
    | _ -> ()
  and _pp_surrounded depth out t = match view t with
    | Bind _
    | SimpleApp (_,_::_)
    | AppBuiltin (_,_::_)
    | App _ -> Format.fprintf out "(@[%a@])" (_pp depth) t
    | _ -> _pp depth out t
  in
  _pp depth out t

let pp out t = pp_depth ~hooks:!_hooks 0 out t
let to_string t = CCFormat.to_string pp t
