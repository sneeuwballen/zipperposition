
(*
Copyright (c) 2013-2014, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Prolog-like LogtkTyped Terms}.

These terms are scoped, and possibly typed. LogtkType inference should be
performed on them. *)

module Hash = CCHash

type location = LogtkParseLocation.t

type t = {
  term : view;
  ty : t option;
  loc : location option;
}
and view =
  | Var of string             (** variable *)
  | BVar of string            (** bound variable *)
  | Const of LogtkSymbol.t         (** constant *)
  | App of t * t list         (** apply term *)
  | Bind of LogtkSymbol.t * t * t  (** bind variable in term *)
  | Multiset of t list
  | Record of (string * t) list * t option  (** extensible record *)

type term = t

let view t = t.term
let loc t = t.loc
let ty t = t.ty

let __to_int = function
  | Var _ -> 0
  | BVar _ -> 1
  | Const _ -> 3
  | App _ -> 4
  | Bind _ -> 5
  | Multiset _ -> 6
  | Record _ -> 7

let rec cmp t1 t2 = match t1.term, t2.term with
  | Var s1, Var s2
  | BVar s1, BVar s2 -> String.compare s1 s2
  | Const s1, Const s2 -> LogtkSymbol.cmp s1 s2
  | App (s1,l1), App (s2, l2) ->
      CCOrd.(
        cmp s1 s2
        <?> (CCOrd.list_ cmp, l1, l2)
      )
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
      CCOrd.(
        LogtkSymbol.cmp s1 s2
        <?> (cmp, v1, v2)
        <?> (cmp, t1, t2)
      )
  | Multiset l1, Multiset l2 ->
      let l1 = List.sort cmp l1 and l2 = List.sort cmp l2 in
      CCOrd.list_ cmp l1 l2
  | Record (l1, rest1), Record (l2, rest2) ->
      CCOrd.(
        CCOpt.compare cmp rest1 rest2
        <?> (cmp_fields, l1, l2)
      )
  | Var _, _
  | BVar _, _
  | Const _, _
  | App _, _
  | Bind _, _
  | Multiset _, _
  | Record _, _ -> __to_int t1.term - __to_int t2.term
and cmp_field x y = CCOrd.pair String.compare cmp x y
and cmp_fields x y = CCOrd.list_ cmp_field x y

let eq t1 t2 = cmp t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var s -> h |> Hash.string_ "var" |> Hash.string_ s
  | BVar s -> h |> Hash.string_ "bvar" |> Hash.string_ s
  | Const s -> h |> Hash.string_ "const" |> LogtkSymbol.hash_fun s
  | App (s, l) -> h |> Hash.string_ "app" |> hash_fun s |> Hash.list_ hash_fun l
  | Multiset l ->
    h |> Hash.string_ "multiset" |> Hash.list_ hash_fun l
  | Bind (s,v,t') ->
    h |> LogtkSymbol.hash_fun s |> hash_fun t' |> hash_fun v
  | Record (l, rest) ->
    h |> Hash.opt hash_fun rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l

let hash x = Hash.apply hash_fun x

let _pp_list p buf l = CCList.pp ~start:"" ~stop:"" ~sep:", " p buf l

let rec pp buf t = match view t with
  | Var s
  | BVar s ->
      Buffer.add_string buf s;
      begin match ty t with
      | None -> ()
      | Some ty -> Printf.bprintf buf ":%a" _pp_inner ty
      end
  | Const s -> LogtkSymbol.pp buf s
  | App (f, []) -> assert false
  | App (f, l) ->
      Printf.bprintf buf "%a(%a)" _pp_inner f (_pp_list pp) l
  | Bind (s, v, t) ->
      Printf.bprintf buf "%a %a. %a" LogtkSymbol.pp s _pp_inner v _pp_inner t
  | Record (l, None) ->
      Printf.bprintf buf "{%a}" (_pp_list _pp_field) l
  | Record (l, Some r) ->
      Printf.bprintf buf "{%a | %a}" (_pp_list _pp_field) l pp r
  | Multiset l ->
      Printf.bprintf buf "[%a]" (_pp_list _pp_inner) l
and _pp_inner buf t = match view t with
  | Bind _ -> Printf.bprintf buf "(%a)" pp t  (* avoid ambiguities *)
  | _ -> pp buf t
and _pp_field buf (name,t) =
  Printf.bprintf buf "%s=%a" name _pp_inner t

exception IllFormedTerm of string

let _make ?loc ?ty view = {term=view; loc; ty; }

let var ?loc ?ty s = _make ?loc ?ty (Var s)

let bvar ?loc ?ty s = _make ?loc ?ty (BVar s)

let const ?loc ?ty s = _make ?loc ?ty (Const s)

let app ?loc ?ty s l = match l with
  | [] -> s
  | _::_ -> _make ?loc ?ty (App(s,l))

let bind ?loc ?ty s v l =
  match v.term with
  | BVar _ -> _make ?loc ?ty (Bind(s,v,l))
  | _ ->
      let msg = CCPrint.sprintf "in binder, expected variable, got %a" pp v in
      raise (IllFormedTerm msg)

let bind_list ?loc ?ty s vars t =
  List.fold_right (fun v t -> bind ?loc ?ty s v t) vars t

let multiset ?loc ?ty l = _make ?loc ?ty (Multiset l)

let record ?loc ?ty l ~rest =
  match rest with
  | None
  | Some {term=Var _} ->
      let l = List.sort cmp_field l in
      _make ?loc ?ty (Record (l, rest))
  | Some {term=Record (l', rest')} ->
      let l = List.sort cmp_field (l@l') in
      _make ?loc ?ty (Record(l, rest'))
  | Some t' ->
      let msg = CCPrint.sprintf "ill-formed record row: %a" pp t' in
      raise (IllFormedTerm msg)

let at_loc ?loc t = {t with loc; }
let with_ty ?ty t = {t with ty; }

let of_int ?ty i = const ?ty (LogtkSymbol.of_int i)
let of_string ?loc ?ty s = const ?loc ?ty (LogtkSymbol.of_string s)

let tType = const LogtkSymbol.Base.tType
let wildcard = const LogtkSymbol.Base.wildcard

let _gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name = if i=0
      then CCPrint.sprintf "'_%c" names.[j]
      else CCPrint.sprintf "'_%c%d" names.[j] i
    in
    incr r;
    name

let fresh_var ?loc ?ty () = var ?loc ?ty (_gensym ())
let fresh_bvar ?loc ?ty () = bvar ?loc ?ty (_gensym ())

(** {2 LogtkUtils} *)

let is_var = function | {term=Var _} -> true | _ -> false
let is_bvar = function | {term=BVar _} -> true | _ -> false

let rec ground t =
  CCOpt.maybe ground true t.ty
  &&
  match t.term with
  | Var _ -> false
  | BVar _
  | Const _ -> true
  | App (f, l) -> ground f && List.for_all ground l
  | Bind (_, v, t') -> ground v && ground t'
  | Record (l, rest) ->
      CCOpt.maybe ground true rest
      &&
      List.for_all (fun (_,t') -> ground t') l
  | Multiset l -> List.for_all ground l

module Set = Sequence.Set.Make(struct type t = term let compare = cmp end)
module Map = Sequence.Map.Make(struct type t = term let compare = cmp end)
module Tbl = Hashtbl.Make(struct type t = term let equal = eq let hash = hash end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      CCOpt.iter iter t.ty;
      match t.term with
      | Var _
      | BVar _
      | Const _ -> ()
      | App (f, l) -> iter f; List.iter iter l
      | Bind (_, v, t') -> iter v; iter t'
      | Record (l, rest) ->
          CCOpt.iter iter rest;
          List.iter (fun (_,t) -> iter t) l
      | Multiset l -> List.iter iter l
    in iter t

  let vars t = subterms t |> Sequence.filter is_var

  let subterms_with_bound t k =
    let rec iter set t =
      k (t,set);
      CCOpt.iter (iter set) t.ty;
      match t.term with
      | Var _
      | BVar _
      | Const _ -> ()
      | App (f, l) -> iter set f; List.iter (iter set) l
      | Bind (_, v, t') ->
          let set' = Set.add v set in
          iter set' v; iter set' t'
      | Record (l, rest) ->
          CCOpt.iter (iter set) rest;
          List.iter (fun (_,t) -> iter set t) l
      | Multiset l -> List.iter (iter set) l
    in iter Set.empty t
end

let vars t = Seq.vars t |> Set.of_seq |> Set.to_list

module Visitor = struct
  type 'a t = {
    var : term -> ?loc:location -> ?ty:'a -> string -> 'a;
    bvar : term -> ?loc:location -> ?ty:'a -> string -> 'a;
    app : term -> ?loc:location -> ?ty:'a -> 'a -> 'a list -> 'a;
    const : term -> ?loc:location -> ?ty:'a -> LogtkSymbol.t -> 'a;
    bind : term -> ?loc:location -> ?ty:'a -> LogtkSymbol.t -> 'a -> 'a -> 'a;
    multiset : term -> ?loc:location -> ?ty:'a -> 'a list -> 'a;
    record : term -> ?loc:location -> ?ty:'a ->
            (string*'a) list -> 'a option -> 'a;
  }

  let apply ~visitor t =
    let rec _apply t =
      let ty = CCOpt.map _apply t.ty in
      let loc = t.loc in
      match t.term with
      | Var s -> visitor.var t ?loc ?ty s
      | BVar s -> visitor.bvar t ?loc ?ty s
      | App (f,l) -> visitor.app t ?loc ?ty (_apply f) (_apply_list l)
      | Const s -> visitor.const t ?loc ?ty s
      | Bind (s, v, t) ->
          visitor.bind t ?loc ?ty s (_apply v) (_apply t)
      | Multiset l -> visitor.multiset t ?loc ?ty (_apply_list l)
      | Record (l, rest) ->
          visitor.record t ?loc ?ty
            (List.map (fun (n,t) -> n, _apply t) l)
            (CCOpt.map _apply rest)
    and _apply_list l = List.map _apply l
    in
    _apply t

  let id = {
    var=CCFun.const var; bvar=CCFun.const bvar;
    const=CCFun.const const; app=CCFun.const app; bind=CCFun.const bind;
    multiset=CCFun.const multiset;
    record=(fun _t ?loc ?ty l rest -> record ?loc ?ty l ~rest);
  }

  let _id x = x
  let _opt o = CCOpt.maybe _id true o
  let _true _ ?loc ?ty _ = _opt ty

  let for_all = {
    var=_true; const=_true; bvar=_true;
    app=(fun _ ?loc ?ty f l -> _opt ty && f && List.for_all _id l);
    bind=(fun _ ?loc ?ty _ v t -> _opt ty && v && t);
    multiset=(fun _ ?loc ?ty l -> _opt ty && List.for_all _id l);
    record=(fun _ ?loc ?ty l rest -> _opt ty && _opt rest
      && List.for_all snd l)
  }
end
  
let closed t =
  Seq.subterms_with_bound t
  |> Sequence.filter (CCFun.compose fst is_bvar)
  |> Sequence.for_all (fun (v,set) -> Set.mem v set)

let close_all ?ty s t = bind_list ?ty s (vars t) t

let to_string = CCPrint.to_string pp
let fmt fmt t = Format.pp_print_string fmt (to_string t)

let _pp_term = pp

(** {2 Substitutions, LogtkUnification} *)

type 'a or_error = [`Error of string | `Ok of 'a]

module Subst = struct
  type t = term Map.t

  let empty = Map.empty

  let pp buf subst =
    Map.to_seq subst
    |> CCPrint.seq ~start:"{" ~stop:"}" ~sep:"," (CCPair.pp _pp_term _pp_term) buf
  let to_string = CCPrint.to_string pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)

  let add subst v t =
    assert (is_var v);
    if Map.mem v subst
      then invalid_arg
        (LogtkUtil.sprintf "var %a already bound in %a" _pp_term v pp subst);
    Map.add v t subst

  let rec eval_head subst t =
    let ty = CCOpt.map (eval_head subst) t.ty in
    match t.term with
    | Var _ ->
        begin try
          let t' = Map.find t subst in
          eval_head subst t'
        with Not_found ->
          with_ty ?ty t
        end
    | BVar _
    | Const _
    | App _
    | Bind _
    | Record _
    | Multiset _ -> with_ty ?ty t

  let rec eval subst t =
    let ty = CCOpt.map (eval subst) t.ty in
    match t.term with
    | Var _ ->
        begin try
          let t' = Map.find t subst in
          eval subst t'
        with Not_found ->
          with_ty ?ty t
        end
    | BVar _
    | Const _ -> with_ty ?ty t
    | App (f, l) ->
        app ?loc:t.loc ?ty (eval subst f) (eval_list subst l)
    | Bind (s, v, t) ->
        bind ?loc:t.loc ?ty s (eval subst v) (eval subst t)
    | Record (l, rest) ->
        record ?loc:t.loc ?ty
          (List.map (CCPair.map2 (eval subst)) l)
          ~rest:(CCOpt.map (eval subst) rest)
    | Multiset l ->
        multiset ?loc:t.loc ?ty (eval_list subst l)
  and eval_list subst l = List.map (eval subst) l
end

let rename t =
  let subst = List.fold_left
    (fun s v -> Subst.add s v (fresh_var ?loc:v.loc ?ty:v.ty ()))
    Subst.empty (vars t)
  in
  Subst.eval subst t

exception LogtkUnifyFailure of term * term * Subst.t

let _pp_failure buf (t1,t2,subst) =
  Printf.bprintf buf "could not unify %a and %a (in context %a)"
  pp t1 pp t2 Subst.pp subst

let _fail_unif t1 t2 subst = raise (LogtkUnifyFailure (t1,t2,subst))

let _occur_check ~subst v t =
  assert (is_var v);
  let rec check t =
    CCOpt.maybe check false t.ty ||
    match view (Subst.eval_head subst t) with
    | Var _
    | BVar _
    | Const _ -> false
    | App (f, l) -> check f || List.exists check l
    | Bind (_, v, t) -> check v || check t
    | Multiset l -> List.exists check l
    | Record (l, rest) ->
        CCOpt.maybe check false rest ||
        List.exists (fun (_,t) -> check t) l
  in
  check t

let rec _unify_exn subst t1 t2 =
  let t1 = Subst.eval_head subst t1
  and t2 = Subst.eval_head subst t2 in
  let subst = match t1.ty, t2.ty with
    | None, None -> subst
    | Some ty1, Some ty2 -> _unify_exn subst ty1 ty2
    | Some _, None
    | None, Some _ -> _fail_unif t1 t2 subst
  in
  match t1.term, t2.term with
  | Var _, _ ->
      if _occur_check subst t1 t2 || not (closed t2)
        then _fail_unif t1 t2 subst
        else Subst.add subst t1 t2
  | _, Var _ ->
      if _occur_check subst t2 t1 || not (closed t1)
        then _fail_unif t1 t2 subst
        else Subst.add subst t2 t1
  | BVar i, BVar j ->
      if i=j then subst else _fail_unif t1 t2 subst
  | App (f1,l1), App (f2,l2) when List.length l1=List.length l2 ->
      let subst = _unify_exn subst f1 f2 in
      _unify_list subst l1 l2
  | Multiset l1, Multiset l2 when List.length l1 = List.length l2 ->
      (* unification is n-ary, so we choose the first satisfying subst, if any *)
      _unify_multi subst l1 l2
      |> Sequence.take 1
      |> Sequence.to_list
      |> (function
        | [subst] -> subst
        | _ -> _fail_unif t1 t2 subst
        )
  | Record (l1, r1), Record (l2, r2) ->
      let subst, rest1, rest2 = _unify_record_fields subst l1 l2 in
      let fail() = _fail_unif t1 t2 subst in
      let subst = _unify_record_rest subst ~fail ?ty:t1.ty r2 rest1 in
      _unify_record_rest subst ~fail ?ty:t2.ty r1 rest2
  | BVar _, _
  | Const _, _
  | App _, _
  | Bind _, _
  | Multiset _, _
  | Record _, _ -> _fail_unif t1 t2 subst
and _unify_list subst l1 l2 =
  List.fold_left2 _unify_exn subst l1 l2
and _unify_multi subst l1 l2 k = match l1 with
  | [] ->
      assert (l2 = []);
      k subst
  | t1::l1' ->
      _unify_multiset_with subst t1 l1' [] l2 k
and _unify_multiset_with subst t1 l1 rest2 l2 k = match l2 with
  | [] -> ()
  | t2::l2' ->
      begin try
        let subst = _unify_exn subst t1 t2 in
        _unify_multi subst l1 (rest2@l2') k
      with LogtkUnifyFailure _ -> ()
      end;
      _unify_multiset_with subst t1 l1 (t2::rest2) l2' k
and _unify_record_fields subst l1 l2 = match l1, l2 with
  | [], _
  | _, [] -> subst, l1, l2
  | (n1,t1)::l1', (n2,t2)::l2' ->
      if n1=n2
      then
        let subst = _unify_exn subst t1 t2 in
        _unify_record_fields subst l1' l2'
      else if n1<n2
      then
        let subst, rest1, rest2 = _unify_record_fields subst l1' l2 in
        subst, (n1,t1)::rest1, rest2
      else
        let subst, rest1, rest2 = _unify_record_fields subst l1 l2' in
        subst, rest1, (n2,t2)::rest2
and _unify_record_rest ~fail ?ty subst r rest = match r, rest with
  | None, [] -> subst
  | None, _::_ -> fail()
  | Some ({term=Var _} as r), _ ->
      let t' = record ?ty rest ~rest:None in
      if _occur_check subst r t'
        then fail()
        else Subst.add subst r t'
  | Some _, _ -> assert false

let unify_exn ?(subst=Subst.empty) t1 t2 =
  _unify_exn subst t1 t2

let unify ?(subst=Subst.empty) t1 t2 =
  try CCError.return (_unify_exn subst t1 t2)
  with LogtkUnifyFailure (t1,t2,subst) ->
    CCError.fail
      (CCPrint.sprintf "could not unify %a and %a (in context %a)"
        pp t1 pp t2 Subst.pp subst)
