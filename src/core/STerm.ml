
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 S-like Terms}. *)

type location = ParseLocation.t

type var =
  | V of string
  | Wildcard

type t = {
  term : view;
  loc : location option;
}

and match_branch =
  | Match_case of string * var list * t
  | Match_default of t

and view =
  | Var of var (** variable *)
  | Const of string (** constant *)
  | AppBuiltin of Builtin.t * t list
  | App of t * t list (** apply term *)
  | Ite of t * t * t
  | Match of t * match_branch list
  | Let of (var * t) list * t
  | Bind of Binder.t * typed_var list * t (** bind n variables *)
  | List of t list (** special constructor for lists *)
  | Record of (string * t) list * var option (** extensible record *)

and typed_var = var * t option

type term = t

let view t = t.term
let loc t = t.loc

let to_int_ = function
  | Var _ -> 0
  | Const _ -> 3
  | AppBuiltin _ -> 4
  | App _ -> 5
  | Bind _ -> 6
  | List _ -> 7
  | Record _ -> 8
  | Ite _ -> 9
  | Match _ -> 10
  | Let _ -> 11

let rec compare t1 t2 = match t1.term, t2.term with
  | Var s1, Var s2 -> Pervasives.compare s1 s2
  | Const s1, Const s2 -> String.compare s1 s2
  | App (s1,l1), App (s2, l2) ->
    let c = compare s1 s2 in
    if c = 0
    then CCOrd.list compare l1 l2
    else c
  | AppBuiltin (s1,l1), AppBuiltin (s2,l2) ->
    let c = Builtin.compare s1 s2 in
    if c = 0
    then CCOrd.list compare l1 l2
    else c
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
    let c = Binder.compare s1 s2 in
    if c = 0
    then
      let c' = compare t1 t2 in
      if c' = 0
      then CCOrd.list compare_typed_var v1 v2
      else c'
    else c
  | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
    CCList.compare compare [a1;b1;c1] [a2;b2;c2]
  | Match (u1,l1), Match (u2,l2) ->
    let cmp_branch b1 b2 = match b1, b2 with
      | Match_case (s1,vars1,rhs1), Match_case (s2,vars2,rhs2) ->
        CCOrd.(String.compare s1 s2
          <?> (list compare_var, vars1,vars2)
          <?> (compare,rhs1,rhs2))
      | Match_default t1, Match_default t2 -> compare t1 t2
      | Match_case _, Match_default _ -> -1
      | Match_default _, Match_case _ -> 1
    in
    CCOrd.( compare u1 u2 <?> (list cmp_branch,l1,l2))
  | Let (l1,t1), Let (l2,t2) ->
    CCOrd.( compare t1 t2
      <?> (list (pair compare_var compare), l1, l2))
  | (Var _,_)
  | (Const _,_)
  | (Ite _, _)
  | (Match _, _)
  | (Let _, _)
  | (AppBuiltin (_,_),_)
  | (App (_,_),_)
  | (Bind (_,_,_),_)
  | (List _,_)
  | (Record (_,_),_) -> to_int_ t1.term - to_int_ t2.term

and compare_typed_var (v1,o1)(v2,o2) =
  let cmp = compare in
  CCOrd.( compare_var v1 v2 <?> (Util.ord_option cmp, o1, o2) )

and compare_var : var CCOrd.t = Pervasives.compare

let equal t1 t2 = compare t1 t2 = 0

let rec hash t = match t.term with
  | Var v -> hash_var v
  | Const s -> Hash.string s
  | AppBuiltin (s,l) -> Hash.combine3 10 (Builtin.hash s) (Hash.list hash l)
  | App (s, l) -> Hash.combine3 20 (hash s) (Hash.list hash l)
  | List l -> Hash.combine2 30 (Hash.list hash l)
  | Bind (s,v,t') ->
    Hash.combine4 40 (Binder.hash s) (Hash.list hash_ty_var v) (hash t')
  | Ite (a,b,c) ->
    Hash.combine4 50 (hash a) (hash b) (hash c)
  | Match (u, _) -> Hash.combine2 60 (hash u)
  | Let (_, u) -> Hash.combine2 70 (hash u)
  | Record (l, rest) ->
    Hash.combine3 80
      (Hash.opt hash_var rest)
      (Hash.list (Hash.pair Hash.string hash) l)

and hash_ty_var (v,ty) =
  Hash.combine3 42 (hash_var v) (Hash.opt hash ty)

and hash_var v = match v with
  | V s -> Hash.string s
  | Wildcard -> Hash.string "_"

let make_ ?loc view = {term=view; loc;}

let mk_var ?loc v = make_ ?loc (Var v)
let var ?loc s = mk_var ?loc (V s)
let v_wild = mk_var Wildcard
let builtin ?loc s = make_ ?loc (AppBuiltin (s,[]))
let app_builtin ?loc s l = make_ ?loc (AppBuiltin(s,l))
let app ?loc s l = match s.term, l with
  | _, [] -> s
  | AppBuiltin (s1,l1), _ -> app_builtin ?loc s1 (l1@l)
  | App (s1,l1), _::_ -> make_ ?loc (App (s1,l1@l))
  | _, _::_ -> make_ ?loc (App(s,l))
let const ?loc s = make_ ?loc (Const s)
let app_const ?loc s l = app (const ?loc s) l
let bind ?loc s v l = match v, l with
  | [], _ -> l
  | _::_, {term=Bind(s',v',l'); _} when s=s' ->
    make_ ?loc (Bind (s, v@v', l')) (* flatten *)
  | _::_, _ -> make_ ?loc (Bind(s,v,l))
let ite ?loc a b c = make_ ?loc (Ite (a,b,c))
let match_ ?loc t l = make_ ?loc (Match (t, l))
let let_ ?loc l u = match l with
  | [] -> u
  | _ -> make_ ?loc (Let (l,u))
let list_ ?loc l = make_ ?loc (List l)
let nil = list_ []
let record ?loc l ~rest =
  let l = List.sort (fun (n1,_)(n2,_) -> String.compare n1 n2) l in
  make_ ?loc (Record (l, rest))
let at_loc ~loc t = {t with loc=Some loc; }

let wildcard = builtin Builtin.wildcard

let is_app = function {term=App _; _} -> true | _ -> false
let is_var = function | {term=Var _; _} -> true | _ -> false

let true_ = builtin Builtin.true_
let false_ = builtin Builtin.false_

let and_ ?loc = function [] -> true_ | [x] -> x | l -> app_builtin ?loc Builtin.and_ l
let or_ ?loc = function [] -> false_ | [x] -> x | l -> app_builtin ?loc Builtin.or_ l
let not_ ?loc a = app_builtin ?loc Builtin.not_ [a]
let equiv ?loc a b = app_builtin ?loc Builtin.equiv [a;b]
let xor ?loc a b = app_builtin ?loc Builtin.xor [a;b]
let imply ?loc a b = app_builtin ?loc Builtin.imply [a;b]
let eq ?loc a b = app_builtin ?loc Builtin.eq [a;b]
let neq ?loc a b = app_builtin ?loc Builtin.neq [a;b]
let forall ?loc vars f = bind ?loc Binder.forall vars f
let exists ?loc vars f = bind ?loc Binder.exists vars f
let lambda ?loc vars f = bind ?loc Binder.lambda vars f

let int_ i = builtin (Builtin.Int i)
let of_int i = int_ (Z.of_int i)
let rat n = builtin (Builtin.Rat n)
let real r = builtin (Builtin.Real r)

let fun_ty ?loc l ret = match l with
  | [] -> ret
  | _::_ -> app_builtin ?loc Builtin.arrow (ret :: l)

let tType = builtin Builtin.tType
let term = builtin Builtin.Term
let prop = builtin Builtin.Prop
let ty_int = builtin Builtin.TyInt
let ty_rat = builtin Builtin.TyRat
let ty_real = builtin Builtin.TyReal
let forall_ty ?loc vars t = bind ?loc Binder.forall_ty vars t

let ty_unfold =
  let rec aux acc ty = match ty.term with
    | AppBuiltin (Builtin.Arrow, ret :: l) ->
      aux (l@acc) ret
    | _ -> acc, ty
  in
  aux []

let unfold_bind b =
  let rec aux acc t = match t.term with
    | Bind (b', vars, t) when b=b' ->
      aux (List.rev_append vars acc) t
    | _ -> List.rev acc, t
  in
  aux []

module AsKey = struct
  type t = term
  let compare = compare
  let hash = hash
  let equal = equal
end

module Set = CCSet.Make(AsKey)
module Map = CCMap.Make(AsKey)
module Tbl = CCHashtbl.Make(AsKey)

module StringSet = CCSet.Make(String)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      match t.term with
        | Var _ | Const _ -> ()
        | List l
        | AppBuiltin (_,l) -> List.iter iter l
        | App (f, l) -> iter f; List.iter iter l
        | Ite (a,b,c) -> iter a; iter b; iter c
        | Let (l,u) -> iter u; List.iter (fun (_,t) -> iter t) l
        | Match (u,l) ->
          iter u;
          List.iter
            (function
              | Match_case (_,_,rhs)
              | Match_default rhs -> iter rhs)
            l
        | Bind (_, _, t') -> iter t'
        | Record (l, _) ->
          List.iter (fun (_,t') -> iter t') l
    in iter t

  let vars t = subterms t
               |> Sequence.filter_map
                 (fun t -> match t.term with
                    | Var v -> Some v
                    | _ -> None)

  let subterms_with_bound t k =
    let add_var set = function
      | Wildcard -> set
      | V s -> StringSet.add s set
    in
    let add_typed_var set (v,_) = add_var set v in
    let rec iter bound t =
      k (t, bound);
      match t.term with
        | Var _ | Const _ -> ()
        | AppBuiltin (_, l)
        | List l -> List.iter (iter bound) l
        | App (f, l) -> iter bound f; List.iter (iter bound) l
        | Bind (_, v, t') ->
          (* add variables of [v] to the set *)
          let bound' = List.fold_left add_typed_var bound v in
          iter bound' t'
        | Ite (a,b,c) -> iter bound a; iter bound b; iter bound c
        | Let (l,u) ->
          let bound' =
            List.fold_left
              (fun bound' (v,u) -> iter bound u; add_var bound' v)
              bound l
          in
          iter bound' u
        | Match (u,l) ->
          iter bound u;
          List.iter
            (function
              | Match_case (_,vars,rhs) ->
                let bound' = List.fold_left add_var bound vars in
                iter bound' rhs
              | Match_default rhs -> iter bound rhs)
            l
        | Record (l, _) ->
          List.iter (fun (_,t') -> iter bound t') l
    in iter StringSet.empty t

  let free_vars t =
    subterms_with_bound t
    |> Sequence.filter_map
      (fun (t,bound) -> match t.term with
         | Var (V v) when not (StringSet.mem v bound) -> Some v
         | _ -> None)

  let symbols t = subterms t
                  |> Sequence.filter_map
                    (function
                      | {term=Const s; _} -> Some s
                      | _ -> None)
end

let ground t = Seq.vars t |> Sequence.is_empty

let close_all s t =
  let vars = Seq.free_vars t
             |> StringSet.of_seq
             |> StringSet.elements
             |> List.map (fun v->V v,None)
  in
  bind s vars t

let subterm ~strict t ~sub =
  (not strict && equal t sub)
  || Sequence.exists (equal sub) (Seq.subterms t)

(** {2 Print} *)

let rec pp out t = match t.term with
  | Var v -> pp_var out v
  | Const s -> CCFormat.string out s
  | List l ->
    Format.fprintf out "[@[<hv>%a@]]"
      (Util.pp_list ~sep:"," pp) l;
  | AppBuiltin (Builtin.TType, []) -> CCFormat.string out "type"
  | AppBuiltin (Builtin.TyInt, []) -> CCFormat.string out "int"
  | AppBuiltin (Builtin.TyRat, []) -> CCFormat.string out "rat"
  | AppBuiltin (Builtin.Arrow, [ret;a]) ->
    Format.fprintf out "@[<2>%a@ → %a@]" pp_inner a pp_inner ret
  | AppBuiltin (Builtin.Arrow, ret::l) ->
    Format.fprintf out "@[<2>(%a)@ → %a@]" (Util.pp_list ~sep:" × " pp_inner) l pp_inner ret
  | AppBuiltin (b, []) -> Builtin.pp out b
  | AppBuiltin (Builtin.Not, [f]) -> Format.fprintf out "@[¬@ %a@]" pp_inner f
  | AppBuiltin (b, ([t1;t2] | [_;t1;t2])) when Builtin.fixity b = Builtin.Infix_binary ->
    Format.fprintf out "@[%a %a@ %a@]"  pp_inner t1 Builtin.pp b pp_inner t2
  | AppBuiltin (b, l) when Builtin.is_infix b && List.length l > 0 ->
    pp_infix_ b out l
  | AppBuiltin (s, l) ->
    Format.fprintf out "%a(@[<2>%a@])"
      Builtin.pp s (Util.pp_list ~sep:", " pp_inner) l
  | App (s, l) ->
    Format.fprintf out "@[<2>%a %a@]"
      pp s (Util.pp_list ~sep:" " pp_inner) l
  | Bind (s, vars, t') ->
    Format.fprintf out "@[<2>%a @[%a@].@ %a@]"
      Binder.pp s
      (Util.pp_list ~sep:" " pp_typed_var) vars
      pp t'
  | Ite (a,b,c) ->
    Format.fprintf out "@[<2>if %a@ then %a@ else %a@]" pp a pp b pp c
  | Let (l, u) ->
    let pp_binding out (v,t) = Format.fprintf out "@[%a := %a@]" pp_var v pp t in
    Format.fprintf out "@[<2>let %a@ in %a@]"
      (Util.pp_list ~sep:" and " pp_binding) l pp u
  | Match (u, l) ->
    let pp_branch out = function
      | Match_default rhs -> Format.fprintf out "_ -> %a" pp rhs
      | Match_case (c,vars,rhs) ->
        Format.fprintf out "@[<2>case@ %s %a ->@ %a@]"
          c (Util.pp_list ~sep:" " pp_var) vars pp rhs
    in
    Format.fprintf out "@[<hv>@[<hv2>match %a with@ @[<hv>%a@]@]@ end@]"
      pp u (Util.pp_list ~sep:" | " pp_branch) l
  | Record (l, None) ->
    Format.fprintf out "{@[<hv>%a@]}"
      (Util.pp_list (fun out (s,t') -> Format.fprintf out "%s:%a" s pp t')) l;
  | Record (l, Some r) ->
    Format.fprintf out "{@[<hv>%a@ | %a@]}"
      (Util.pp_list (Util.pp_pair ~sep:":" CCFormat.string pp)) l
      pp_var r
and pp_inner out t = match view t with
  | AppBuiltin (_, _::_)
  | App _
  | Bind _ -> Format.fprintf out "(@[%a@])" pp t  (* avoid ambiguities *)
  | _ -> pp out t
and pp_typed_var out = function
  | v, None -> pp_var out v
  | v, Some ty -> Format.fprintf out "%a:%a" pp_var v pp ty
and pp_infix_ b out l = match l with
  | [] -> assert false
  | [t] -> pp_inner out t
  | t :: l' ->
    Format.fprintf out "@[<hv>%a@ @[<hv>%a@ %a@]@]"
      pp_inner t Builtin.pp b (pp_infix_ b) l'
and pp_var out = function
  | Wildcard -> CCFormat.string out "_"
  | V s -> CCFormat.string out s

let to_string = CCFormat.to_string pp

(** {2 TPTP} *)

module TPTP = struct
  let pp_id out s =
    if Util.tstp_needs_escaping s
    then CCFormat.fprintf out "'%s'" s
    else CCFormat.string out s

  let rec pp out t = match t.term with
    | Var v -> pp_var out v
    | Const s -> pp_id out s
    | List l ->
      Format.fprintf out "[@[<hv>%a@]]"
        (Util.pp_list ~sep:"," pp) l;
    | AppBuiltin (Builtin.And, l) ->
      Util.pp_list ~sep:" & " pp_surrounded out l
    | AppBuiltin (Builtin.Or, l) ->
      Util.pp_list ~sep:" | " pp_surrounded out l
    | AppBuiltin (Builtin.Not, [a]) ->
      Format.fprintf out "@[<1>~@,@[%a@]@]" pp_surrounded a
    | AppBuiltin (Builtin.Imply, [a;b]) ->
      Format.fprintf out "@[%a =>@ %a@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Xor, [a;b]) ->
      Format.fprintf out "@[%a <~>@ %a@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Equiv, [a;b]) ->
      Format.fprintf out "@[%a <=>@ %a@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Eq, ([_;a;b] | [a;b])) ->
      Format.fprintf out "@[%a =@ %a@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Neq, ([_;a;b] | [a;b])) ->
      Format.fprintf out "@[%a !=@ %a@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Arrow, [ret;a]) ->
      Format.fprintf out "@[<2>%a >@ %a@]" pp a pp ret
    | AppBuiltin (Builtin.Arrow, ret::l) ->
      Format.fprintf out "@[<2>(@[<hv>%a@]) >@ %a@]" (Util.pp_list~sep:" * " pp) l pp_surrounded ret
    | AppBuiltin (s, []) -> Builtin.TPTP.pp out s
    | AppBuiltin (s, l) ->
      Format.fprintf out "@[%a(@[%a@])@]" Builtin.TPTP.pp s (Util.pp_list ~sep:", " pp_surrounded) l
    | App (s, l) ->
      Format.fprintf out "@[%a(@[%a@])@]"
        pp s (Util.pp_list ~sep:"," pp) l
    | Bind (s, vars, t') ->
      Format.fprintf out "@[<2>%a[@[%a@]]:@ %a@]"
        Binder.TPTP.pp s
        (Util.pp_list ~sep:"," pp_typed_var) vars
        pp_surrounded t'
    | Ite _ -> failwith "cannot print `ite` in TPTP"
    | Let _ -> failwith "cannot print `let` in TPTP"
    | Match _ -> failwith "cannot print `match` in TPTP"
    | Record _ -> failwith "cannot print records in TPTP"
  and pp_typed_var out (v,o) = match o with
    | None -> pp_var out v
    | Some {term=AppBuiltin (Builtin.Term,[]); _} ->
      pp_var out v (* implicit type *)
    | Some ty -> Format.fprintf out "%a:%a" pp_var v pp_surrounded ty
  and pp_surrounded out t = match t.term with
    | AppBuiltin (_, _::_)
    | Bind _ -> Format.fprintf out "(@[%a@])" pp t
    | _ -> pp out t
  and pp_var out = function
    | Wildcard -> CCFormat.string out "$_"
    | V s -> Util.pp_var_tstp out s

  let to_string = CCFormat.to_string pp
end

(** {2 ZF} *)

module ZF = struct
  let pp_id = TPTP.pp_id
  let rec pp out t = match t.term with
    | Var v -> pp_var out v
    | Const s -> pp_id out s
    | List l ->
      Format.fprintf out "[@[<hv>%a@]]"
        (Util.pp_list ~sep:"," pp) l;
    | AppBuiltin (Builtin.TType, _) -> CCFormat.string out "type"
    | AppBuiltin (Builtin.TyInt, _) -> CCFormat.string out "int"
    | AppBuiltin (Builtin.And, l) ->
      Format.fprintf out "@[<hv>%a@]"
        (Util.pp_list ~sep:" && " pp_surrounded) l
    | AppBuiltin (Builtin.Or, l) ->
      Format.fprintf out "@[<hv>%a@]"
        (Util.pp_list ~sep:" || " pp_surrounded) l
    | AppBuiltin (Builtin.Not, [a]) ->
      Format.fprintf out "@[<2>~@ @[%a@]@]" pp_surrounded a
    | AppBuiltin (Builtin.Imply, [a;b]) ->
      Format.fprintf out "@[@[%a@]@ => @[%a@]@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Xor, [a;b]) ->
      Format.fprintf out "@[<3>~ (@[%a@]@ <=> @[%a@])@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Equiv, [a;b]) ->
      Format.fprintf out "@[@[%a@]@ <=> @[%a@]@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Eq, [_;a;b]) ->
      Format.fprintf out "@[@[%a@]@ = @[%a@]@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Neq, [_;a;b]) ->
      Format.fprintf out "@[@[%a@]@ != @[%a@]@]" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Arrow, [ret;a]) ->
      Format.fprintf out "@[@[%a@] ->@ @[%a@]@]" pp a pp ret
    | AppBuiltin (Builtin.Arrow, ret::l) ->
      Format.fprintf out "@[%a ->@ %a@]"
        (Util.pp_list~sep:" -> " pp_surrounded)
        l pp_surrounded ret
    | AppBuiltin (s, []) -> Builtin.ZF.pp out s
    | AppBuiltin (s, [a;b])
    | AppBuiltin (s, [_;a;b]) when Builtin.is_infix s ->
      Format.fprintf out "@[@[%a@]@ %a @[%a@]@]"
        pp_surrounded a Builtin.ZF.pp s pp_surrounded b
    | AppBuiltin (s, l) ->
      Format.fprintf out "@[%a@ %a@]"
        Builtin.ZF.pp s (Util.pp_list ~sep:", " pp_surrounded) l
    | App (s, l) ->
      Format.fprintf out "@[<2>%a@ %a@]"
        pp_surrounded s (Util.pp_list ~sep:" " pp_surrounded) l
    | Ite (a,b,c) ->
      Format.fprintf out "@[<2>if %a@ then %a@ else %a@]" pp a pp b pp c
    | Let (l, u) ->
      let pp_binding out (v,t) = Format.fprintf out "@[%a := %a@]" pp_var v pp t in
      Format.fprintf out "@[<2>let %a@ in %a@]"
        (Util.pp_list ~sep:" and " pp_binding) l pp u
    | Match (u, l) ->
      let pp_branch out = function
        | Match_default rhs -> Format.fprintf out "_ -> %a" pp rhs
        | Match_case (c,vars,rhs) ->
          Format.fprintf out "@[<2>case@ %s %a ->@ %a@]"
            c (Util.pp_list ~sep:" " pp_var) vars pp rhs
      in
      Format.fprintf out "@[<hv>@[<hv2>match %a with@ @[<hv>%a@]@]@ end@]"
        pp u (Util.pp_list ~sep:" | " pp_branch) l
    | Bind (s, vars, t') ->
      Format.fprintf out "@[<2>%a @[%a@].@ @[%a@]@]"
        Binder.ZF.pp s
        (Util.pp_list ~sep:" " pp_typed_var) vars
        pp_surrounded t'
    | Record _ -> failwith "cannot print records in ZF"
  and pp_typed_var out (v,o) = match o with
    | None -> pp_var out v
    | Some ty -> Format.fprintf out "(@[%a:%a@])" pp_var v pp_surrounded ty
  and pp_surrounded out t = match t.term with
    | AppBuiltin (_, _::_)
    | App _
    | Ite _
    | Bind _ -> Format.fprintf out "(@[%a@])" pp t
    | _ -> pp out t
  and pp_var out = function
    | Wildcard -> CCFormat.string out "_"
    | V s -> CCFormat.string out s

  let pp_inner = pp_surrounded
  let to_string = CCFormat.to_string pp
end

let pp_in = function
  | Output_format.O_zf -> ZF.pp
  | Output_format.O_tptp -> TPTP.pp
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent

(** {2 Subst} *)

module StrMap = CCMap.Make(String)

type subst = t StrMap.t

let empty_subst = StrMap.empty

let merge_subst a b =
  StrMap.merge_safe a b
    ~f:(fun _ v -> match v with
      | `Both (_,x) -> Some x (* favor right one *)
      | `Left x | `Right x -> Some x)

(* make fresh copy of [base] not bound in subst *)
let copy_fresh_ subst v : subst * var = match v with
  | Wildcard -> subst, Wildcard
  | V base ->
    let rec aux i =
      let v = Printf.sprintf "%s%d" base i in
      if StrMap.mem v subst then aux (i+1) else v
    in
    let new_base = aux 0 in
    StrMap.add base (var new_base) subst, V new_base

let copy_fresh_tyvar subst (v,ty) =
  let subst, v = copy_fresh_ subst v in
  subst, (v,ty)

let rec apply_subst (subst:subst) (t:t): t =
  let loc = t.loc in
  begin match t.term with
    | Var (V s) -> StrMap.get_or ~default:t s subst
    | Var Wildcard -> t
    | Const c -> const ?loc c
    | AppBuiltin (b, l) ->
      let l = List.map (apply_subst subst) l in
      app_builtin ?loc b l
    | App (hd, l) ->
      let hd = apply_subst subst hd in
      let l = List.map (apply_subst subst) l in
      app ?loc hd l
    | Ite (a,b,c) ->
      let a = apply_subst subst a in
      let b = apply_subst subst b in
      let c = apply_subst subst c in
      ite ?loc a b c
    | Match (u,l) ->
      let u = apply_subst subst u in
      let l =
        List.map
          (function
            | Match_default rhs -> Match_default (apply_subst subst rhs)
            | Match_case (c,vars,rhs) ->
              let subst, vars = CCList.fold_map copy_fresh_ subst vars in
              Match_case (c, vars, apply_subst subst rhs))
          l
      in
      match_ ?loc u l
    | Let (l, u) ->
      let subst', l =
        CCList.fold_map
          (fun subst' (v,t) ->
             let t = apply_subst subst t in
             let subst', v = copy_fresh_ subst' v in
             subst', (v,t))
          subst l
      in
      let u = apply_subst subst' u in
      let_ ?loc l u
    | Bind (b, vars, body) ->
      let subst, vars = CCList.fold_map copy_fresh_tyvar subst vars in
      let body = apply_subst subst body in
      bind ?loc b vars body
    | List l -> list_ ?loc (List.map (apply_subst subst) l)
    | Record (l, row) ->
      let l = List.map (fun (name,t) -> name, apply_subst subst t) l in
      record ?loc l ~rest:row
  end

