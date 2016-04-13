
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 S-like Terms}. *)

module Hash = CCHash

type location = ParseLocation.t

type var =
  | V of string
  | Wildcard

type t = {
  term : view;
  loc : location option;
}

and view =
  | Var of var (** variable *)
  | Const of string (** constant *)
  | AppBuiltin of Builtin.t * t list
  | App of t * t list (** apply term *)
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

let rec compare t1 t2 = match t1.term, t2.term with
  | Var s1, Var s2 -> Pervasives.compare s1 s2
  | Const s1, Const s2 -> String.compare s1 s2
  | App (s1,l1), App (s2, l2) ->
    let c = compare s1 s2 in
    if c = 0
    then CCOrd.list_ compare l1 l2
    else c
  | AppBuiltin (s1,l1), AppBuiltin (s2,l2) ->
    let c = Builtin.compare s1 s2 in
    if c = 0
    then CCOrd.list_ compare l1 l2
    else c
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
    let c = Binder.compare s1 s2 in
    if c = 0
    then
      let c' = compare t1 t2 in
      if c' = 0
      then CCOrd.list_ compare_typed_var v1 v2
      else c'
    else c
  | (Var _,_)
  | (Const _,_)
  | (AppBuiltin (_,_),_)
  | (App (_,_),_)
  | (Bind (_,_,_),_)
  | (List _,_)
  | (Record (_,_),_) -> to_int_ t1.term - to_int_ t2.term

and compare_typed_var (v1,o1)(v2,o2) =
  let cmp = compare in
  CCOrd.( Pervasives.compare v1 v2 <?> (Util.ord_option cmp, o1, o2) )

let equal t1 t2 = compare t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var v -> hash_var v h
  | Const s -> Hash.string_ s h
  | AppBuiltin (s,l) -> Builtin.hash_fun s (Hash.list_ hash_fun l h)
  | App (s, l) -> Hash.list_ hash_fun l (hash_fun s h)
  | List l -> Hash.list_ hash_fun l (Hash.int_ 42 h)
  | Bind (s,v,t') ->
    h |> Binder.hash_fun s |> hash_fun t' |> Hash.list_ hash_ty_var v
  | Record (l, rest) ->
    h |> Hash.opt hash_var rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l

and hash_ty_var (v,ty) h =
  hash_var v h |> Hash.opt hash_fun ty

and hash_var v h = match v with
  | V s -> Hash.string s h
  | Wildcard -> Hash.string "_" h

let hash x = Hash.apply hash_fun x

let make_ ?loc view = {term=view; loc;}

let mk_var ?loc v = make_ ?loc (Var v)
let var ?loc s = mk_var ?loc (V s)
let v_wild = mk_var Wildcard
let builtin ?loc s = make_ ?loc (AppBuiltin (s,[]))
let app_builtin ?loc s l = make_ ?loc (AppBuiltin(s,l))
let app ?loc s l = match l with
  | [] -> s
  | _::_ -> make_ ?loc (App(s,l))
let const ?loc s = make_ ?loc (Const s)
let bind ?loc s v l = match v with
  | [] -> l
  | _::_ -> make_ ?loc (Bind(s,v,l))
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

let and_ ?loc l = app_builtin ?loc Builtin.and_ l
let or_ ?loc l = app_builtin ?loc Builtin.or_ l
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

let fun_ty ?loc l ret = match l with
  | [] -> ret
  | _::_ -> app_builtin ?loc Builtin.arrow (ret :: l)

let tType = builtin Builtin.tType
let term = builtin Builtin.Term
let prop = builtin Builtin.Prop
let ty_int = builtin Builtin.TyInt
let ty_rat = builtin Builtin.TyRat
let forall_ty ?loc vars t = bind ?loc Binder.forall_ty vars t


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
    let rec iter bound t =
      k (t, bound);
      match t.term with
      | Var _ | Const _ -> ()
      | AppBuiltin (_, l)
      | List l -> List.iter (iter bound) l
      | App (f, l) -> iter bound f; List.iter (iter bound) l
      | Bind (_, v, t') ->
          (* add variables of [v] to the set *)
          let bound' =
            List.fold_left
              (fun set (v,_) -> match v with
                 | Wildcard -> set
                 | V s -> StringSet.add s set)
              bound v
          in
          iter bound' t'
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

let rec pp out t = match t.term with
  | Var v -> pp_var out v
  | Const s -> CCFormat.string out s
  | List l ->
      Format.fprintf out "[@[<hv>%a@]]"
        (Util.pp_list ~sep:"," pp) l;
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
  let rec pp out t = match t.term with
    | Var v -> pp_var out v
    | Const s -> CCFormat.string out s
    | List l ->
      Format.fprintf out "[@[<hv>%a@]]"
        (Util.pp_list ~sep:"," pp) l;
    | AppBuiltin (Builtin.And, l) ->
      Util.pp_list ~sep:" & " pp_surrounded out l
    | AppBuiltin (Builtin.Or, l) ->
      Util.pp_list ~sep:" | " pp_surrounded out l
    | AppBuiltin (Builtin.Not, [a]) ->
      Format.fprintf out "~%a" pp_surrounded a
    | AppBuiltin (Builtin.Imply, [a;b]) ->
      Format.fprintf out "%a => %a" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Xor, [a;b]) ->
      Format.fprintf out "%a <~> %a" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Equiv, [a;b]) ->
      Format.fprintf out "%a <=> %a" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Eq, [_;a;b]) ->
      Format.fprintf out "%a = %a" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Neq, [_;a;b]) ->
      Format.fprintf out "%a != %a" pp_surrounded a pp_surrounded b
    | AppBuiltin (Builtin.Arrow, [ret;a]) ->
      Format.fprintf out "%a > %a" pp a pp ret
    | AppBuiltin (Builtin.Arrow, ret::l) ->
      Format.fprintf out "(%a) > %a" (Util.pp_list~sep:" * " pp) l pp_surrounded ret
    | AppBuiltin (s, []) -> Builtin.TPTP.pp out s
    | AppBuiltin (s, l) ->
      Format.fprintf out "%a(%a)" Builtin.TPTP.pp s (Util.pp_list ~sep:", " pp_surrounded) l
    | App (s, l) ->
      Format.fprintf out "@[<2>%a(%a)@]"
        pp s (Util.pp_list ~sep:"," pp) l
    | Bind (s, vars, t') ->
        Format.fprintf out "@[<2>%a[@[%a@]]:@ %a@]"
          Binder.TPTP.pp s
          (Util.pp_list ~sep:"," pp_typed_var) vars
          pp_surrounded t'
    | Record _ -> failwith "cannot print records in TPTP"
  and pp_typed_var out (v,o) = match o with
    | None -> pp_var out v
    | Some {term=AppBuiltin ((Builtin.TType | Builtin.Term),[]); _} ->
        pp_var out v (* implicit type *)
    | Some ty -> Format.fprintf out "%a:%a" pp_var v pp_surrounded ty
  and pp_surrounded out t = match t.term with
    | AppBuiltin (_, _::_)
    | Bind _ -> Format.fprintf out "(@[%a@])" pp t
    | _ -> pp out t
  and pp_var out = function
    | Wildcard -> CCFormat.string out "$_"
    | V s -> CCFormat.string out s

  let to_string = CCFormat.to_string pp
end

(** {2 ZF} *)

module ZF = struct
  let rec pp out t = match t.term with
    | Var v -> pp_var out v
    | Const s -> CCFormat.string out s
    | List l ->
        Format.fprintf out "[@[<hv>%a@]]"
          (Util.pp_list ~sep:"," pp) l;
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
    | Bind (s, vars, t') ->
        Format.fprintf out "@[<2>%a @[%a@].@ @[%a@]@]"
          Binder.ZF.pp s
          (Util.pp_list ~sep:" " pp_typed_var) vars
          pp_surrounded t'
    | Record _ -> failwith "cannot print records in ZF"
  and pp_typed_var out (v,o) = match o with
    | None -> pp_var out v
    | Some {term=AppBuiltin (Builtin.TType ,[]); _} ->
        pp_var out v (* implicit type *)
    | Some ty -> Format.fprintf out "(@[%a:%a@])" pp_var v pp_surrounded ty
  and pp_surrounded out t = match t.term with
    | AppBuiltin (_, _::_)
    | App _
    | Bind _ -> Format.fprintf out "(@[%a@])" pp t
    | _ -> pp out t
  and pp_var out = function
    | Wildcard -> CCFormat.string out "_"
    | V s -> CCFormat.string out s

  let to_string = CCFormat.to_string pp
end
