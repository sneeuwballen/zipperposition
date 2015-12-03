
(*
Copyright (c) 2013, Simon Cruanes
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

(** {1 S-like Terms}. *)

module Hash = CCHash

type location = ParseLocation.t

type t = {
  term : view;
  loc : location option;
}
and view =
  | Var of string (** variable *)
  | Const of string (** constant *)
  | AppBuiltin of Builtin.t * t list
  | App of t * t list (** apply term *)
  | Bind of Binder.t * typed_var list * t (** bind n variables *)
  | List of t list (** special constructor for lists *)
  | Record of (string * t) list * string option (** extensible record *)

and typed_var = string * t option

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
  | Var s1, Var s2 -> String.compare s1 s2
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
  CCOrd.( String.compare v1 v2 <?> (Util.ord_option cmp, o1, o2) )

let equal t1 t2 = compare t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var s -> Hash.string_ s h
  | Const s -> Hash.string_ s h
  | AppBuiltin (s,l) -> Builtin.hash_fun s (Hash.list_ hash_fun l h)
  | App (s, l) -> Hash.list_ hash_fun l (hash_fun s h)
  | List l -> Hash.list_ hash_fun l (Hash.int_ 42 h)
  | Bind (s,v,t') ->
    h |> Binder.hash_fun s |> hash_fun t' |> Hash.list_ hash_ty_var v
  | Record (l, rest) ->
    h |> Hash.opt Hash.string_ rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l

and hash_ty_var (v,ty) h =
  Hash.string_ v h |> Hash.opt hash_fun ty

let hash x = Hash.apply hash_fun x

let make_ ?loc view = {term=view; loc;}

let var ?loc s = Var s
let builtin ?loc s = make_ ?loc (AppBuiltin (s,[]))
let int_ i = builtin (Builtin.Int i)
let of_int i = int_ (Z.of_int i)
let rat n = builtin (Builtin.Rat n)
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

module Set = Sequence.Set.Make(struct
  type t = term
  let compare = compare
end)
module Map = Sequence.Map.Make(struct
  type t = term
  let compare = compare
end)

module Tbl = Hashtbl.Make(struct
  type t = term
  let hash = hash
  let equal = equal
end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      match t.term with
      | Var _ | Int _ | Rat _ | Const _ -> ()
      | List l
      | AppBuiltin (_,l) -> List.iter iter l
      | App (f, l) -> iter f; List.iter iter l
      | Bind (_, v, t') -> List.iter iter v; iter t'
      | Record (l, rest) ->
          begin match rest with | None -> () | Some r -> iter r end;
          List.iter (fun (_,t') -> iter t') l
      | Column(x,y) -> k x; k y
    in iter t

  let vars t = subterms t |> Sequence.filter is_var

  let add_set s seq =
    Sequence.fold (fun set x -> Set.add x set) s seq

  let subterms_with_bound t k =
    let rec iter bound t =
      k (t, bound);
      match t.term with
      | Var _ | Int _ | Rat _ | Const _ -> ()
      | AppBuiltin (_, l)
      | List l -> List.iter (iter bound) l
      | App (f, l) -> iter bound f; List.iter (iter bound) l
      | Bind (_, v, t') ->
          (* add variables of [v] to the set *)
          let bound' = List.fold_left
            (fun set v -> add_set set (vars v))
            bound v
          in
          iter bound' t'
      | Record (l, rest) ->
          begin match rest with | None -> () | Some r -> iter bound r end;
          List.iter (fun (_,t') -> iter bound t') l
      | Column(x,y) -> k (x, bound); k (y, bound)
    in iter Set.empty t

  let free_vars t =
    subterms_with_bound t
      |> Sequence.fmap (fun (v,bound) ->
          if is_var v && not (Set.mem v bound)
          then Some v
          else None)

  let symbols t = subterms t
      |> Sequence.fmap (function
        | {term=Const s; _} -> Some s
        | _ -> None)
end

let ground t = Seq.vars t |> Sequence.is_empty

let close_all s t =
  let vars = Seq.free_vars t
    |> Seq.add_set Set.empty
    |> Set.elements
  in
  bind s vars t

let subterm ~strict t ~sub =
  (not strict && equal t sub)
  || Sequence.exists (equal sub) (Seq.subterms t)

let rec pp out t = match t.term with
  | Var s -> CCFormat.string out s
  | Int i -> CCFormat.string out (Z.to_string i)
  | Rat i -> CCFormat.string out (Q.to_string i)
  | Const s -> Sym.pp out s
  | List l ->
      Format.fprintf out "[@[<hv>%a@]]"
        (Util.pp_list ~sep:"," pp) l;
  | AppBuiltin (Builtin.Arrow, [ret;a]) ->
      Format.fprintf out "@[<2>%a@ -> %a@]" pp a pp ret
  | AppBuiltin (Builtin.Arrow, ret::l) ->
      Format.fprintf out "@[<2>(%a)@ -> %a@]" (Util.pp_list ~sep:" * " pp) l pp ret
  | AppBuiltin (s, l) ->
      Format.fprintf out "%a(@[<2>%a@])" Builtin.pp s (Util.pp_list ~sep:", " pp) l
  | App (s, l) ->
      Format.fprintf out "@[<2>%a(%a)@]"
        pp s (Util.pp_list ~sep:"," pp) l
  | Bind (s, vars, t') ->
      Format.fprintf out "@[<2>%a[@[%a@]]:@ %a@]"
        Binder.pp s
        (Util.pp_list ~sep:"," pp) vars
        pp t'
  | Record (l, None) ->
      Format.fprintf out "{@[<hv>%a@]}"
        (Util.pp_list (fun out (s,t') -> Format.fprintf out "%s:%a" s pp t')) l;
  | Record (l, Some r) ->
      Format.fprintf out "{@[<hv>%a@ | %a@]}"
        (Util.pp_list (fun out (s,t') -> Format.fprintf out "%s:%a" s pp t')) l
        pp r
  | Column(x,y) -> Format.fprintf out "@[%a:@,%a@]" pp x pp y

let to_string = CCFormat.to_string pp

(** {2 TPTP} *)

module TPTP = struct
  let true_ = builtin Builtin.true_
  let false_ = builtin Builtin.false_

  let var = var
  let bind = bind
  let const = const
  let app = app

  let and_ ?loc l = app_builtin ?loc Builtin.and_ l
  let or_ ?loc l = app_builtin ?loc Builtin.or_ l
  let not_ ?loc a = app_builtin ?loc Builtin.not_ [a]
  let equiv ?loc a b = app_builtin ?loc Builtin.equiv [a;b]
  let xor ?loc a b = app_builtin ?loc Builtin.xor [a;b]
  let imply ?loc a b = app_builtin ?loc Builtin.imply [a;b]
  let eq ?loc ?(ty=wildcard) a b = app_builtin ?loc Builtin.eq [ty;a;b]
  let neq ?loc ?(ty=wildcard) a b = app_builtin ?loc Builtin.neq [ty;a;b]
  let forall ?loc vars f = bind ?loc Binder.forall vars f
  let exists ?loc vars f = bind ?loc Binder.exists vars f
  let lambda ?loc vars f = bind ?loc Binder.lambda vars f

  let mk_fun_ty ?loc l ret = match l with
    | [] -> ret
    | _::_ -> app_builtin ?loc Builtin.arrow (ret :: l)
  let tType = builtin Builtin.tType
  let forall_ty ?loc vars t = bind ?loc Binder.forall_ty vars t

  let rec pp out t = match t.term with
    | Var s -> CCFormat.string out s
    | Int i -> CCFormat.string out (Z.to_string i)
    | Rat i -> CCFormat.string out (Q.to_string i)
    | Const s -> Sym.pp out s
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
    | AppBuiltin (s, l) ->
      Format.fprintf out "%a(%a)" Builtin.pp s (Util.pp_list ~sep:", " pp_surrounded) l
    | App (s, l) ->
      Format.fprintf out "@[<2>%a(%a)@]"
        pp s (Util.pp_list ~sep:"," pp) l
    | Bind (s, vars, t') ->
        Format.fprintf out "@[<2>%a[@[%a@]]:@ %a@]"
          Binder.TPTP.pp s
          (Util.pp_list ~sep:"," pp_typed_var) vars
          pp_surrounded t'
    | Record _ -> failwith "cannot print records in TPTP"
    | Column(x,y) ->
        begin match view y with
        | AppBuiltin (Builtin.Term, []) ->
          pp out x  (* do not print X:$i *)
        | _ ->
          pp out x;
          CCFormat.char out ':';
          pp out y
        end
  and pp_typed_var out t = match t.term with
    | Column ({term=Var s; _}, {term=AppBuiltin(Builtin.TType,[]); _})
    | Var s -> CCFormat.string out s
    | Column ({term=Var s; _}, {term=AppBuiltin (Builtin.Term,[]); _}) ->
        CCFormat.string out s
    | Column ({term=Var s; _}, ty) ->
      Format.fprintf out "%s:%a" s pp ty
    | _ -> assert false
  and pp_surrounded out t = match t.term with
    | AppBuiltin _
    | Bind _ -> CCFormat.char out '('; pp out t; CCFormat.char out ')'
    | _ -> pp out t

  let to_string = CCFormat.to_string pp
end
