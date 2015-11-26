
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

(** {1 Prolog-like Terms}. *)

module Hash = CCHash
module Sym = LogtkSymbol

type location = LogtkParseLocation.t

type t = {
  term : view;
  loc : location option;
}
and view =
  | Var of string                   (** variable *)
  | Int of Z.t                      (** integer *)
  | Rat of Q.t                      (** rational *)
  | Const of Sym.t               (** constant *)
  | Syntactic of Sym.t * t list  (** syntactic construct (operator...) *)
  | App of t * t list               (** apply term *)
  | Bind of Sym.t * t list * t   (** bind n variables *)
  | List of t list                  (** special constructor for lists *)
  | Record of (string * t) list * t option  (** extensible record *)
  | Column of t * t                 (** t:t (useful for typing, e.g.) *)

type term = t

let view t = t.term
let loc t = t.loc

let __to_int = function
  | Var _ -> 0
  | Int _ -> 1
  | Rat _ -> 2
  | Const _ -> 3
  | Syntactic _ -> 4
  | App _ -> 5
  | Bind _ -> 6
  | List _ -> 7
  | Record _ -> 8
  | Column _ -> 9

let rec cmp t1 t2 = match t1.term, t2.term with
  | Var s1, Var s2 -> String.compare s1 s2
  | Int i1, Int i2 -> Z.compare i1 i2
  | Rat n1, Rat n2 -> Q.compare n1 n2
  | Const s1, Const s2 -> Sym.cmp s1 s2
  | App (s1,l1), App (s2, l2) ->
    let c = cmp s1 s2 in
    if c = 0
    then CCOrd.list_ cmp l1 l2
    else c
  | Syntactic (s1,l1), Syntactic (s2,l2) ->
    let c = Sym.cmp s1 s2 in
    if c = 0
    then CCOrd.list_ cmp l1 l2
    else c
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
    let c = Sym.cmp s1 s2 in
    if c = 0
    then
      let c' = cmp t1 t2 in
      if c' = 0
      then CCOrd.list_ cmp v1 v2
      else c'
    else c
  | Column (x1,y1), Column (x2,y2) ->
    let c = cmp x1 x2 in
    if c = 0 then cmp y1 y2 else c
  | _ -> __to_int t1.term - __to_int t2.term

let eq t1 t2 = cmp t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var s -> Hash.string_ s h
  | Int i -> Hash.int_ (Z.hash i) h
  | Rat n -> Hash.string_ (Q.to_string n) h  (* TODO: find better *)
  | Const s -> Sym.hash_fun s h
  | Syntactic (s,l) -> Sym.hash_fun s (Hash.list_ hash_fun l h)
  | App (s, l) -> Hash.list_ hash_fun l (hash_fun s h)
  | List l -> Hash.list_ hash_fun l (Hash.int_ 42 h)
  | Bind (s,v,t') ->
    h |> Sym.hash_fun s |> hash_fun t' |> Hash.list_ hash_fun v
  | Record (l, rest) ->
    h |> Hash.opt hash_fun rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l
  | Column (x,y) -> hash_fun x (hash_fun y h)

let hash x = Hash.apply hash_fun x

let __make ?loc view = {term=view; loc;}

let var ?loc ?ty s = match ty with
  | None -> __make ?loc (Var s)
  | Some ty -> __make ?loc (Column (__make (Var s), ty))
let int_ i = __make (Int i)
let of_int i = __make (Int (Z.of_int i))
let rat n = __make (Rat n)
let syntactic ?loc s l = __make ?loc (Syntactic(s,l))
let app ?loc s l = match l with
  | [] -> s
  | _::_ -> __make ?loc (App(s,l))
let const ?loc s = __make ?loc (Const s)
let bind ?loc s v l = match v with
  | [] -> l
  | _::_ -> __make ?loc (Bind(s,v,l))
let list_ ?loc l = __make ?loc (List l)
let nil = list_ []
let record ?loc l ~rest =
  let l = List.sort (fun (n1,_)(n2,_) -> String.compare n1 n2) l in
  __make ?loc (Record (l, rest))
let column ?loc x y = __make ?loc (Column(x,y))
let at_loc ~loc t = {t with loc=Some loc; }

let wildcard = const Sym.Base.wildcard

let is_app = function {term=App _; _} -> true | _ -> false
let is_var = function | {term=Var _; _} -> true | _ -> false

module Set = Sequence.Set.Make(struct
  type t = term
  let compare = cmp
end)
module Map = Sequence.Map.Make(struct
  type t = term
  let compare = cmp
end)

module Tbl = Hashtbl.Make(struct
  type t = term
  let hash = hash
  let equal = eq
end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      match t.term with
      | Var _ | Int _ | Rat _ | Const _ -> ()
      | List l
      | Syntactic (_,l) -> List.iter iter l
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
      | Syntactic (_, l)
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
        | {term=Bind (s, _, _); _} -> Some s
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
  (not strict && eq t sub)
  || Sequence.exists (eq sub) (Seq.subterms t)

let rec pp out t = match t.term with
  | Var s -> CCFormat.string out s
  | Int i -> CCFormat.string out (Z.to_string i)
  | Rat i -> CCFormat.string out (Q.to_string i)
  | Const s -> Sym.pp out s
  | List l ->
      CCFormat.char out '[';
      CCFormat.list ~sep:"," pp out l;
      CCFormat.char out ']'
  | Syntactic (Sym.Conn Sym.Arrow, [ret;a]) ->
    Format.fprintf out "%a -> %a" pp a pp ret
  | Syntactic (Sym.Conn Sym.Arrow, ret::l) ->
    Format.fprintf out "(%a) -> %a" (CCFormat.list ~sep:" * " pp) l pp ret
  | Syntactic (s, l) ->
      Format.fprintf out "%a(%a)" Sym.pp s (CCFormat.list ~sep:", " pp) l
  | App (s, l) ->
      pp out s;
      CCFormat.char out '(';
      CCFormat.list ~sep:"," pp out l;
      CCFormat.char out ')'
  | Bind (s, vars, t') ->
      Sym.pp out s;
      CCFormat.char out '[';
      CCFormat.list ~sep:"," pp out vars;
      CCFormat.string out "]:";
      pp out t'
  | Record (l, None) ->
    CCFormat.char out '{';
    CCFormat.list (fun buf (s,t') -> Format.fprintf buf "%s:%a" s pp t') out l;
    CCFormat.char out '}'
  | Record (l, Some r) ->
    CCFormat.char out '{';
    CCFormat.list (fun buf (s,t') -> Format.fprintf buf "%s:%a" s pp t') out l;
    Format.fprintf out " | %a}" pp r
  | Column(x,y) ->
      pp out x;
      CCFormat.char out ':';
      pp out y

let to_string = CCFormat.to_string pp

(** {2 Visitor} *)

class virtual ['a] visitor = object (self)
  method virtual var : ?loc:location -> string -> 'a
  method virtual int_ : ?loc:location -> Z.t -> 'a
  method virtual rat_ : ?loc:location -> Q.t -> 'a
  method virtual const : ?loc:location -> Sym.t -> 'a
  method virtual syntactic : ?loc:location -> Sym.t -> 'a list -> 'a
  method virtual app : ?loc:location -> 'a -> 'a list -> 'a
  method virtual bind : ?loc:location -> Sym.t -> 'a list -> 'a -> 'a
  method virtual list_ : ?loc:location -> 'a list -> 'a
  method virtual record : ?loc:location -> (string*'a) list -> 'a option -> 'a
  method virtual column : ?loc:location -> 'a -> 'a -> 'a
  method visit t =
    let loc = t.loc in
    match t.term with
    | Var s -> self#var ?loc s
    | Int n -> self#int_ ?loc n
    | Rat n -> self#rat_ ?loc n
    | Const s -> self#const ?loc s
    | Syntactic(s,l) -> self#syntactic ?loc s (List.map self#visit l)
    | App(f,l) -> self#app ?loc (self#visit f) (List.map self#visit l)
    | Bind (s, vars,t') ->
        self#bind ?loc s (List.map self#visit vars) (self#visit t')
    | List l -> self#list_ ?loc (List.map self#visit l)
    | Record (l, rest) ->
        let rest = CCOpt.map self#visit rest in
        let l = List.map (fun (n,t) -> n, self#visit t) l in
        self#record ?loc l rest
    | Column (a,b) -> self#column ?loc (self#visit a) (self#visit b)
end

class id_visitor = object
  inherit [t] visitor
  method var ?loc s = var ?loc s
  method int_ ?loc:_ i = int_ i
  method rat_ ?loc:_ n = rat n
  method const ?loc s = const ?loc s
  method syntactic ?loc s l = syntactic ?loc s l
  method app ?loc f l = app ?loc f l
  method bind ?loc s vars t = bind ?loc s vars t
  method list_ ?loc l = list_ ?loc l
  method record ?loc l rest = record ?loc l ~rest
  method column ?loc a b = column ?loc a b
end (** Visitor that maps the subterms into themselves *)

(** {2 TPTP} *)

module TPTP = struct
  let true_ = const Sym.Base.true_
  let false_ = const Sym.Base.false_

  let var = var
  let bind = bind
  let const = const
  let app = app

  let and_ ?loc l = syntactic ?loc Sym.Base.and_ l
  let or_ ?loc l = syntactic ?loc Sym.Base.or_ l
  let not_ ?loc a = syntactic ?loc Sym.Base.not_ [a]
  let equiv ?loc a b = syntactic ?loc Sym.Base.equiv [a;b]
  let xor ?loc a b = syntactic ?loc Sym.Base.xor [a;b]
  let imply ?loc a b = syntactic ?loc Sym.Base.imply [a;b]
  let eq ?loc ?(ty=wildcard) a b = syntactic ?loc Sym.Base.eq [ty;a;b]
  let neq ?loc ?(ty=wildcard) a b = syntactic ?loc Sym.Base.neq [ty;a;b]
  let forall ?loc vars f = bind ?loc Sym.Base.forall vars f
  let exists ?loc vars f = bind ?loc Sym.Base.exists vars f
  let lambda ?loc vars f = bind ?loc Sym.Base.lambda vars f

  let mk_fun_ty ?loc l ret = match l with
    | [] -> ret
    | _::_ -> syntactic ?loc Sym.Base.arrow (ret :: l)
  let tType = const Sym.Base.tType
  let forall_ty ?loc vars t = bind ?loc Sym.Base.forall_ty vars t

  let rec pp out t = match t.term with
    | Var s -> CCFormat.string out s
    | Int i -> CCFormat.string out (Z.to_string i)
    | Rat i -> CCFormat.string out (Q.to_string i)
    | Const s -> Sym.TPTP.pp out s
    | List l ->
        CCFormat.char out '[';
        CCFormat.list ~sep:"," pp out l;
        CCFormat.char out ']'
    | Syntactic (Sym.Conn Sym.And, l) ->
      CCFormat.list ~sep:" & " pp_surrounded out l
    | Syntactic (Sym.Conn Sym.Or, l) ->
      CCFormat.list ~sep:" | " pp_surrounded out l
    | Syntactic (Sym.Conn Sym.Not, [a]) ->
      Format.fprintf out "~%a" pp_surrounded a
    | Syntactic (Sym.Conn Sym.Imply, [a;b]) ->
      Format.fprintf out "%a => %a" pp_surrounded a pp_surrounded b
    | Syntactic (Sym.Conn Sym.Xor, [a;b]) ->
      Format.fprintf out "%a <~> %a" pp_surrounded a pp_surrounded b
    | Syntactic (Sym.Conn Sym.Equiv, [a;b]) ->
      Format.fprintf out "%a <=> %a" pp_surrounded a pp_surrounded b
    | Syntactic (Sym.Conn Sym.Eq, [_;a;b]) ->
      Format.fprintf out "%a = %a" pp_surrounded a pp_surrounded b
    | Syntactic (Sym.Conn Sym.Neq, [_;a;b]) ->
      Format.fprintf out "%a != %a" pp_surrounded a pp_surrounded b
    | Syntactic (Sym.Conn Sym.Arrow, [ret;a]) ->
      Format.fprintf out "%a > %a" pp a pp ret
    | Syntactic (Sym.Conn Sym.Arrow, ret::l) ->
      Format.fprintf out "(%a) > %a" (CCFormat.list~sep:" * " pp) l pp_surrounded ret
    | Syntactic (s, l) ->
      Format.fprintf out "%a(%a)" Sym.pp s (CCFormat.list ~sep:", " pp_surrounded) l
    | App (s, l) ->
        pp out s;
        CCFormat.char out '(';
        CCFormat.list ~sep:"," pp out l;
        CCFormat.char out ')'
    | Bind (s, vars, t') ->
        Sym.TPTP.pp out s;
        CCFormat.char out '[';
        CCFormat.list ~sep:"," pp_typed_var out vars;
        CCFormat.string out "]:";
        pp_surrounded out t'
    | Record _ -> failwith "cannot print records in TPTP"
    | Column(x,y) ->
        begin match view y with
        | Const s when Sym.eq s Sym.TPTP.i ->
          pp out x  (* do not print X:$i *)
        | _ ->
          pp out x;
          CCFormat.char out ':';
          pp out y
        end
  and pp_typed_var out t = match t.term with
    | Column ({term=Var s; _}, {term=Const (Sym.Conn Sym.TType); _})
    | Var s -> CCFormat.string out s
    | Column ({term=Var s; _}, {term=Const sy; _}) when Sym.eq sy Sym.TPTP.i ->
        CCFormat.string out s
    | Column ({term=Var s; _}, ty) ->
      Format.fprintf out "%s:%a" s pp ty
    | _ -> assert false
  and pp_surrounded out t = match t.term with
    | App ({term=Const (Sym.Conn _); _}, _::_::_)
    | Bind _ -> CCFormat.char out '('; pp out t; CCFormat.char out ')'
    | _ -> pp out t

  let to_string = CCFormat.to_string pp
end
