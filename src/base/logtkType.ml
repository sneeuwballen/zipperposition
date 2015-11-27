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

(** {1 LogtkTypes} *)

exception Error of string
  (** Generic error on types. *)

module T = LogtkScopedTerm

type symbol = LogtkSymbol.t

type t = T.t

type ty = t

let kind = T.Kind.Type

type view =
  | Var of int              (** Type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t            (** Function type (left to right) *)
  | Record of (string*t) list * t option  (** Record type *)
  | Forall of t             (** explicit quantification using De Bruijn index *)

let view t = match T.kind t with
  | T.Kind.Type ->
    begin match T.view t with
    | T.Var i -> Var i
    | T.BVar i -> BVar i
    | T.Bind (LogtkSymbol.Conn LogtkSymbol.ForallTy, varty, t') ->
      assert (T.equal varty T.tType);
      Forall t'
    | T.Const s -> App (s, [])
    | T.SimpleApp (LogtkSymbol.Conn LogtkSymbol.Arrow, [l;r]) -> Fun (l, r)
    | T.SimpleApp (s, l) -> App (s, l)
    | T.Record (l, rest) -> Record (l, rest)
    | _ -> failwith "Type.view"
    end
  | _ -> raise (Invalid_argument "Type.view")

let hash_fun = T.hash_fun
let hash = T.hash
let equal = T.equal
let compare = T.compare

let is_var ty = match view ty with | Var _ -> true | _ -> false
let is_bvar ty = match view ty with | BVar _ -> true | _ -> false
let is_app ty = match view ty with App _ -> true | _ -> false
let is_fun ty = match view ty with | Fun _ -> true | _ -> false
let is_forall ty = match view ty with | Forall _ -> true | _ -> false

let var i =
  T.var ~kind ~ty:T.tType i

let app s l = T.simple_app ~kind ~ty:T.tType s l

let const s = T.const ~kind ~ty:T.tType s

let arrow l r =
  T.simple_app ~kind ~ty:T.tType LogtkSymbol.Base.arrow [l; r]

let rec arrow_list l r = match l with
  | [] -> r
  | [x] -> arrow x r
  | x::l' -> arrow x (arrow_list l' r)

let forall vars ty =
  T.bind_vars ~kind ~ty:T.tType LogtkSymbol.Base.forall_ty vars ty

let record l ~rest = T.record ~kind ~ty:T.tType l ~rest

let __bvar i =
  T.bvar ~kind ~ty:T.tType i

let __forall ty =
  T.bind ~kind ~ty:T.tType ~varty:T.tType LogtkSymbol.Base.forall_ty ty

let multiset ty = app LogtkSymbol.Base.multiset [ty]

let (<==) ret args = arrow_list args ret
let (<=.) ret a = arrow a ret
let (@@) = app

(* downcast *)
let of_term_exn ty = match T.kind ty with
  | T.Kind.Type -> ty
  | _ -> raise (Invalid_argument "Type.of_term_exn")

let of_term ty = try Some (of_term_exn ty) with Invalid_argument _ -> None

let is_type t = try ignore (of_term_exn t); true with Invalid_argument _ -> false

(** {2 Containers} *)

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Seq = struct
  let vars ty = Sequence.fmap of_term (T.Seq.vars ty)
  let sub ty = Sequence.fmap of_term (T.Seq.subterms ty)
  let add_set = T.Seq.add_set
  let max_var = T.Seq.max_var
end

let vars_set set t =
  Seq.add_set set (Seq.vars t)

let vars t = Set.elements (vars_set Set.empty t)

let close_forall ty = forall (vars ty) ty

type arity_result =
  | Arity of int * int
  | NoArity

let arity ty =
  let rec traverse i j ty = match T.view ty with
    | T.SimpleApp (LogtkSymbol.Conn LogtkSymbol.Arrow, [_; ty']) ->
        traverse i (j+1) ty'
    | T.Bind (LogtkSymbol.Conn LogtkSymbol.ForallTy, _, ty') ->
        traverse (i+1) j ty'
    | T.Var _
    | T.RigidVar _ -> NoArity
    | T.Record _
    | T.BVar _
    | T.Const _
    | T.SimpleApp _
    | T.App _ -> Arity (i, j)
    | T.RecordGet _
    | T.RecordSet _
    | T.Multiset _
    | T.Bind _
    | T.At _ -> assert false
  in traverse 0 0 ty

let rec expected_args ty = match view ty with
  | Fun (a, ret) -> a :: expected_args ret
  | BVar _
  | Var _
  | Record _
  | App _ -> []
  | Forall ty' -> expected_args ty'

let is_ground = T.ground

let size = T.size

let rec depth ty = match view ty with
  | Var _
  | BVar _ -> 1
  | App (_, l) -> 1 + List.fold_left (fun d t -> max d (depth t)) 0 l
  | Forall ty' -> 1 + depth ty'
  | Fun (t1,t2) -> 1 + max (depth t1) (depth t2)
  | Record (r,rest) ->
      let d = CCOpt.maybe depth 0 rest in
      List.fold_left
        (fun d (_,ty) -> max d (depth ty))
        d r

let rec open_fun ty = match view ty with
  | Fun (x, ret) ->
      let xs, ret' = open_fun ret in
      x::xs, ret'
  | _ -> [], ty

let _error msg = raise (Error msg)

let apply ty arg =
  match T.view ty with
  | T.SimpleApp(LogtkSymbol.Conn LogtkSymbol.Arrow, [arg'; ret]) ->
    if equal arg arg'
    then ret
    else
      let msg = CCFormat.sprintf
          "Type.apply: wrong argument type, expected %a but got %a"
            T.pp arg' T.pp arg
      in _error msg
  | T.Bind (LogtkSymbol.Conn LogtkSymbol.ForallTy, _, ty') ->
      T.DB.eval (LogtkDBEnv.singleton arg) ty'
  | _ ->
      let msg = CCFormat.sprintf
        "Type.apply: expected quantified or function type, but got %a"
        T.pp ty
      in _error msg

(* apply a type to arguments. *)
let rec apply_list ty args = match args with
  | [] -> ty
  | arg::args' ->
      let ty' = apply ty arg in
      apply_list ty' args'

(*
let bij = T.bij
*)

let __var =
  let r = ref ~-1 in
  fun () ->
    let n = !r in
    decr r;
    T.var ~ty:T.tType n

type print_hook = int -> (CCFormat.t -> t-> unit) -> CCFormat.t -> t-> bool

module TPTP = struct
  let i = const LogtkSymbol.TPTP.i
  let o = const LogtkSymbol.TPTP.o
  let int = const LogtkSymbol.TPTP.int
  let rat = const LogtkSymbol.TPTP.rat
  let real = const LogtkSymbol.TPTP.real

  let rec pp_tstp_rec depth out t = match view t with
    | Var i -> Format.fprintf out "T%d" i
    | BVar i -> Format.fprintf out "Tb%d" (depth-i-1)
    | App (p, []) -> LogtkSymbol.TPTP.pp out p
    | App (p, args) -> Format.fprintf out "%a(%a)" LogtkSymbol.TPTP.pp p
      (CCFormat.list (pp_tstp_rec depth)) args
    | Fun (arg, ret) ->
      (* FIXME: uncurry? *)
      Format.fprintf out "%a > %a" (pp_inner depth) arg (pp_tstp_rec depth) ret
    | Record _ -> failwith "cannot print record types in TPTP"
    | Forall ty' ->
      Format.fprintf out "!>[Tb%d:$tType]: %a" depth (pp_inner (depth+1)) ty'
  and pp_inner depth out t = match view t with
    | Fun _ ->
      CCFormat.char out '('; pp_tstp_rec depth out t; CCFormat.char out ')'
    | _ -> pp_tstp_rec depth out t

  let pp out t = pp_tstp_rec 0 out t

  let pp_depth ?hooks:_ depth out t = pp_tstp_rec depth out t

  let to_string = CCFormat.to_string pp
end

(** {2 IO} *)

let rec pp_rec depth out t = match view t with
  | Var i -> Format.fprintf out "A%d" i
  | BVar i -> Format.fprintf out "T%i" (depth-i-1)
  | App (p, []) -> CCFormat.string out (LogtkSymbol.to_string p)
  | App (LogtkSymbol.Conn LogtkSymbol.Multiset, l) ->
      Format.fprintf out "[%a]" (CCFormat.list (pp_rec depth)) l
  | App (p, args) ->
    Format.fprintf out "%s(%a)"
      (LogtkSymbol.to_string p) (CCFormat.list (pp_rec depth)) args
  | Fun (arg, ret) ->
    Format.fprintf out "%a → %a" (pp_inner depth) arg (pp_rec depth) ret
  | Record (l, None) ->
    CCFormat.char out '{';
    CCFormat.list (fun buf (n, t) -> Format.fprintf buf "%s: %a" n (pp_rec depth) t)
      out l;
    CCFormat.char out '}'
  | Record (l, Some r) ->
    CCFormat.char out '{';
    CCFormat.list (fun buf (n, t) -> Format.fprintf buf "%s: %a" n (pp_rec depth) t)
      out l;
    Format.fprintf out "| %a}" (pp_rec depth) r
  | Forall ty' ->
    Format.fprintf out "Λ T%i. %a" depth (pp_inner (depth+1)) ty'
and pp_inner depth out t = match view t with
  | Fun _ ->
    CCFormat.char out '('; pp_rec depth out t; CCFormat.char out ')'
  | _ -> pp_rec depth out t

let pp_depth ?hooks:_ depth out t = pp_rec depth out t

let pp buf t = pp_rec 0 buf t
let pp_surrounded buf t = (pp_inner 0) buf t

let to_string = CCFormat.to_string pp

(** {2 Misc} *)

let fresh_var = T.fresh_var ~kind ~ty:T.tType

(** {2 Conversions} *)

module Conv = struct
  module PT = LogtkPrologTerm

  exception LocalExit

  type ctx = (string,int) Hashtbl.t

  let create() = Hashtbl.create 5
  let copy = Hashtbl.copy
  let clear = Hashtbl.clear

  let of_prolog ~ctx t =
    let rec of_prolog t = match t.PT.term with
      | PT.Column ({PT.term=PT.Var name; _},
                   {PT.term=PT.Const (LogtkSymbol.Conn LogtkSymbol.TType); _})
      | PT.Var name ->
        assert (name <> "");
        begin try var (Hashtbl.find ctx name)
        with Not_found ->
          let n = Hashtbl.length ctx in
          Hashtbl.add ctx name n;
          var n
        end
      | PT.Const (LogtkSymbol.Conn LogtkSymbol.Wildcard) ->
        fresh_var ()
      | PT.Int _
      | PT.Rat _ -> raise LocalExit
      | PT.Const s -> const s
      | PT.Syntactic (LogtkSymbol.Conn LogtkSymbol.Arrow, [ret;arg]) ->
        let ret = of_prolog ret in
        let arg = of_prolog arg in
        arrow arg ret
      | PT.Syntactic (LogtkSymbol.Conn LogtkSymbol.Arrow, ret::l) ->
        let ret = of_prolog ret in
        let l = List.map of_prolog l in
        arrow_list l ret
      | PT.App ({PT.term=PT.Const hd; _}, l) ->
        let l = List.map of_prolog l in
        app hd l
      | PT.Bind (LogtkSymbol.Conn LogtkSymbol.ForallTy, vars, t') ->
        let vars = List.map of_prolog vars in
        let t' = of_prolog t' in
        forall vars t'
      | PT.Record (l, rest) ->
        let rest = CCOpt.map of_prolog rest in
        let l = List.map (fun (n,t) -> n, of_prolog t) l in
        record l ~rest
      | PT.Bind _
      | PT.Syntactic _
      | PT.App _
      | PT.Column _
      | PT.List _ -> raise LocalExit
    in
    try Some (of_prolog t)
    with LocalExit -> None

  let to_prolog ?(curry=true) ?(depth=0) t =
    let rec to_prolog depth t = match view t with
    | Var i -> PT.var (CCFormat.sprintf "A%d" i)
    | BVar i -> PT.var (CCFormat.sprintf "B%d" (depth-i-1))
    | App (s,l) -> PT.app (PT.const s) (List.map (to_prolog depth) l)
    | Fun (arg, ret) when curry ->
      PT.TPTP.mk_fun_ty [to_prolog depth arg] (to_prolog depth ret)
    | Fun _ ->
      let args, ret = open_fun t in
      let args = List.map (to_prolog depth) args in
      let ret = to_prolog depth ret in
      PT.TPTP.mk_fun_ty args ret
    | Record (l, rest) ->
      let rest = CCOpt.map (to_prolog depth) rest in
      PT.record (List.map (fun (n,ty) -> n, to_prolog depth ty) l) ~rest
    | Forall t' ->
      PT.bind LogtkSymbol.Base.forall_ty
        [PT.var (CCFormat.sprintf "B%d" depth)]
        (to_prolog (depth+1) t')
    in
    to_prolog depth t
end
