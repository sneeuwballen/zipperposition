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

(** {1 Types} *)

exception Error of string
  (** Generic error on types. *)

module T = ScopedTerm

type symbol = Symbol.t

type t = T.t

type ty = t

type builtin = TType | Prop | Term

type view =
  | Builtin of builtin
  | Var of int              (** Type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t            (** Function type (left to right) *)
  | Record of (string*t) list * t option  (** Record type *)
  | Multiset of t
  | Forall of t             (** explicit quantification using De Bruijn index *)

let view t = match T.view t with
  | T.Var i -> Var i
  | T.BVar i -> BVar i
  | T.Bind (Binder.ForallTy, varty, t') ->
    assert (T.equal varty T.tType);
    Forall t'
  | T.Const s -> App (s, [])
  | T.AppBuiltin (Builtin.Arrow, [l;r]) -> Fun (l, r)
  | T.AppBuiltin (Builtin.Multiset, [t]) -> Multiset t
  | T.AppBuiltin (Builtin.Prop, []) -> Builtin Prop
  | T.AppBuiltin (Builtin.TType, []) -> Builtin TType
  | T.AppBuiltin (Builtin.Term, []) -> Builtin Term
  | T.Record (l, rest) -> Record (l, rest)
  | _ -> assert false

let hash_fun = T.hash_fun
let hash = T.hash
let equal = T.equal
let compare = T.compare

let is_var ty = match view ty with | Var _ -> true | _ -> false
let is_bvar ty = match view ty with | BVar _ -> true | _ -> false
let is_app ty = match view ty with App _ -> true | _ -> false
let is_fun ty = match view ty with | Fun _ -> true | _ -> false
let is_forall ty = match view ty with | Forall _ -> true | _ -> false

let tType = T.tType
let prop = T.builtin ~ty:tType Builtin.Prop
let term = T.builtin ~ty:tType Builtin.Term
let int = T.builtin ~ty:tType Builtin.TyInt
let rat = T.builtin ~ty:tType Builtin.TyRat

let var i =
  T.var ~ty:T.tType i

(* FIXME: ask for the arity instead? *)
let app s l = T.app ~ty:T.tType (T.const ~ty:T.tType s) l

let const s = T.const ~ty:T.tType s

let arrow l r =
  T.app_builtin ~ty:T.tType Builtin.arrow [l; r]

let rec arrow_list l r = match l with
  | [] -> r
  | [x] -> arrow x r
  | x::l' -> arrow x (arrow_list l' r)

let forall vars ty =
  T.bind_vars ~ty:T.tType Binder.forall_ty vars ty

let record l ~rest = T.record ~ty:T.tType l ~rest

let __bvar i =
  T.bvar ~ty:T.tType i

let __forall ty =
  T.bind ~ty:T.tType ~varty:T.tType Binder.forall_ty ty

let multiset ty = T.app_builtin ~ty:T.tType Builtin.multiset [ty]

let (<==) ret args = arrow_list args ret
let (<=.) ret a = arrow a ret
let (@@) = app

let of_term_unsafe t = t

(** {2 Containers} *)

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Seq = struct
  let vars ty = T.Seq.vars ty
  let sub ty = T.Seq.subterms ty
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
  let rec traverse i j ty = match view ty with
    | Fun (_,ty') ->
        traverse i (j+1) ty'
    | Forall ty' ->
        traverse (i+1) j ty'
    | Multiset _
    | Record _
    | Var _ | Builtin _ -> NoArity
    | BVar _
    | App _ -> Arity (i, j)
  in traverse 0 0 ty

let rec expected_args ty = match view ty with
  | Fun (a, ret) -> a :: expected_args ret
  | Forall ty' -> expected_args ty'
  | BVar _ | Var _ | Builtin _ | Record _ | Multiset _ | App _ -> []

let is_ground = T.ground

let size = T.size

let rec depth ty = match view ty with
  | Builtin _
  | Var _
  | BVar _ -> 1
  | App (_, l) -> 1 + depth_l l
  | Forall ty' -> 1 + depth ty'
  | Fun (t1,t2) -> 1 + max (depth t1) (depth t2)
  | Multiset t -> 1 + depth t
  | Record (r,rest) ->
      let d = CCOpt.maybe depth 0 rest in
      List.fold_left
        (fun d (_,ty) -> max d (depth ty))
        d r
and depth_l l = List.fold_left (fun d t -> max d (depth t)) 0 l

let rec open_fun ty = match view ty with
  | Fun (x, ret) ->
      let xs, ret' = open_fun ret in
      x::xs, ret'
  | _ -> [], ty

let _error msg = raise (Error msg)

let apply ty arg =
  match T.view ty with
  | T.AppBuiltin(Builtin.Arrow, [arg'; ret]) ->
    if equal arg arg'
    then ret
    else
      let msg = CCFormat.sprintf
          "Type.apply: wrong argument type, expected %a but got %a"
            T.pp arg' T.pp arg
      in _error msg
  | T.Bind (Binder.ForallTy, _, ty') ->
      T.DB.eval (DBEnv.singleton arg) ty'
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
  let i = term
  let o = prop
  let int = int
  let rat = rat
  let real = const Symbol.TPTP.real

  let rec pp_tstp_rec depth out t = match view t with
    | Builtin Prop -> CCFormat.string out "$o"
    | Builtin TType -> CCFormat.string out "$tType"
    | Builtin Term -> CCFormat.string out "$i"
    | Var i -> Format.fprintf out "T%d" i
    | BVar i -> Format.fprintf out "Tb%d" (depth-i-1)
    | App (p, []) -> Symbol.pp out p
    | App (p, args) -> Format.fprintf out "%a(%a)" Symbol.pp p
      (Util.pp_list (pp_tstp_rec depth)) args
    | Fun (arg, ret) ->
      (* FIXME: uncurry? *)
      Format.fprintf out "%a > %a" (pp_inner depth) arg (pp_tstp_rec depth) ret
    | Record _ -> failwith "cannot print record types in TPTP"
    | Multiset _ -> failwith "cannot print multiset types in TPTP"
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
  | Builtin Prop -> CCFormat.string out "prop"
  | Builtin TType -> CCFormat.string out "type"
  | Builtin Term -> CCFormat.string out "ι"
  | Var i -> Format.fprintf out "A%d" i
  | BVar i -> Format.fprintf out "T%i" (depth-i-1)
  | App (p, []) -> CCFormat.string out (Symbol.to_string p)
  | Multiset t ->
      Format.fprintf out "@[<2>multiset@ %a@]" (pp_rec depth) t
  | App (p, args) ->
    Format.fprintf out "@[<2>%s(%a)@]"
      (Symbol.to_string p) (Util.pp_list (pp_rec depth)) args
  | Fun (arg, ret) ->
    Format.fprintf out "@[%a →@ %a@]" (pp_inner depth) arg (pp_rec depth) ret
  | Record (l, None) ->
    CCFormat.char out '{';
    Util.pp_list (fun buf (n, t) -> Format.fprintf buf "@[%s: %a@]" n (pp_rec depth) t)
      out l;
    CCFormat.char out '}'
  | Record (l, Some r) ->
    Format.fprintf out "@[{%a | %a}@]"
      (Util.pp_list (fun buf (n, t) -> Format.fprintf buf "@[%s: %a@]" n (pp_rec depth) t))
      l (pp_rec depth) r
  | Forall ty' ->
    Format.fprintf out "@[Π T%i.@ %a@]" depth (pp_inner (depth+1)) ty'
and pp_inner depth out t = match view t with
  | Fun _ ->
    CCFormat.char out '('; pp_rec depth out t; CCFormat.char out ')'
  | _ -> pp_rec depth out t

let pp_depth ?hooks:_ depth out t = pp_rec depth out t

let pp buf t = pp_rec 0 buf t
let pp_surrounded buf t = (pp_inner 0) buf t

let to_string = CCFormat.to_string pp

(** {2 Misc} *)

let fresh_var = T.fresh_var ~ty:T.tType

(** {2 Conversions} *)

module Conv = struct
  module PT = STerm

  exception LocalExit

  type ctx = (string,int) Hashtbl.t

  let create() = Hashtbl.create 5
  let copy = Hashtbl.copy
  let clear = Hashtbl.clear

  let of_simple_term ~ctx t =
    let rec aux t = match t.PT.term with
      | PT.Column ({PT.term=PT.Var name; _},
                   {PT.term=PT.AppBuiltin (Builtin.TType, []); _})
      | PT.Var name ->
        assert (name <> "");
        begin try var (Hashtbl.find ctx name)
        with Not_found ->
          let n = Hashtbl.length ctx in
          Hashtbl.add ctx name n;
          var n
        end
      | PT.AppBuiltin (Builtin.Wildcard, []) ->
        fresh_var ()
      | PT.Int _
      | PT.Rat _ -> raise LocalExit
      | PT.Const s -> const s
      | PT.AppBuiltin (Builtin.Arrow, [ret;arg]) ->
        let ret = aux ret in
        let arg = aux arg in
        arrow arg ret
      | PT.AppBuiltin (Builtin.Arrow, ret::l) ->
        let ret = aux ret in
        let l = List.map aux l in
        arrow_list l ret
      | PT.App ({PT.term=PT.Const hd; _}, l) ->
        let l = List.map aux l in
        app hd l
      | PT.Bind (Binder.ForallTy, vars, t') ->
        let vars = List.map aux vars in
        let t' = aux t' in
        forall vars t'
      | PT.Record (l, rest) ->
        let rest = CCOpt.map aux rest in
        let l = List.map (fun (n,t) -> n, aux t) l in
        record l ~rest
      | PT.Bind _
      | PT.AppBuiltin _
      | PT.App _
      | PT.Column _
      | PT.List _ -> raise LocalExit
    in
    try Some (aux t)
    with LocalExit -> None

  let to_simple_term ?(curry=true) ?(depth=0) t =
    let rec aux depth t = match view t with
    | Builtin Prop -> PT.builtin Builtin.Prop
    | Builtin TType -> PT.builtin Builtin.TType
    | Builtin Term -> PT.builtin Builtin.Term
    | Var i -> PT.var (CCFormat.sprintf "A%d" i)
    | BVar i -> PT.var (CCFormat.sprintf "B%d" (depth-i-1))
    | App (s,l) -> PT.app (PT.const s) (List.map (aux depth) l)
    | Fun (arg, ret) when curry ->
      PT.TPTP.mk_fun_ty [aux depth arg] (aux depth ret)
    | Fun _ ->
      let args, ret = open_fun t in
      let args = List.map (aux depth) args in
      let ret = aux depth ret in
      PT.TPTP.mk_fun_ty args ret
    | Record (l, rest) ->
      let rest = CCOpt.map (aux depth) rest in
      PT.record (List.map (fun (n,ty) -> n, aux depth ty) l) ~rest
    | Multiset t -> PT.app_builtin Builtin.multiset [aux depth t]
    | Forall t' ->
      PT.bind Binder.forall_ty
        [PT.var (CCFormat.sprintf "B%d" depth)]
        (aux (depth+1) t')
    in
    aux depth t
end
