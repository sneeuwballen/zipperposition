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

let kind = T.Kind.Type

type view =
  | Var of int              (** Type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t             (** explicit quantification using De Bruijn index *)

let view t = match T.kind t with
  | T.Kind.Type ->
    begin match T.view t with
    | T.Var i -> Var i
    | T.BVar i -> BVar i
    | T.Bind (Symbol.Conn Symbol.ForallTy, t') -> Forall t'
    | T.App (head, ((t'::l') as l)) ->
      begin match T.view head with
      | T.Const (Symbol.Conn Symbol.Arrow) -> Fun (t', l')
      | T.Const s -> App (s, l)
      | _ -> failwith "Type.view"
      end
    | _ -> failwith "Type.view"
    end
  | _ -> raise (Invalid_argument "Type.view")

let hash = T.hash
let eq = T.eq
let cmp = T.cmp

let is_var ty = match view ty with | Var _ -> true | _ -> false
let is_bvar ty = match view ty with | BVar _ -> true | _ -> false
let is_app ty = match view ty with App _ -> true | _ -> false
let is_fun ty = match view ty with | Fun _ -> true | _ -> false
let is_forall ty = match view ty with | Forall _ -> true | _ -> false

let tType = T.tType

let var i =
  if i < 0 then raise (Invalid_argument "Type.var");
  T.var ~ty:tType i

let app s l = T.app ~kind ~ty:tType (T.const ~kind ~ty:tType s) l

let const s = T.const ~kind ~ty:tType s

let rec mk_fun ret args =
  match args with
  | [] -> ret
  | _::_ ->
    match view ret with
    | Fun (ret', args') ->
      (* invariant: flatten function types. Symmetric w.r.t the {!HOTerm.At}
          constructor invariant. [args] must be applied before [args']
          need to be supplied.
          Example: [(a <- b) <- c] requires [c] first *)
      mk_fun ret' (args @ args')
    | _ ->
      T.app ~kind ~ty:tType (T.const ~kind ~ty:tType Symbol.Base.arrow) (ret :: args)

let forall vars ty =
  T.bind_vars ~ty:tType Symbol.Base.forall_ty vars ty

let __forall ty = T.bind Symbol.Base.forall_ty ty

let (<==) = mk_fun
let (<=.) ret a = mk_fun ret [a]
let (@@) = app

(* downcast *)
let of_term ty = match T.kind ty with
  | T.Kind.Type -> Some ty
  | _ -> None

let of_term_exn ty = match T.kind ty with
  | T.Kind.Type -> ty
  | _ -> raise (Invalid_argument "Type.of_term_exn")

let is_type t = match T.kind t with
  | T.Kind.Type -> true
  | _ -> false

(** {2 Containers} *)

module Set = Sequence.Set.Make(struct
  type t = ty
  let compare = cmp
end)
module Map = Sequence.Map.Make(struct
  type t = ty
  let compare = cmp
end)
module Tbl = Hashtbl.Make(struct
  type t = ty
  let hash = hash
  let equal = eq
end)

module Seq = struct
  let vars ty = Sequence.fmap of_term (T.Seq.vars ty)
  let sub ty = Sequence.fmap of_term (T.Seq.subterms ty)
  let add_set set t = Sequence.fold (fun set x -> Set.add x set) set t
end

let vars_set set t =
  Seq.add_set set (Seq.vars t)

let vars t = Set.elements (vars_set Set.empty t)

let close_forall ty = forall (vars ty) ty

let rec arity ty = match view ty with
  | Fun (_, l) -> 0, List.length l
  | Var _
  | BVar _
  | App _ -> 0, 0
  | Forall ty' ->
    let i1, i2 = arity ty' in
    i1 + 1, i2

let rec expected_args ty = match view ty with
  | Fun (_, l) -> l
  | BVar _
  | Var _
  | App _ -> []
  | Forall ty' -> expected_args ty'

let is_ground = T.ground

let size = T.size

let _error msg = raise (Error msg)

(* apply a type to arguments. *)
let apply ty args =
  (* apply ty to args *)
  let rec apply ty args =
    match view ty, args with
    | _, [] -> ty
    | Fun (ret, l), l' -> apply_fun ret l l'
    | Forall ty', a::args' ->
      let ty' = T.DB.eval (DBEnv.singleton a) ty' in
      apply ty' args'
    | _, _ -> _error "Type.apply: expected function or forall type"
  (* recursive matching of expected arguments and provided arguments.
    careful: we could have a curried function *)
  and apply_fun f_ret f_args args =
    match f_args, args with
    | x::f_args', y::args' ->
      (* match arguments after substitution *)
      if eq x y
        then apply_fun f_ret f_args' args'
        else
          let msg = Util.sprintf
          "Type.apply: wrong argument type, expected %a but got %a" T.pp x T.pp y
          in _error msg
    | [], [] ->
      (* total application, result is return type. special case of last case. *)
      f_ret
    | [], _ -> apply f_ret args
    | _::_, [] ->
      (* partial application. The remaining arguments will have to be
          provided by another call to {!apply}. *)
      mk_fun f_ret f_args
  in
  apply ty args

(*
let bij = T.bij
*)

let __var =
  let r = ref ~-1 in
  fun () ->
    let n = !r in
    decr r;
    T.var ~ty:tType n

(* apply a type to arguments. *)
let apply ty args =
  (* apply ty to args *)
  let rec apply ty args =
    match view ty, args with
    | _, [] -> ty
    | Fun (ret, l), l' -> apply_fun ret l l'
    | Forall ty', a::args' ->
      let ty' = T.DB.eval (DBEnv.singleton a) ty' in
      apply ty' args'
    | _, _ -> _error "Type.apply: expected function or forall type"
  (* recursive matching of expected arguments and provided arguments.
    careful: we could have a curried function *)
  and apply_fun f_ret f_args args =
    match f_args, args with
    | x::f_args', y::args' ->
      (* match arguments after substitution *)
      if eq x y
        then apply_fun f_ret f_args' args'
        else
          let msg = Util.sprintf
            "Type.apply: argument type mismatch, expected %a but got %a"
              T.pp (x:>T.t) T.pp (y:>T.t)
          in _error msg
    | [], [] ->
      (* total application, result is return type. special case of last case. *)
      f_ret
    | [], _ -> apply f_ret args
    | _::_, [] ->
      (* partial application. The remaining arguments will have to be
          provided by another call to {!apply}. *)
      mk_fun f_ret f_args
  in
  apply ty args

module TPTP = struct
  let arrow = Symbol.of_string ">"
  let forall_ty = Symbol.of_string "!>"

  let i = Symbol.of_string "$i"
  let o = Symbol.of_string "$o"
  let int = Symbol.of_string "$int"
  let rat = Symbol.of_string "$rat"
  let real = Symbol.of_string "$real"

  let rec pp_tstp_rec depth buf t = match view t with
    | Var i -> Printf.bprintf buf "T%d" i
    | BVar i -> Printf.bprintf buf "Tb%d" (depth-i-1)
    | App (p, []) -> Symbol.pp buf p
    | App (p, args) -> Printf.bprintf buf "%a(%a)" Symbol.pp p
      (Util.pp_list (pp_tstp_rec depth)) args
    | Fun (ret, []) -> assert false
    | Fun (ret, [arg]) ->
      Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
    | Fun (ret, l) ->
      Printf.bprintf buf "(%a) > %a"
        (Util.pp_list ~sep:" * " (pp_inner depth)) l (pp_tstp_rec depth) ret
    | Forall ty' ->
      Printf.bprintf buf "!>[Tb%d:$tType]: %a" depth (pp_inner (depth+1)) ty'
  and pp_inner depth buf t = match view t with
    | Fun (_, _::_) ->
      Buffer.add_char buf '('; pp_tstp_rec depth buf t; Buffer.add_char buf ')'
    | _ -> pp_tstp_rec depth buf t

  let pp buf t = pp_tstp_rec 0 buf t

  let to_string t =
    let b = Buffer.create 15 in
    pp b t;
    Buffer.contents b

  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

(** {2 IO} *)

let rec pp_rec depth buf t = match view t with
  | Var i -> Printf.bprintf buf "T%d" i
  | BVar i -> Printf.bprintf buf "Tb%i" (depth-i-1)
  | App (p, []) -> Buffer.add_string buf (Symbol.to_string p)
  | App (p, args) ->
    Printf.bprintf buf "%s(%a)"
      (Symbol.to_string p) (Util.pp_list (pp_rec depth)) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) ->
    Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a"
      (Util.pp_list ~sep:" * " (pp_inner depth)) l (pp_rec depth) ret
  | Forall ty' ->
    Printf.bprintf buf "∀ Tb%i. %a" depth (pp_inner (depth+1)) ty'
and pp_inner depth buf t = match view t with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp_rec depth buf t; Buffer.add_char buf ')'
  | _ -> pp_rec depth buf t

let pp buf t = pp_rec 0 buf t

(* let pp buf ty = T.pp buf ty *)
let to_string ty = T.to_string ty
let fmt fmt ty = T.fmt fmt ty

(*
let bij =
  let (!!) = Lazy.force in
  Bij.(fix (fun bij' ->
    let bij_app = lazy (pair string_ (list_ (!! bij'))) in
    let bij_fun = lazy (pair (!! bij') (list_ (!! bij'))) in
    let bij_forall = bij' in
    switch
      ~inject:(fun ty -> match ty.ty with
        | Var i -> "v", BranchTo (int_, i)
        | BVar i -> "bv", BranchTo (int_, i)
        | App (p, l) -> "at", BranchTo (!! bij_app, (p, l))
        | Fun (ret, l) -> "fun", BranchTo (!! bij_fun, (ret, l))
        | Forall ty -> "all", BranchTo(!! bij_forall, ty))
      ~extract:(function
        | "v" -> BranchFrom (int_, var)
        | "bv" -> BranchFrom (int_, bvar)
        | "at" -> BranchFrom (!! bij_app, fun (s,l) -> app s l)
        | "fun" -> BranchFrom (!! bij_fun, fun (ret,l) -> mk_fun ret l)
        | "all" -> BranchFrom (!! bij_forall, fun ty -> __forall ty)
        | _ -> raise (DecodingError "expected Type"))))
*)

(** {2 Misc} *)

let __var i =
  T.var ~ty:tType i
