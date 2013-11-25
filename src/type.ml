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

let prof_unify = Util.mk_profiler "Type.unify"
let prof_variant = Util.mk_profiler "Type.variant"

(** {2 Main Type representation} *)

type t = {
  ty : tree; (* shape of the term *)
  mutable ground : bool;
  mutable id : int;  (* hashconsing tag *)
}
and tree =
  | Var of int              (** type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t             (** explicit quantification *)

type ty = t

exception Error of string
  (** Generic error on types. *)

let eq t1 t2 = t1 == t2

let hash t = t.id

let cmp ty1 ty2 = ty1.id - ty2.id

let rec _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | x1::l1', x2::l2' -> eq x1 x2 && _eq_list l1' l2'

let _eq_top t1 t2 = match t1.ty, t2.ty with
  | Var i1, Var i2 
  | BVar i1, BVar i2 -> i1 = i2
  | App (s1, l1), App (s2, l2) -> 
    s1 = s2 && _eq_list l1 l2
  | Fun (ret1,l1), Fun (ret2,l2) ->
    eq ret1 ret2 && _eq_list l1 l2
  | Forall ty1, Forall ty2 -> eq ty1 ty2
  | _, _ -> false

let _hash_top t = match t.ty with
  | Var i -> i
  | BVar i -> Hash.combine 33 i
  | App (s, l) -> Hash.hash_list hash (Hash.hash_string s) l
  | Fun (ret, l) -> Hash.hash_list hash (hash ret) l
  | Forall ty ->  Hash.combine 12 (hash ty)

(* hashconsing *)
module H = Hashcons.Make(struct
  type t = ty
  let equal = _eq_top
  let hash = _hash_top
  let tag i ty = (assert (ty.id = ~-1); ty.id <- i)
end)

let is_var = function | {ty=Var _} -> true | _ -> false
let is_bvar = function | {ty=BVar _} -> true | _ -> false
let is_app = function | {ty=App _} -> true | _ -> false
let is_fun = function | {ty=Fun _} -> true | _ -> false
let is_forall = function | {ty=Forall _} -> true | _ -> false

module Tbl = Hashtbl.Make(struct
  type t = ty
  let equal = eq
  let hash = hash
end)

module Set = Sequence.Set.Make(struct
  type t = ty
  let compare = cmp
end)

(** {2 Infix constructors} *)

let rec _list_ground l = match l with
  | [] -> true
  | ty::l' -> ty.ground && _list_ground l'

(* real constructor *)
let _mk_fun ret args =
  let ty = {ty=Fun (ret, args); id= ~-1; ground=false;} in
  let ty' = H.hashcons ty in
  if ty == ty' then begin
    ty'.ground <- ret.ground && _list_ground args
    end;
  ty'

let rec mk_fun ret args =
  match args with
  | [] -> ret
  | _::_ ->
    match ret.ty with
    | Fun (ret', args') ->
      (* invariant: flatten function types. Symmetric w.r.t the {!HOTerm.At}
          constructor invariant. [args] must be applied before [args']
          need to be supplied.
          Example: [(a <- b) <- c] requires [c] first *)
      mk_fun ret' (args @ args')
    | _ -> _mk_fun ret args

let (<==) = mk_fun

let (<=.) ret arg = mk_fun ret [arg]

let app s args =
  let my_ty = {ty=App(s, args); id= ~-1; ground=false} in
  let ty' = H.hashcons my_ty in
  if my_ty == ty' then begin
    ty'.ground <- _list_ground args;
    end;
  ty'

let const s = app s []

let (@@) = app

let var i =
  if i < 0 then failwith "Type.var: expected a nonnegative int";
  H.hashcons {ty=Var i; id= ~-1; ground=false; }

let bvar i =
  if i < 0 then failwith "Type.bvar: expected a nonnegative int";
  H.hashcons {ty=BVar i; id= ~-1; ground=false; }

(* real constructor *)
let __forall ty =
  H.hashcons {ty=Forall ty; id= ~-1; ground=ty.ground; }

(** Handling De Bruijn indexes. We assume that all types are
    always {!DB.closed}, ie all De Bruijn indices are properly bound
    by a quantifier. *)
module DB = struct
  (* type is closed (ie all {!BVar} are properly scoped *)
  let closed ?(depth=0) ty =
    let rec closed depth ty =
      ty.ground || match ty.ty with
      | Var _
      | App (_, []) -> true
      | BVar i -> i < depth
      | App (_, l) -> closed_list depth l
      | Fun (ret, l) -> closed depth ret && closed_list depth l
      | Forall ty' -> closed (depth+1) ty'
    and closed_list depth l = match l with
      | [] -> true
      | ty::l' -> closed depth ty && closed_list depth l'
    in
    closed depth ty

  (* replace [var] by outermost De Bruijn *)
  let replace ?(depth=0) ty ~var =
    let rec recurse depth ty =
      if ty.ground then ty
      else match ty.ty with
      | Var _ when eq var ty -> bvar depth  (* replace [var] by De Bruijn *)
      | Var _
      | App (_, []) -> ty
      | BVar i -> assert (i<depth); ty  (* must be closed *)
      | Fun (ret, l) -> mk_fun (recurse depth ret) (recurse_l depth l)
      | App (s, l) -> app s (recurse_l depth l)
      | Forall ty' -> __forall (recurse (depth+1) ty')
    and recurse_l depth l = match l with
      | [] -> []
      | ty::l' -> recurse depth ty :: recurse_l depth l'
    in
    recurse depth ty

  (* shift free De Bruijn indexes by [n] *)
  let shift ?(depth=0) n ty =
    let rec shift depth ty =
      if ty.ground then ty
      else match ty.ty with
        | Var _ -> ty
        | BVar i when i < depth -> ty  (* protected *)
        | BVar i -> bvar (i+n)         (* shift *)
        | Fun (ret, l) -> mk_fun (shift depth ret) (shift_l depth l)
        | App (s, l) -> app s (shift_l depth l)
        | Forall ty' -> __forall (shift (depth+1) ty')
    and shift_l depth = function
      | [] -> []
      | ty::l' -> shift depth ty :: shift_l depth l'
    in
    shift depth ty

  (* evaluate ty in the given environment. *)
  let eval ?(depth=0) env ty =
    let rec eval depth env ty =
    if ty.ground then ty
    else match ty.ty with
      | BVar i ->
        begin match DBEnv.find env i with
          | None -> ty
          | Some ty' -> shift depth ty'
        end
      | App (_, [])
      | Var _ -> ty
      | App (s, l) -> app s (eval_list depth env l)
      | Fun (ret, l) -> mk_fun (eval depth env ret) (eval_list depth env l)
      | Forall ty' -> __forall (eval (depth+1) (DBEnv.push_none env) ty')
    and eval_list depth env l = match l with
      | [] -> []
      | ty::l' ->
        eval depth env ty :: eval_list depth env l'
    in
    eval depth env ty

  let eval_list ?(depth=0) env l = List.map (eval ~depth env) l
end

let rec forall vars ty = match vars with
  | [] -> ty
  | v::vars' ->
    assert (is_var v);
    let ty' = forall vars' ty in
    let ty' = DB.replace ty' ~var:v in
    __forall ty'

(** {2 Basic types} *)

let i = const "$i"
let o = const "$o"
let int = const "$int"
let rat = const "$rat"
let real = const "$real"
let tType = const "$tType"

(** {2 Utils} *)

let rec free_vars_set set ty =
  if ty.ground then set
  else match ty.ty with
  | Var _ -> Set.add ty set
  | BVar _ -> set
  | App (_, l) -> List.fold_left free_vars_set set l
  | Fun (ret, l) -> List.fold_left free_vars_set (free_vars_set set ret) l
  | Forall ty' -> free_vars_set set ty'

let free_vars ty =
  let set = free_vars_set Set.empty ty in
  Set.elements set

let close_forall ty =
  let fvars = free_vars ty in
  forall fvars ty

let rec arity ty = match ty.ty with
  | Fun (_, l) -> 0, List.length l
  | Var _
  | BVar _
  | App _ -> 0, 0
  | Forall ty' ->
    let i1, i2 = arity ty' in
    i1 + 1, i2

let rec expected_args ty = match ty.ty with
  | Fun (_, l) -> l
  | BVar _
  | Var _
  | App _ -> []
  | Forall ty' -> expected_args ty'

let is_ground t = t.ground

let rec size ty = match ty.ty with
  | Var _
  | BVar _
  | App (_, []) -> 1
  | App (s, l) -> List.fold_left (fun acc ty' -> acc + size ty') 1 l
  | Fun (ret, l) -> List.fold_left (fun acc ty' -> acc + size ty') (size ret) l
  | Forall ty' -> 1 + size ty' 

let _error msg = raise (Error msg)

let rec pp_rec depth buf t = match t.ty with
  | Var i -> Printf.bprintf buf "T%d" i
  | BVar i -> Printf.bprintf buf "Tb%i" (depth-i-1)
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list (pp_rec depth)) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " (pp_inner depth)) l (pp_rec depth) ret
  | Forall ty' ->
    Printf.bprintf buf "∀ Tb%i. %a" depth (pp_inner (depth+1)) ty'
and pp_inner depth buf t = match t.ty with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp_rec depth buf t; Buffer.add_char buf ')'
  | _ -> pp_rec depth buf t

let pp buf t = pp_rec 0 buf t

(* apply a type to arguments. *)
let apply ty args =
  (* apply ty to args *)
  let rec apply ty args =
    match ty.ty, args with
    | _, [] -> ty
    | Fun (ret, l), l' -> apply_fun ret l l'
    | Forall ty', a::args' ->
      let ty' = DB.eval (DBEnv.singleton a) ty' in
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
            "Type.apply: argument type mismatch, expected %a[%d] but got %a[%d]" pp x x.id pp y y.id in
          _error msg
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

(** {2 IO} *)

let rec pp_tstp_rec depth buf t = match t.ty with
  | Var i -> Printf.bprintf buf "T%d" i
  | BVar i -> Printf.bprintf buf "Tb%d" (depth-i-1)
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list (pp_tstp_rec depth)) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " (pp_inner depth)) l (pp_tstp_rec depth) ret
  | Forall ty' ->
    Printf.bprintf buf "!>[Tb%d:$tType]: %a" depth (pp_inner (depth+1)) ty'
and pp_inner depth buf t = match t.ty with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp_tstp_rec depth buf t; Buffer.add_char buf ')'
  | _ -> pp_tstp_rec depth buf t

let pp_tstp buf t = pp_tstp_rec 0 buf t

let to_string t =
  let b = Buffer.create 15 in
  pp b t;
  Buffer.contents b

let fmt fmt t = Format.pp_print_string fmt (to_string t)

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

(** {2 Misc} *)

let __var i =
  H.hashcons {ty=Var i; id= ~-1; ground=false; }
