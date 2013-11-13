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
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t list * t    (** explicit quantification *)

type ty = t

let rec eq_struct t1 t2 = match t1.ty, t2.ty with
  | Var i1, Var i2 -> i1 = i2
  | App (s1, l1), App (s2, l2) -> 
    s1 = s2 && (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
  | Fun (ret1,l1), Fun (ret2,l2) ->
    ret1 == ret2 && (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
  | Forall (vars1,ty1), Forall (vars2, ty2) ->
    ty1 == ty2 && (try List.for_all2 (==) vars1 vars2 with Invalid_argument _ -> false)
  | _, _ -> false

let hash t = t.id

let rec _hash_top t = match t.ty with
  | Var i -> i
  | App (s, l) -> Hash.hash_list hash (Hash.hash_string s) l
  | Fun (ret, l) -> Hash.hash_list hash (hash ret) l
  | Forall (vars, ty) -> Hash.hash_list hash (hash ty) vars

(* hashconsing *)
module H = Hashcons.Make(struct
  type t = ty
  let equal = eq_struct
  let hash = _hash_top
  let tag i ty = ty.id <- i
end)

let eq t1 t2 = t1 == t2

let cmp ty1 ty2 = ty1.id - ty2.id

let is_var = function | {ty=Var _} -> true | _ -> false
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
    ty.ground <- ret.ground && _list_ground args
    end;
  ty'

let rec (<==) ret args =
  match args with
  | [] -> ret
  | _::_ ->
    match ret.ty with
    | Fun (ret', args') ->
      (* invariant: flatten function types. Symmetric w.r.t the {!HOTerm.At}
          constructor invariant. [args] must be applied before [args']
          need to be supplied.
          Example: [(a <- b) <- c] requires [c] first *)
      ret' <== (args @ args')
    | _ -> _mk_fun ret args

let (<=.) ret arg = ret <== [arg]

let (@@) s args =
  let ty = {ty=App(s, args); id= ~-1; ground=false} in
  let ty' = H.hashcons ty in
  if ty == ty' then begin
    ty.ground <- _list_ground args;
    end;
  ty'

let const s = s @@ []

let app s args = s @@ args

let var i =
  if i < 0 then failwith "Type.var: expected a nonnegative int";
  H.hashcons {ty=Var i; id= ~-1; ground=false; }

let mk_fun = (<==)

let forall vars ty =
  if not (List.for_all is_var vars) then raise (Invalid_argument "Type.forall");
  match vars, ty.ty with
  | [], _ -> ty
  | _::_, Forall (vars', ty') ->
    (* flatten forall's *)
    H.hashcons {ty=Forall(vars@vars', ty'); id= ~-1; ground=false; }
  | _::_, _ ->
    H.hashcons {ty=Forall(vars, ty); id= ~-1; ground=false; }

(** {2 Basic types} *)

let i = const "$i"
let o = const "$o"
let int = const "$int"
let rat = const "$rat"
let real = const "$real"
let tType = const "$tType"

(** {2 Utils} *)

let rec _free_vars set ty =
  if ty.ground then set
  else match ty.ty with
  | Var _ -> Set.add ty set
  | App (_, l) -> List.fold_left _free_vars set l
  | Fun (ret, l) -> List.fold_left _free_vars (_free_vars set ret) l
  | Forall (vars, ty') ->
    let set' = _free_vars Set.empty ty' in
    let set' = Set.filter (fun v -> not (List.memq v vars)) set' in
    Set.union set set'

let free_vars ty =
  let set = _free_vars Set.empty ty in
  Set.elements set

let close_forall ty =
  let fvars = free_vars ty in
  forall fvars ty

let rec arity ty = match ty.ty with
  | Fun (_, l) -> 0, List.length l
  | Var _
  | App _ -> 0, 0
  | Forall (vars, ty') ->
    let i1, i2 = arity ty' in
    List.length vars + i1, i2

let rec expected_args ty = match ty.ty with
  | Fun (_, l) -> [], l
  | Var _
  | App _ -> [], []
  | Forall (vars, ty') ->
    let l1, l2 = expected_args ty' in
    vars @ l1, l2

let is_ground t = t.ground

let rec size ty = match ty.ty with
  | Var _
  | App (_, []) -> 1
  | App (s, l) -> List.fold_left (fun acc ty' -> acc + size ty') 1 l
  | Fun (ret, l) -> List.fold_left (fun acc ty' -> acc + size ty') (size ret) l
  | Forall (vars, ty') -> size ty' + List.length vars

let _is_empty_subst = function | [] -> true | _::_ -> false

(* substitute variables by types in [ty] *)
let rec _apply_subst subst ty =
  if _is_empty_subst subst then ty
  else match ty.ty with
  | Var _ ->
    begin try
      let ty' = List.assq ty subst in
      _apply_subst subst ty'
    with Not_found -> ty
    end
  | App (s, l) -> app s (_apply_subst_list subst l)
  | Fun (ret, l) -> mk_fun (_apply_subst subst ret) (_apply_subst_list subst l)
  | Forall (vars, ty') ->
    (* hide the bound variables *)
    let subst = List.filter (fun (v,_) -> not (List.memq v vars)) subst in
    forall vars (_apply_subst subst ty')
and _apply_subst_list subst l = match l with
  | [] -> []
  | ty::l' -> _apply_subst subst ty :: _apply_subst_list subst l'

(* apply a type to arguments *)
let apply ty args =
  (* apply subst(ty) to subst(args) *)
  let rec apply subst ty args =
    match ty.ty, args with
    | _, [] -> _apply_subst subst ty
    | Fun (ret, l), l' -> apply_fun subst ret l l'
    | Forall (vars, ty'), _ -> apply_forall subst ty' vars args
    | _, _ -> failwith "Type.apply: expected function or forall type"
  (* recursive matching of expected arguments and provided arguments.
    careful: we could have a curried function *)
  and apply_fun subst f_ret f_args args = match f_args, args with
    | x::f_args', y::args' ->
      (* match arguments after substitution *)
      if eq (_apply_subst subst x) (_apply_subst subst y)
        then apply_fun subst f_ret f_args' args'
        else failwith "Type.apply: argument type mismatch"
    | [], [] ->
      (* total application *)
      _apply_subst subst f_ret
    | [], _ -> failwith "Type.apply: too many arguments"
    | _::_, [] ->
      (* partial application. The remaining arguments need be provided *)
      mk_fun (_apply_subst subst f_ret) (_apply_subst_list subst f_args)
  (* forall(vars,ty') applied to args *)
  and apply_forall subst ty' vars args = match vars, args with
    | [], _ -> apply subst ty' args
    | v::vars', a::args' ->
      let subst' = (v,a) :: subst in
      apply_forall subst' ty' vars' args'
  in
  apply [] ty args

(** {2 IO} *)

let rec pp buf t = match t.ty with
  | Var i -> Printf.bprintf buf "T%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
  | Forall (vars, ty') ->
    Printf.bprintf buf "∀ %a. %a" (Util.pp_list pp) vars pp_inner ty'
and pp_inner buf t = match t.ty with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp buf t; Buffer.add_char buf ')'
  | _ -> pp buf t

let rec pp_tstp buf t = match t.ty with
  | Var i -> Printf.bprintf buf "T%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
  | Forall (vars, ty') ->
    Printf.bprintf buf "!>[%a]: %a" (Util.pp_list pp_bvar) vars pp_inner ty'
and pp_bvar buf v =
  pp_tstp buf v;
  Buffer.add_string buf ":%tType"
and pp_inner buf t = match t.ty with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp_tstp buf t; Buffer.add_char buf ')'
  | _ -> pp_tstp buf t

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
    let bij_forall = lazy (pair (list_ (!! bij')) (!! bij')) in
    switch
      ~inject:(fun ty -> match ty.ty with
        | Var i -> "v", BranchTo (int_, i)
        | App (p, l) -> "at", BranchTo (!! bij_app, (p, l))
        | Fun (ret, l) -> "fun", BranchTo (!! bij_fun, (ret, l))
        | Forall (vars, ty) -> "all", BranchTo(!! bij_forall, (vars, ty)))
      ~extract:(function
        | "v" -> BranchFrom (int_, var)
        | "at" -> BranchFrom (!! bij_app, fun (s,l) -> app s l)
        | "fun" -> BranchFrom (!! bij_fun, fun (ret,l) -> mk_fun ret l)
        | "all" -> BranchFrom (!! bij_forall, fun (vars,ty) -> forall vars ty)
        | _ -> raise (DecodingError "expected Type"))))

(** {2 Misc} *)

let __var i =
  H.hashcons {ty=Var i; id= ~-1; ground=false; }
