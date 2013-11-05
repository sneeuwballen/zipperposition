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
  | Var of int              (** Universal type variable *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

let rec eq_struct t1 t2 = match t1.ty, t2.ty with
  | Var i1, Var i2 -> i1 = i2
  | App (s1, l1), App (s2, l2) -> 
    s1 = s2 && (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
  | Fun (ret1,l1), Fun (ret2,l2) ->
    ret1 == ret2 && (try List.for_all2 (==) l1 l2 with Invalid_argument _ -> false)
  | _, _ -> false

let hash t = t.id

let rec _hash_top t = match t.ty with
  | Var i -> i
  | App (s, l) -> Hash.hash_list hash (Hash.hash_string s) l
  | Fun (ret, l) -> Hash.hash_list hash (hash ret) l

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

(** {2 Basic types} *)

let const s = s @@ []

let app s args = s @@ args

let var i =
  if i < 0 then failwith "Type.var: expected a nonnegative int";
  H.hashcons {ty=Var i; id= ~-1; ground=false; }

let mk_fun = (<==)

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

let free_vars ty =
  let set = _free_vars Set.empty ty in
  Set.elements set

let arity ty = match ty.ty with
  | Fun (_, l) -> List.length l
  | Var _
  | App _ -> 0

let is_ground t = t.ground

let rec size ty = match ty.ty with
  | Var _
  | App (_, []) -> 1
  | App (s, l) -> List.fold_left (fun acc ty' -> acc + size ty') 1 l
  | Fun (ret, l) -> List.fold_left (fun acc ty' -> acc + size ty') (size ret) l

let apply_fun f args =
  (* recursive matching of expected arguments and provided arguments.
    careful: we could have a curried function *)
  let rec apply_fun f_ret f_args args = match f_ret.ty, f_args, args with
    | _, x::f_args', y::args' ->
      (* match arguments *)
      if eq x y
        then apply_fun f_ret f_args' args'
        else failwith "Type.apply_fun: argument type mismatch"
    | _, [], [] -> f_ret  (* total application *)
    | Fun (f_ret', f_args'), _, _ -> assert false
    | _, [], _ -> failwith "Type.apply_fun: too many arguments"
    | _, _::_, [] ->
      (* partial application. The remaining arguments need be provided *)
      mk_fun f_ret f_args
  in
  match f.ty, args with
  | _, [] -> f
  | Fun (ret, l), l' -> apply_fun ret l l'
  | _, _ -> failwith "Type.apply_fun: expected function type"

let rec looks_similar ty1 ty2 =
  ty1 == ty2 ||
  match ty1.ty, ty2.ty with
  | Var _, _
  | _, Var _ -> true
  | App (s1, []), App (s2, []) -> s1 = s2
  | App (_, []), App (_, _)
  | App (_, _), App (_, []) -> false
  | App (s1, l1), App (s2, l2) ->
    s1 = s2 && looks_similar_list l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) ->
    looks_similar ret1 ret2 &&
    looks_similar_list l1 l2
  | _ -> false
and looks_similar_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | ty1::l1', ty2::l2' -> looks_similar ty1 ty2 && looks_similar_list l1' l2

(** {2 IO} *)

let rec pp buf t = match t.ty with
  | Var i -> Printf.bprintf buf "T%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
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
  Bij.(fix (fun bij' ->
    let bij_app = lazy (pair string_ (list_ (Lazy.force bij'))) in
    let bij_fun = lazy (pair (Lazy.force bij') (list_ (Lazy.force bij'))) in
    switch
      ~inject:(fun ty -> match ty.ty with
        | Var i -> "v", BranchTo (int_, i)
        | App (p, l) -> "at", BranchTo (Lazy.force bij_app, (p, l))
        | Fun (ret, l) -> "fun", BranchTo (Lazy.force bij_fun, (ret, l)))
      ~extract:(function
        | "v" -> BranchFrom (int_, var)
        | "at" -> BranchFrom (Lazy.force bij_app, fun (s,l) -> app s l)
        | "fun" -> BranchFrom (Lazy.force bij_fun, fun (ret,l) -> mk_fun ret l)
        | _ -> raise (DecodingError "expected Type"))))

(** {2 Parsed types} *)

module Parsed = struct
  type t =
    | Var of string
    | App of string * t list
    | Fun of t * t list

  let eq a b = a = b
  let cmp a b = Pervasives.compare a b
  let hash a = Hashtbl.hash a

  let var s = Var s
  let app s l = App (s, l)
  let const s = app s []

  let mk_fun ret l = match l with
    | [] -> ret
    | _ ->
      (* see {!(<==)} for the invariants *)
      begin match ret with
      | Fun (ret', l') -> Fun (ret', l @ l')
      | _ -> Fun (ret, l)
      end

  let (<==) = mk_fun
  let (<=.) a b = mk_fun a [b]

  let i = const "$i"
  let o = const "$o"
  let int = const "$int"
  let rat = const "$rat"
  let real = const "$real"
  let tType = const "$tType"

  let rec pp buf t = match t with
    | Var s -> Buffer.add_string buf s
    | App (s, []) -> Buffer.add_string buf s
    | App (s, l) ->
      Printf.bprintf buf "%s(%a)" s (Util.pp_list pp) l
    | Fun (ret, [x]) ->
      Printf.bprintf buf "%a > %a" pp x pp ret
    | Fun (ret, l) ->
      Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp) l pp ret

  let pp_tstp = pp
  let to_string = Util.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

type ctx = (string, ty) Hashtbl.t

let create_ctx () = Hashtbl.create 5

let of_parsed ?(ctx=create_ctx ()) ty =
  let rec convert ty = match ty with
  | Parsed.Var s ->
    begin try
      Hashtbl.find ctx s
    with Not_found ->
      let v = var (Hashtbl.length ctx) in
      Hashtbl.add ctx s v;
      v
    end
  | Parsed.App (s, l) ->
    let l = List.map convert l in
    app s l
  | Parsed.Fun (ret, l) ->
    let ret = convert ret in
    let l = List.map convert l in
    mk_fun ret l
  in
  convert ty

let rec to_parsed ty = match ty.ty with
  | Var i -> Parsed.var (Util.sprintf "T%d" i)
  | App (s, l) -> Parsed.app s (List.map to_parsed l)
  | Fun (ret, l) -> Parsed.mk_fun (to_parsed ret) (List.map to_parsed l)

(** {2 Misc} *)

let __var i =
  H.hashcons {ty=Var i; id= ~-1; ground=false; }
