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

type t =
  | Var of int              (** Universal type variable *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

let rec eq_struct t1 t2 = match t1, t2 with
  | Var i1, Var i2 -> i1 = i2
  | App (s1, l1), App (s2, l2) when List.length l1 = List.length l2 ->
    s1 = s2 && List.for_all2 (==) l1 l2
  | Fun (ret1,l1), Fun (ret2,l2) when List.length l1 = List.length l2 ->
    ret1 == ret2 && List.for_all2 (==) l1 l2
  | _, _ -> false

let rec hash t = match t with
  | Var i -> i
  | App (s, l) -> Hash.hash_list hash (Hash.hash_string s) l
  | Fun (ret, l) -> Hash.hash_list hash (hash ret) l

(* hashconsing *)
module H = Hashcons.Make(struct
  type t = ty
  let equal = eq_struct
  let hash = hash
  let tag i ty = ()
end)

let eq t1 t2 = t1 == t2

let __to_int = function
  | Var _ -> 0
  | App _ -> 1
  | Fun _ -> 2

let rec cmp t1 t2 = match t1, t2 with
  | Var i1, Var i2 -> i1 - i2
  | App (s1, l1), App (s2, l2) ->
    let c = String.compare s1 s2 in
    if c <> 0 then c else Util.lexicograph cmp l1 l2
  | Fun (ret1, l1), Fun(ret2, l2) ->
    let c = cmp ret1 ret2 in
    if c <> 0 then c else Util.lexicograph cmp l1 l2
  | _, _ -> __to_int t1 - __to_int t2

let is_var = function | Var _ -> true | _ -> false
let is_app = function | App _ -> true | _ -> false
let is_fun = function | Fun _ -> true | _ -> false

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

let (<==) ret args =
  match args with
  | [] -> ret
  | _::_ -> H.hashcons (Fun (ret, args))

let (<=.) ret arg =
  H.hashcons (Fun (ret, [arg]))

let (@@) s args =
  H.hashcons (App (s, args))

(** {2 Basic types} *)

let const s = H.hashcons (App (s, []))

let app s args = H.hashcons (App (s, args))

let var i =
  if i < 0 then failwith "Type.var: expected a nonnegative int";
  H.hashcons (Var si)

let mk_fun = (<==)

let i = const "$i"
let o = const "$o"
let int = const "$int"
let rat = const "$rat"
let real = const "$real"
let tType = const "$tType"

(** {2 Utils} *)

let rec _free_vars set ty = match ty with
  | Var _ -> Set.add ty set
  | App (_, l) -> List.fold_left _free_vars set l
  | Fun (ret, l) -> List.fold_left _free_vars (_free_vars set ret) l

let free_vars ty =
  let set = _free_vars Set.empty ty in
  Set.elements set

let arity ty = match ty with
  | Fun (_, l) -> List.length l
  | Var _
  | App _ -> 0

let rec is_ground t = match t with
  | Var _ -> false
  | App (_, l) -> List.for_all is_ground l
  | Fun (ret, l) -> is_ground ret && List.for_all is_ground l

let rec curry ty = match ty with
  | Var _ -> ty
  | App (s, l) -> app s (List.map curry l)
  | Fun (ret, l) ->
    List.fold_left
      (fun ret arg -> mk_fun ret [curry arg])
      (curry ret) l

let rec uncurry ty = match ty with
  | Var _ -> ty
  | App (s, l) -> app s (List.map uncurry l)
  | Fun _ ->
    begin match _gather_uncurry ty [] with
    | [] -> failwith "Type.uncurry: expected curried type"
    | ret::args -> mk_fun (uncurry ret) args
    end
(* given a curried function type, recover all its argument types into
    a list prepended to [acc] *)
and _gather_uncurry ty acc = match ty with
  | Var _
  | App _ -> uncurry ty :: acc  (* proper return value *)
  | Fun (ret, [arg]) -> _gather_uncurry ret (uncurry arg :: acc)
  | Fun _ -> uncurry ty :: acc (* consider this as a single argument *)

let rec size ty = match ty with
  | Var _ -> 1
  | App (s, []) -> 1
  | App (s, l) -> List.fold_left (fun acc ty' -> acc + size ty') 1 l
  | Fun (ret, l) -> List.fold_left (fun acc ty' -> acc + size ty') (size ret) l

(** {2 IO} *)

let rec pp buf t = match t with
  | Var i -> Printf.bprintf buf "T%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
and pp_inner buf t = match t with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp buf t; Buffer.add_char buf ')'
  | _ -> pp buf t

let rec pp_tstp buf t = match t with
  | Var i -> Printf.bprintf buf "T%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
and pp_inner buf t = match t with
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
      ~inject:(fun ty -> match ty with
        | Var i -> "v", BranchTo (int_, i)
        | App (p, l) -> "at", BranchTo (Lazy.force bij_app, (p, l))
        | Fun (ret, l) -> "fun", BranchTo (Lazy.force bij_fun, (ret, l)))
      ~extract:(function
        | "v" -> BranchFrom (int_, var)
        | "at" -> BranchFrom (Lazy.force bij_app, fun (s,l) -> app s l)
        | "fun" -> BranchFrom (Lazy.force bij_fun, fun (ret,l) -> mk_fun ret l)
        | _ -> raise (DecodingError "expected Type"))))

let arbitrary =
  QCheck.Arbitrary.(
    let var = among [var 0; var 1 ] in
    let base = choose [ among [ i; const "$int"; const "a"; const "b"; ] ; var ] in
    fix ~max:4 ~base (fun sub -> choose
      [ lift (app "list") (list_repeat 1 sub)
      ; lift (app "prod") (list_repeat 2 sub)
      ; lift2 mk_fun sub (list sub)
      ]))

let arbitrary_ground =
  QCheck.Arbitrary.(
    let base = among [ i; const "$int"; const "a"; const "b" ] in
    fix ~max:4 ~base (fun sub -> choose
      [ lift (app "list") (list_repeat 1 sub)
      ; lift (app "prod") (list_repeat 2 sub)
      ; lift2 mk_fun sub (list sub)
      ]))


(** {2 Misc} *)

let __var i = H.hashcons (Var i)
