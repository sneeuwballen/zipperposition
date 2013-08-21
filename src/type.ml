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

type t =
  | Var of int              (** Type variable *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

let compare_struct t1 t2 = Pervasives.compare t1 t2

let rec hash t = match t with
  | Var i -> Hash.hash_int i
  | App (s, l) -> Hash.hash_list hash (Hash.hash_string s) l
  | Fun (ret, l) -> Hash.hash_list hash (hash ret) l

(* hashconsing *)
module H = Hashcons.Make(struct
  type t = ty
  let equal a b = compare_struct a b = 0
  let hash = hash
  let tag i ty = ()
end)

let eq t1 t2 = t1 == t2

let cmp t1 t2 = compare_struct t1 t2

exception Error of string
  (** Type error *)

let is_var = function | Var _ -> true | _ -> false
let is_app = function | App _ -> true | _ -> false
let is_fun = function | Fun _ -> true | _ -> false

(** {2 Infix constructors} *)

module Infix = struct
  let (<==) ret args =
    match args with
    | [] -> ret
    | _::_ -> H.hashcons (Fun (ret, args))

  let (<=.) ret arg = ret <== [arg]

  let (@@) s args =
    H.hashcons (App (s, args))
end

(** {2 Basic types} *)

let const s = H.hashcons (App (s, []))

let app s args = H.hashcons (App (s, args))

let var i =
  assert (i >= 0);
  H.hashcons (Var i)

let mk_fun = Infix.(<==)

let i = const "$i"
let o = const "$o"
let int = const "$int"
let rat = const "$rat"
let real = const "$real"

(** {2 IO} *)

(* printing var *)
let pp_var buf i =
  if i >= 0 && i < Char.code 'z'
    then Printf.bprintf buf "'%c" (Char.chr (Char.code 'a' + i))
    else Printf.bprintf buf "'a%d" i

let rec pp buf t = match t with
  | Var i -> pp_var buf i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp arg pp ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:"*" pp) l pp ret

let to_string t =
  let b = Buffer.create 15 in
  pp b t;
  Buffer.contents b

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let bij =
  Bij.(fix (fun bij' ->
    let bij_app () = pair string_ (list_ (bij' ())) in
    let bij_fun () = pair (bij' ()) (list_ (bij' ())) in
    switch
      ~inject:(function
        | Var i -> 'v', BranchTo (int_, i)
        | App (p, l) -> 'a', BranchTo (bij_app (), (p, l))
        | Fun (ret, l) -> 'f', BranchTo (bij_fun (), (ret, l)))
      ~extract:(function
        | 'v' -> BranchFrom (int_, var)
        | 'a' -> BranchFrom (bij_app (), fun (s,l) -> app s l)
        | 'f' -> BranchFrom (bij_fun (), fun (ret,l) -> mk_fun ret l)
        | _ -> raise (DecodingError "expected Type"))))

(** {2 Type unification} *)

module VarSet = Sequence.Set.Make(struct
  type t = int
  let compare i j = i - j
end)

module Subst = struct
  module IntMap = Sequence.Map.Make(struct
    type t = int
    let compare i j = i - j
  end)

  type t = ty IntMap.t
  
  let empty = IntMap.empty

  let bind subst i ty =
    IntMap.add i ty subst

  let bind_seq subst seq =
    Sequence.fold (fun subst (i, ty) -> IntMap.add i ty subst) subst seq

  let rec apply subst t = match t with
    | Var i ->
      begin try
        let t' = IntMap.find i subst in
        apply subst t'
      with Not_found -> t
      end
    | App (s, l) -> app s (List.map (apply subst) l)
    | Fun (ret, l) -> mk_fun (apply subst ret) (List.map (apply subst) l)

  let rec apply_not_rec subst t = match t with
    | Var i ->
      begin try
        let t' = IntMap.find i subst in
        t'
      with Not_found -> t
      end
    | App (s, l) -> app s (List.map (apply_not_rec subst) l)
    | Fun (ret, l) ->
      mk_fun (apply_not_rec subst ret) (List.map (apply_not_rec subst) l)

  let to_seq = IntMap.to_seq

  let pp buf subst =
    let pp_pair buf (a,b) = Printf.bprintf buf "%a -> %a" pp a pp b in
    let open Sequence.Infix in
    Buffer.add_char buf '{';
    to_seq subst
      |> Sequence.map (fun (i, v) -> var i, v)
      |> Util.pp_seq pp_pair buf;
    Buffer.add_char buf '}'

  let to_string subst =
    let b = Buffer.create 20 in
    pp b subst;
    Buffer.contents b

  let fmt fmt subst =
    Format.pp_print_string fmt (to_string subst)
end

let free_vars ?(init=VarSet.empty) t =
  let rec find vars t = match t with
  | Var i -> VarSet.add i vars
  | App (_, l) -> List.fold_left find vars l
  | Fun (ret, l) -> List.fold_left find (find vars ret) l
  in
  find init t

let max_var t =
  let rec find acc t = match t with
  | Var i -> max acc i
  | App (_, l) -> List.fold_left find acc l
  | Fun (ret, l) -> List.fold_left find (find acc ret) l
  in find 0 t

let arity ty = match ty with
  | Fun (_, l) -> List.length l
  | Var _
  | App _ -> 0

let rec is_ground t = match t with
  | Var _ -> false
  | App (_, l) -> List.for_all is_ground l
  | Fun (ret, l) -> is_ground ret && List.for_all is_ground l

let normalize t =
  let vars = free_vars t in
  if VarSet.is_empty vars
    then t
    else
      let open Sequence.Infix in
      let subst = VarSet.to_seq vars
        |> Sequence.mapi (fun i v -> v, var i)
        |> Subst.bind_seq Subst.empty
      in
      Subst.apply_not_rec subst t

let rename offset t =
  if offset = 0
  then t
  else
    let vars = free_vars t in
    if VarSet.is_empty vars
      then t
      else
        let open Sequence.Infix in
        let subst = VarSet.to_seq vars
          |> Sequence.map (fun i -> i, var (i+offset))
          |> Subst.bind_seq Subst.empty
        in
        Subst.apply_not_rec subst t

(* occur-check *)
let rec _occur_check i t = match t with
  | Var j -> i = j
  | App (_, l) -> List.exists (_occur_check i) l
  | Fun (ret, l) ->
    _occur_check i ret || List.exists (_occur_check i) l

(* unification *)
let rec unify_rec subst t1 t2 =
  let t1 = Subst.apply subst t1 in
  let t2 = Subst.apply subst t2 in
  match t1, t2 with
  | Var i, _ when not (_occur_check i t2) ->
    Subst.bind subst i t2
  | _, Var j when not (_occur_check j t1) ->
    Subst.bind subst j t1
  | App (s1, l1), App (s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
    List.fold_left2 unify_rec subst l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) when List.length l1 = List.length l2 ->
    let subst = unify_rec subst ret1 ret2 in
    List.fold_left2 unify_rec subst l1 l2
  | _, _ -> raise (Error "unification error")

let unify ?(subst=Subst.empty) t1 t2 =
  unify_rec subst t1 t2

(* alpha-equivalence check *)
let rec alpha_equiv_unify subst t1 t2 =
  let t1 = Subst.apply subst t1 in
  let t2 = Subst.apply subst t2 in
  match t1, t2 with
  | Var i, Var j -> Subst.bind subst i t2
  | App (s1, l1), App (s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
    List.fold_left2 alpha_equiv_unify subst l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) when List.length l1 = List.length l2 ->
    let subst = alpha_equiv_unify subst ret1 ret2 in
    List.fold_left2 alpha_equiv_unify subst l1 l2
  | _, _ -> raise (Error "not alpha equivalent")

let alpha_equiv t1 t2 =
  let offset = max_var t1 in
  let t2 = rename offset t2 in
  try
    ignore (alpha_equiv_unify Subst.empty t1 t2);
    true
  with Error _ ->
    false
