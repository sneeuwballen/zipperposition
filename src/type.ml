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
  | Var of string           (** Universal type variable *)
  | GVar of int * t ref     (** Variable instance. The int is unique *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

let compare_struct t1 t2 = Pervasives.compare t1 t2

let rec hash t = match t with
  | Var s -> Hash.hash_string s
  | GVar (i, _) -> Hash.hash_int i
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
let is_gvar = function | GVar _ -> true | _ -> false
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

let var s = H.hashcons (Var s)

let new_var =
  let n = ref 0 in
  fun () ->
    incr n;
    let s = Printf.sprintf "$$ty_%d" !n in
    H.hashcons (Var s)

let new_gvar =
  let n = ref 0 in
  let __default = H.hashcons (Var "") in  (* some default type *)
  fun () ->
    incr n;
    let ty = H.hashcons (GVar (!n, ref __default)) in
    begin match ty with
    | GVar (_, r) -> r := ty  (* make [ty] point to itself *)
    | _ -> assert false
    end;
    ty

let mk_fun = Infix.(<==)

let i = const "$i"
let o = const "$o"
let int = const "$int"
let rat = const "$rat"
let real = const "$real"

(** {2 Utils} *)

let free_vars ty =
  let rec recurse acc ty = match ty with
  | Var _ -> acc
  | GVar _ -> if List.memq ty acc then acc else ty :: acc
  | App (_, l) -> List.fold_left recurse acc l
  | Fun (ret, l) -> List.fold_left recurse (recurse acc ret) l
  in
  recurse [] ty

let bound_vars ty =
  let rec recurse acc ty = match ty with
  | Var _ -> if List.memq ty acc then acc else ty :: acc
  | GVar _ -> acc
  | App (_, l) -> List.fold_left recurse acc l
  | Fun (ret, l) -> List.fold_left recurse (recurse acc ret) l
  in
  recurse [] ty

let rec is_closed ty = match ty with
  | Var _ -> true
  | GVar _ -> false
  | App (_, l) -> List.for_all is_closed l
  | Fun (ret, l) -> is_closed ret && List.for_all is_closed l

let rec deref ty = match ty with
  | Var _ -> ty
  | GVar (_, r) ->
    if !r == ty
      then ty   (* points to self, not bound *)
      else deref !r
  | App (s, l) ->
    let l' = List.map deref l in
    app s l'
  | Fun (ret, l) ->
    let ret' = deref ret in
    let l' = List.map deref l in
    mk_fun ret' l'

let bind gvar to_ =
  match gvar with
  | GVar (_, r) -> r := to_
  | _ -> raise (Invalid_argument "Type.bind: expect a GVar")

let arity ty = match ty with
  | Fun (_, l) -> List.length l
  | Var _
  | GVar _
  | App _ -> 0

let rec is_ground t = match t with
  | Var _ -> false
  | GVar _ -> false
  | App (_, l) -> List.for_all is_ground l
  | Fun (ret, l) -> is_ground ret && List.for_all is_ground l

(** {2 IO} *)

let rec pp buf t = match t with
  | Var s -> Printf.bprintf buf "'%s" s
  | GVar (i, _) -> Printf.bprintf buf "'_%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a -> %a" pp arg pp ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) -> %a" (Util.pp_list ~sep:"*" pp) l pp ret

let rec pp_tstp buf t = match t with
  | Var s -> Printf.bprintf buf "%s" (String.capitalize s)
  | GVar (i, _) -> failwith "Type.pp_tstp: free variables not printable"
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
        | Var s -> 'v', BranchTo (string_, s)
        | GVar _ -> failwith "Type.bij: GVar not supported"
        | App (p, l) -> 'a', BranchTo (bij_app (), (p, l))
        | Fun (ret, l) -> 'f', BranchTo (bij_fun (), (ret, l)))
      ~extract:(function
        | 'v' -> BranchFrom (string_, var)
        | 'a' -> BranchFrom (bij_app (), fun (s,l) -> app s l)
        | 'f' -> BranchFrom (bij_fun (), fun (ret,l) -> mk_fun ret l)
        | _ -> raise (DecodingError "expected Type"))))

(** {2 Unification} *)

module Stack = struct
  type t = {
    gvars : ty Stack.t;
    bindings : ty Stack.t;
    mutable size : int;  (* Stack.length is O(n)... *)
  }
  and pos = int

  let create () =
    { gvars = Stack.create ();
      bindings = Stack.create ();
      size = 0;
    }

  let clear st =
    Stack.clear st.gvars;
    Stack.clear st.bindings;
    st.size <- 0;
    ()

  let bottom = 0
  
  let save st = st.size

  let restore st pos =
    assert (st.size >= pos);
    while st.size > pos do
      let gvar = Stack.pop st.gvars in
      let binding = Stack.pop st.bindings in
      bind gvar binding;
      st.size <- st.size - 1;
    done

  let restore_all st = restore st bottom

  (* the current immediate binding of the variable *)
  let _binding gvar = match gvar with
  | GVar (_, r) -> !r
  | _ -> assert false

  let bind st gvar ty =
    Stack.push gvar st.gvars;
    Stack.push (_binding gvar) st.bindings;
    st.size <- st.size + 1;
    ()

  let protect st f =
    let pos = save st in
    try
      let x = f () in
      restore st pos;
      x
    with e ->
      restore st pos;
      raise e

  let unwind_protect st f =
    let pos = save st in
    try
      let x = f () in
      x
    with e ->
      restore st pos;
      raise e
end

(* instantiate all bound variables *)
let instantiate ty =
  let bvars = bound_vars ty in
  let subst = List.map (fun v -> v, new_gvar ()) bvars in
  let rec recurse subst ty = match ty with
  | Var _ -> List.assq ty subst
  | GVar _ -> ty
  | App (s, l) ->
    let l' = List.map (recurse subst) l in
    app s l'
  | Fun (ret, l) ->
    let ret' = recurse subst ret in
    let l' = List.map (recurse subst) l in
    mk_fun ret' l'
  in
  recurse subst ty

(* close all free variables *)
let close ty =
  let ty = deref ty in
  let gvars = free_vars ty in
  match gvars with
  | [] -> ty
  | _::_ ->
    let old_bindings = List.map deref gvars in
    (* bind free variables to new universal variables, and deref *)
    List.iter (fun gvar -> bind gvar (new_var ())) gvars;
    let new_ty = deref ty in
    (* restore the previous bindings of the free variables *)
    List.iter2 (fun gvar old_bind -> bind gvar old_bind) gvars old_bindings;
    new_ty

let close_var ty ~var =
  match var with
  | GVar (_,r) ->
    let old = !r in
    bind var (new_var ());
    let new_ty = deref ty in
    bind var old;  (* restore *)
    new_ty
  | _ -> raise (Invalid_argument "Type.close_var: expected a GVar as 2nd arg")

(* occur-check *)
let rec _occur_check gvar ty = match ty with
  | Var _ -> false
  | GVar _ when ty == gvar -> true
  | GVar (_, r) ->
    if !r == ty then false else _occur_check gvar !r  (* deref *)
  | App (_, l) -> List.exists (_occur_check gvar) l
  | Fun (ret, l) ->
    _occur_check gvar ret || List.exists (_occur_check gvar) l

(* unification *)
let rec unify_rec stack ty1 ty2 =
  let ty1 = deref ty1 in
  let ty2 = deref ty2 in
  match ty1, ty2 with
  | Var s1, Var s2 when s1 = s2 -> ()
  | GVar _, _ when not (_occur_check ty1 ty2) ->
    Stack.bind stack ty1 ty2
  | _, GVar _ when not (_occur_check ty2 ty1) ->
    Stack.bind stack ty2 ty1
  | App (s1, l1), App (s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
    List.iter2 (unify_rec stack) l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) when List.length l1 = List.length l2 ->
    unify_rec stack ret1 ret2;
    List.iter2 (unify_rec stack) l1 l2
  | _, _ ->
    let msg = Util.sprintf "unification error: %a and %a" pp ty1 pp ty2 in
    raise (Error msg)

(* alpha-equivalence check *)
let rec alpha_equiv_unify st ty1 ty2 =
  let ty1 = deref ty1 in
  let ty2 = deref ty2 in
  match ty1, ty2 with
  | Var s1, Var s2 when s1 = s2 -> ()
  | GVar _, GVar _ when not (_occur_check ty1 ty2) ->
    Stack.bind st ty1 ty2
  | App (s1, l1), App (s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
    List.iter2 (alpha_equiv_unify st) l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) when List.length l1 = List.length l2 ->
    alpha_equiv_unify st ret1 ret2;
    List.iter2 (alpha_equiv_unify st) l1 l2
  | _, _ -> raise (Error "not alpha equivalent")

let unify st ty1 ty2 =
  let pos = Stack.save st in
  try
    unify_rec st ty1 ty2;
    ()
  with e ->
    Stack.restore st pos;
    raise e

let alpha_equiv ty1 ty2 =
  let st = Stack.create () in
  let ty1 = instantiate ty1 in
  let ty2 = instantiate ty2 in
  try
    Stack.protect st
      (fun () -> alpha_equiv_unify st ty1 ty2; true)
  with Error _ ->
    false

let unifiable ty1 ty2 =
  let st = Stack.create () in
  let ty1 = instantiate ty1 in
  let ty2 = instantiate ty2 in
  try
    Stack.protect st
      (fun () -> unify_rec st ty1 ty2; true)
  with Error _ ->
    false
