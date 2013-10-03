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
  | Var of string           (** Universal type variable *)
  | GVar of int * t ref     (** Variable instance. The int is unique *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

let rec eq_struct t1 t2 = match t1, t2 with
  | Var s1, Var s2 -> s1 = s2
  | GVar (i1,r1), GVar (i2,r2) -> i1 = i2 && r1 == r2
  | App (s1, l1), App (s2, l2) when List.length l1 = List.length l2 ->
    s1 = s2 && List.for_all2 (==) l1 l2
  | Fun (ret1,l1), Fun (ret2,l2) ->
    ret1 == ret2 && List.for_all2 (==) l1 l2
  | _, _ -> false

let rec hash t = match t with
  | Var s -> Hash.hash_string s
  | GVar (i, _) -> Hash.hash_int i
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

let cmp t1 t2 = Pervasives.compare t1 t2

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
    let s = Printf.sprintf "Ty_%d" !n in
    H.hashcons (Var s)

let new_gvar =
  let n = ref 0 in
  let __default = H.hashcons (Var "") in  (* some default type *)
  fun () ->
    incr n;
    (* No need to hashcons, this value is unforgeable *)
    let ty = GVar (!n, ref __default) in
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
let tType = const "$tType"

(** {2 Utils} *)

let rec _deref_var ty = match ty with
  | GVar (_, r) ->
    if !r == ty
      then ty  (* points to self, not bound *)
      else _deref_var !r
  | _ -> ty

let free_vars ty =
  let rec recurse acc ty = match _deref_var ty with
  | Var _ -> acc
  | GVar _ -> if List.memq ty acc then acc else ty :: acc
  | App (_, l) -> List.fold_left recurse acc l
  | Fun (ret, l) -> List.fold_left recurse (recurse acc ret) l
  in
  recurse [] ty

let bound_vars ty =
  let rec recurse acc ty = match _deref_var ty with
  | Var _ -> if List.memq ty acc then acc else ty :: acc
  | GVar _ -> acc
  | App (_, l) -> List.fold_left recurse acc l
  | Fun (ret, l) -> List.fold_left recurse (recurse acc ret) l
  in
  recurse [] ty

let rec is_closed ty = match _deref_var ty with
  | Var _ -> true
  | GVar _ -> false
  | App (_, l) -> List.for_all is_closed l
  | Fun (ret, l) -> is_closed ret && List.for_all is_closed l

let rec deref ty = match _deref_var ty with
  | Var _ -> ty
  | GVar (_, r) -> ty
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

let rec is_ground t = match _deref_var t with
  | Var _ -> false
  | GVar _ -> false
  | App (_, l) -> List.for_all is_ground l
  | Fun (ret, l) -> is_ground ret && List.for_all is_ground l

let rec curry ty = match _deref_var ty with
  | Var _
  | GVar _ -> ty
  | App (s, l) -> app s (List.map curry l)
  | Fun (ret, l) ->
    List.fold_left 
      (fun ret arg -> mk_fun ret [curry arg])
      (curry ret) l

let rec uncurry ty = match _deref_var ty with
  | Var _
  | GVar _ -> ty
  | App (s, l) -> app s (List.map uncurry l)
  | Fun _ ->
    begin match _gather_uncurry ty [] with
    | [] -> assert false
    | ret::args -> mk_fun (uncurry ret) args
    end 
(* given a curried function type, recover all its argument types into
    a list prepended to [acc] *)
and _gather_uncurry ty acc = match _deref_var ty with
  | Var _
  | GVar _
  | App _ -> uncurry ty :: acc  (* proper return value *)
  | Fun (ret, [arg]) -> _gather_uncurry ret (uncurry arg :: acc)
  | Fun _ -> uncurry ty :: acc (* consider this as a single argument *)

(** {2 IO} *)

let rec pp buf t = match _deref_var t with
  | Var s -> Printf.bprintf buf "'%s" s
  | GVar (i, _) -> Printf.bprintf buf "'_%d" i
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s %a " p (Util.pp_list ~sep:" " pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a -> %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) -> %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
and pp_inner buf t = match t with
  | Fun (_, _::_) ->
    Buffer.add_char buf '('; pp buf t; Buffer.add_char buf ')'
  | _ -> pp buf t

let rec pp_tstp buf t = match _deref_var t with
  | Var s -> Printf.bprintf buf "%s" (String.capitalize s)
  | GVar (i, _) -> failwith "Type.pp_tstp: free variables not printable"
  | App (p, []) -> Buffer.add_string buf p
  | App (p, args) -> Printf.bprintf buf "%s(%a)" p (Util.pp_list pp) args
  | Fun (ret, []) -> assert false
  | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" pp_inner arg pp_inner ret
  | Fun (ret, l) ->
    Printf.bprintf buf "(%a) > %a" (Util.pp_list ~sep:" * " pp_inner) l pp ret
and pp_inner buf t = match _deref_var t with
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
      ~inject:(fun ty -> match _deref_var ty with
        | Var s -> "var", BranchTo (string_, s)
        | GVar _ -> failwith "Type.bij: GVar not supported"
        | App (p, l) -> "app", BranchTo (Lazy.force bij_app, (p, l))
        | Fun (ret, l) -> "fun", BranchTo (Lazy.force bij_fun, (ret, l)))
      ~extract:(function
        | "var" -> BranchFrom (string_, var)
        | "app" -> BranchFrom (Lazy.force bij_app, fun (s,l) -> app s l)
        | "fun" -> BranchFrom (Lazy.force bij_fun, fun (ret,l) -> mk_fun ret l)
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

  let bind st gvar ty =
    Stack.push gvar st.gvars;
    Stack.push (_binding gvar) st.bindings;
    st.size <- st.size + 1;
    bind gvar ty;
    ()
end

(* instantiate all bound variables *)
let instantiate ty =
  (* recurse. [map] is a hashtable name -> gvar *)
  let rec find_and_bound map ty = match ty with
  | Var n ->
    (* see whether we already instantiated this var *)
    begin try Hashtbl.find map n
    with Not_found ->
      let v = new_gvar () in
      Hashtbl.add map n v;
      v
    end
  | GVar _ -> ty
  | App (s, l) -> App (s, List.map (find_and_bound map) l)
  | Fun (ret, l) -> Fun (find_and_bound map ret, List.map (find_and_bound map) l)
  in
  find_and_bound (Hashtbl.create 4) ty

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

let close_var var =
  match _deref_var var with
  | GVar (_,r) as var ->
    (* still not bound, bind it to a fresh var *)
    let v = new_var () in
    bind var v
  | _ -> ()

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
  let ty1 = _deref_var ty1 in
  let ty2 = _deref_var ty2 in
  match ty1, ty2 with
  | Var s1, Var s2 when s1 = s2 -> ()
  | GVar _, GVar _ when ty1 == ty2 -> ()
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
  let ty1 = _deref_var ty1 in
  let ty2 = _deref_var ty2 in
  match ty1, ty2 with
  | Var s1, Var s2 when s1 = s2 -> ()
  | GVar _, GVar _ when ty1 == ty2 -> ()
  | GVar _, GVar _ when not (_occur_check ty1 ty2) ->
    Stack.bind st ty1 ty2
  | App (s1, l1), App (s2, l2) when s1 = s2 && List.length l1 = List.length l2 ->
    List.iter2 (alpha_equiv_unify st) l1 l2
  | Fun (ret1, l1), Fun (ret2, l2) when List.length l1 = List.length l2 ->
    alpha_equiv_unify st ret1 ret2;
    List.iter2 (alpha_equiv_unify st) l1 l2
  | _, _ -> raise (Error "not alpha equivalent")

let unify st ty1 ty2 =
  Util.enter_prof prof_unify;
  let pos = Stack.save st in
  try
    unify_rec st ty1 ty2;
    Util.exit_prof prof_unify;
    ()
  with e ->
    Stack.restore st pos;
    Util.exit_prof prof_unify;
    raise e

let alpha_equiv ty1 ty2 =
  Util.enter_prof prof_variant;
  let st = Stack.create () in
  let ty1 = instantiate ty1 in
  let ty2 = instantiate ty2 in
  try
    let res = Stack.protect st
      (fun () -> alpha_equiv_unify st ty1 ty2; true)
    in
    Util.exit_prof prof_variant;
    res
  with Error _ ->
    Util.exit_prof prof_variant;
    false

let unifiable ty1 ty2 =
  Util.enter_prof prof_unify;
  let st = Stack.create () in
  let ty1 = instantiate ty1 in
  let ty2 = instantiate ty2 in
  try
    let res = Stack.protect st
      (fun () -> unify_rec st ty1 ty2; true)
    in
    Util.exit_prof prof_unify;
    res
  with Error _ ->
    Util.exit_prof prof_unify;
    false
