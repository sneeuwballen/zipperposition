
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

(** {1 Higher Order Terms} *)

module T = ScopedTerm
module FOT = FOTerm

(** {2 Type Definitions} *)

type symbol = ID.t

type t = ScopedTerm.t

type term = t

type view =
  | AppBuiltin of Builtin.t * t list
  | Var of t HVar.t (** variable *)
  | DB of int (** bound variable (De Bruijn index) *)
  | Lambda of Type.t * t (** lambda abstraction over one variable. *)
  | Forall of Type.t * t (** Forall quantifier (commutes with other forall) *)
  | Exists of Type.t * t (** Exists quantifier (commutes with other exists) *)
  | Const of ID.t (** Typed constant *)
  | App of t * t list (** curried application *)
  | Multiset of Type.t * t list (** a multiset of terms, and their common type *)
  | Record of (string*t) list * t HVar.t option (** Record of terms *)

let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_unsafe ty

let __get_ty = ty

let view t = match T.view t with
  | T.Var v -> Var v
  | T.DB i -> DB i
  | T.Bind (Binder.Lambda, varty, t') ->
      Lambda (Type.of_term_unsafe varty, t')
  | T.Bind (Binder.Forall, varty, t') ->
      Forall (Type.of_term_unsafe varty, t')
  | T.Bind (Binder.Exists, varty, t') ->
      Exists (Type.of_term_unsafe varty, t')
  | T.Const s -> Const s
  | T.Multiset l ->
      begin match Type.view (ty t) with
        | Type.Multiset tau -> Multiset (tau, l)
        | _ -> assert false
      end
  | T.App (f,l) -> App(f,l)
  | T.Record (l, rest) -> Record (l, rest)
  | T.AppBuiltin (b, l) -> AppBuiltin (b,l)
  | T.Bind _
  | T.SimpleApp _ -> assert false

let of_term_unsafe t = t

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    T.equal sub t ||
    match T.view t with
    | T.Var _ | T.DB _ | T.App (_, []) -> false
    | T.App (_, args) -> List.exists check args
    | _ -> false
  in
  check t

let equal = T.equal
let compare = T.compare
let hash_fun = T.hash_fun
let hash = T.hash

module TermHASH = struct
  type t = term
  let equal = equal
  let hash = hash
end

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Cache = Cache.Replacing(TermHASH)

(** {2 Typing} *)

let cast ~ty t = T.cast ~ty:(ty : Type.t :> T.t) t

let lambda_var_ty t = match T.view t with
  | T.Bind (Binder.Lambda, varty, _) ->
      Type.of_term_unsafe varty
  | _ -> invalid_arg "lambda_var_ty: expected lambda term"

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let var = T.var

let var_of_int ~ty i =
  T.var (HVar.make i ~ty:(ty : Type.t :> T.t))

let bvar ~(ty:Type.t) i =
  T.bvar ~ty:(ty :> T.t) i

let app f l = match l with
  | [] -> f
  | _::_ ->
      (* first; compute type *)
      let ty_result = Type.apply_unsafe (ty f) l in
      (* apply constant to type args and args *)
      T.app ~ty:(ty_result : Type.t :> T.t) f l

let app_ty t args = match args with
  | [] -> t
  | _::_ ->
      let args' = (args : Type.t list :> T.t list) in
      let ty = (Type.apply (ty t) args : Type.t :> T.t) in
      T.app ~ty t args'

let app_full f tyargs l =
  let l = (tyargs : Type.t list :> T.t list) @ l in
  app f l

let const ~ty symbol =
  T.const ~ty:(ty : Type.t :> T.t) symbol

let builtin ~ty b =
  T.builtin ~ty:(ty : Type.t :> T.t) b

let app_builtin ~ty b l =
  T.app_builtin ~ty:(ty : Type.t :> T.t) b l

let multiset ~ty l =
  if List.exists (fun t -> not (Type.equal ty (__get_ty t))) l
  then raise (Type.ApplyError "type mismatch when building a multiset");
  (* all elements are of type [ty], the result has type [multiset ty] *)
  let ty_res = Type.multiset ty in
  T.multiset ~ty:(ty_res:>T.t) l

let record l ~rest =
  (* build record type! *)
  let ty_l = List.map (fun (n,t) -> n, ty t) l in
  let ty_rest = match rest with
    | None -> None
    | Some v ->
        let ty_v = Type.of_term_unsafe (HVar.ty v) in
        begin match Type.view ty_v with
          | Type.Record _ -> Some ty_v
          | _ ->
              raise (Type.ApplyError "the type of a row in a record must be a record type")
        end
  in
  let ty = Type.record_flatten ty_l ~rest:ty_rest in
  (* flattening done by ScopedTerm. *)
  T.record ~ty:(ty:>T.t) l ~rest

let lambda ~varty t' =
  let ty = Type.arrow [varty] (ty t') in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.lambda t'

let forall ~varty t' =
  let ty = ty t' in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.forall t'

let exists ~varty t' =
  let ty = ty t' in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.exists t'

let mk_binder_l f vars t =
  List.fold_right
    (fun v t ->
       let t' = T.DB.replace (T.DB.shift 1 t) ~sub:v in
       f ~varty:(ty v) t')
    vars t

let is_var t = match T.view t with | T.Var _ -> true | _ -> false
let is_bvar t = match T.view t with | T.DB _ -> true | _ -> false
let is_const t = match T.view t with | T.Const _ -> true | _ -> false
let is_app t = match view t with | App _ -> true | _ -> false
let is_lambda t = match view t with | Lambda _ -> true | _ -> false
let is_forall t = match view t with | Forall _ -> true | _ -> false
let is_exists t = match view t with | Exists _ -> true | _ -> false
let is_multiset t = match T.view t with | T.Multiset _ -> true | _ -> false
let is_record t = match T.view t with | T.Record _ -> true | _ -> false

module VarSet = T.VarSet
module VarMap = T.VarMap
module VarTbl = T.VarTbl

(** {2 Sequences} *)

module Seq = struct
  let subterms t = T.Seq.subterms t
  let vars t = T.Seq.vars t
  let subterms_depth = T.Seq.subterms_depth
  let symbols t = T.Seq.symbols t
  let max_var = T.Seq.max_var
  let min_var = T.Seq.min_var
  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> Type.Seq.vars (ty t))
  let add_set set ts =
    Sequence.fold (fun set t -> Set.add t set) set ts
end

(** {2 Subterms and positions} *)

module Pos = struct
  let at = T.Pos.at
  let replace = T.Pos.replace
end

let replace = T.replace

let rec size t = match view t with
  | Var _
  | DB _ -> 1
  | Const _ -> 1
  | Forall (_, t')
  | Exists (_, t')
  | Lambda (_, t') -> 1 + size t'
  | App (f, l) -> 1 + size f + size_l l
  | Record (l, rest) ->
      let s = match rest with None -> 0 | Some _ -> 1 in
      List.fold_left (fun acc (_,t') -> acc+size t') s l
  | AppBuiltin (_,l)
  | Multiset (_,l) -> 1 + size_l l
and size_l l = List.fold_left (fun acc t->acc + size t) 0 l

let is_ground t = Seq.vars t |> Sequence.is_empty

let var_occurs ~var t =
  Seq.vars t |> Sequence.exists (HVar.equal var)

let monomorphic t = Seq.ty_vars t |> Sequence.is_empty

let max_var set = VarSet.to_seq set |> Seq.max_var

let min_var set = VarSet.to_seq set |> T.Seq.min_var

let add_vars tbl t = Seq.vars t (fun x -> VarTbl.replace tbl x ())

let vars ts = Sequence.flat_map Seq.vars ts |> VarSet.of_seq

let vars_prefix_order t =
  Seq.vars t
  |> Sequence.fold (fun l x -> if not (List.memq x l) then x::l else l) []
  |> List.rev

let ty_vars t =
  Seq.ty_vars t |> Type.VarSet.of_seq

let depth t = Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head_exn t = match view t with
  | Const s -> s
  | App (f,_) -> head_exn f
  | AppBuiltin _
  | DB _
  | Var _
  | Forall _
  | Exists _
  | Lambda _
  | Multiset _
  | Record _ -> raise Not_found

let head t = try Some (head_exn t) with Not_found -> None

(** {2 High-level operations} *)

let symbols ?(init=ID.Set.empty) t =
  Seq.symbols t |> ID.Set.add_seq init

let contains_symbol s t =
  Seq.symbols t |> Sequence.exists (ID.equal s)

(** {2 FO conversion} *)

(* Curry all subterms *)
let rec of_fo t =
  let ty = FOT.ty t in
  match FOT.view t with
  | FOT.Var v -> var (HVar.update_ty v ~f:of_fo)
  | FOT.DB i -> bvar ~ty i
  | FOT.Const s -> const ~ty s
  | FOT.App (f, l) -> app (of_fo f) (List.map of_fo l)
  | FOT.AppBuiltin (b,l) -> app (builtin ~ty b) (List.map of_fo l)

let to_fo t =
  let rec aux t =
    let ty = ty t in
    match view t with
    | AppBuiltin (b,l) -> FOT.app_builtin ~ty b (List.map aux l)
    | Var v -> FOT.var (HVar.update_ty v ~f:aux)
    | DB i -> FOT.bvar ~ty i
    | App (f,l) ->
        let f = aux f in
        let l = List.map aux l in
        FOT.app f l
    | Const s -> FOT.const ~ty s
    | Forall _
    | Exists _
    | Lambda _
    | Record _
    | Multiset _ -> raise Exit
  in try Some (aux t)
  with Exit -> None

let rec is_fo t = match view t with
  | AppBuiltin _
  | Var _
  | DB _ -> true
  | App (f,l) ->
      is_fo f && List.for_all is_fo l
  | Const _ -> true
  | Record _
  | Multiset _
  | Forall _
  | Exists _
  | Lambda _ -> false

(** {2 Various operations} *)

let prop = T.builtin ~ty:T.tType Builtin.prop

let close_forall t =
  let vars = Seq.vars t |> T.VarSet.of_seq |> T.VarSet.elements in
  T.bind_vars ~ty:prop Binder.Forall vars t

let close_exists t =
  let vars = Seq.vars t |> T.VarSet.of_seq |> T.VarSet.elements in
  T.bind_vars ~ty:prop Binder.Forall vars t

let open_forall ?(offset=0) f =
  let offset = max offset (Seq.max_var (Seq.vars f)) + 1 in
  (* open next forall, replacing it with a fresh var *)
  let rec open_one offset env f = match view f with
    | Forall (varty,f') ->
        let v = var_of_int ~ty:varty offset in
        let env' = DBEnv.push env v in
        open_one (offset+1) env' f'
    | _ ->
        of_term_unsafe (T.DB.eval env f)  (* replace *)
  in
  open_one offset DBEnv.empty f

(** {2 IO} *)

let print_all_types = ref false

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

let binder_to_str t = match view t with
  | Lambda _ -> "λ"
  | Forall _ -> "∀"
  | Exists _ -> "∃"
  | _ -> assert false

let pp_depth = T.pp_depth

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks

let pp buf t = pp_depth ~hooks:!__hooks 0 buf t

let to_string = CCFormat.to_string pp

let debugf = T.debugf

module TPTP = struct
  let true_ = builtin ~ty:Type.TPTP.o Builtin.true_
  let false_ = builtin ~ty:Type.TPTP.o Builtin.false_

  (** Easy constructors for formulas *)

  let builtin_ b =
    let ty = Signature.Builtin.ty_exn b in
    builtin ~ty b

  let not_ = builtin_ Builtin.not_
  let and_ = builtin_ Builtin.and_
  let or_ = builtin_ Builtin.or_
  let imply = builtin_ Builtin.imply
  let equiv = builtin_ Builtin.equiv
  let xor = builtin_ Builtin.xor

  let eq = builtin_ Builtin.eq
  let neq = builtin_ Builtin.neq

  let forall = builtin_ Builtin.ForallConst
  let exists = builtin_ Builtin.ExistsConst

  let mk_not t = app not_ [t]
  let mk_and a b = app and_ [a; b]
  let mk_or a b = app or_ [a; b]
  let mk_imply a b = app imply [a; b]
  let mk_equiv a b = app equiv [a; b]
  let mk_xor a b = app xor [a; b]
  let mk_eq a b = app eq [(ty a : Type.t :> T.t); a; b]   (* use type of left arg *)
  let mk_neq a b = app neq [(ty a : Type.t :> T.t); a; b]

  let rec mk_and_list l = match l with
    | [] -> true_
    | [x] -> x
    | x::l' -> mk_and x (mk_and_list l')

  let rec mk_or_list l = match l with
    | [] -> false_
    | [x] -> x
    | x::l' -> mk_or x (mk_or_list l')

  let tptp_binder_to_str t = match view t with
    | Lambda _ -> "^"
    | Forall _ -> "!"
    | Exists _ -> "?"
    | _ -> assert false

  let pp_depth ?hooks:_ depth out t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec out t = match view t with
      | DB i ->
          Format.fprintf out "Y%d" (!depth - i - 1);
          (* print type of term *)
          if !print_all_types || not (Type.equal (ty t) Type.TPTP.i)
          then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
      | AppBuiltin (b,[]) -> Builtin.TPTP.pp out b
      | AppBuiltin (b,l) ->
          Format.fprintf out "(@[<2>%a@ %a@])" Builtin.TPTP.pp b (Util.pp_list pp_rec) l
      | Const s -> ID.pp out s
      | App (f, l) ->
          Format.fprintf out "@[<hv2>%a(@,%a)@]" pp_rec f
            (Util.pp_list ~sep:", " pp_rec) l
      | Var i ->
          Format.fprintf out "X%d" (HVar.id i);
          (* print type of term *)
          if !print_all_types || not (Type.equal (ty t) Type.TPTP.i)
          then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
      | Lambda (varty,t') | Forall (varty,t') | Exists (varty,t') ->
          Format.fprintf out "%s[%a:%a]: " (tptp_binder_to_str t)
            pp_bvar () Type.pp varty;
          incr depth;
          pp_surrounded out t';
          decr depth
      | Multiset _ -> failwith "cannot print multiset in TPTP"
      | Record _ -> failwith "cannot print records in TPTP"
    and pp_surrounded out t = match view t with
      | App _ | Lambda _ | AppBuiltin (_, _::_) ->
          Format.fprintf out "(@[%a@])" pp_rec t
      | _ -> pp_rec out t
    and pp_bvar out () =  Format.fprintf out "Y%d" !depth in
    pp_rec out t

  let pp buf t = pp_depth 0 buf t

  let to_string = CCFormat.to_string pp
end
