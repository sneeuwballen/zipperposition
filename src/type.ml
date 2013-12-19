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

(** Base symbols for types *)
module type BASE_SYMBOLS = sig
  type t

  val arrow : t
  val forall_ty : t
  val i : t
  val o : t
  val tType : t
end

module type S = sig
  module T : ScopedTerm.S

  module Base : BASE_SYMBOLS with type t = T.Sym.t

  type symbol = T.Sym.t

  type t = T.t

  type ty = t

  type view = private
  | Var of int              (** Type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t             (** explicit quantification using De Bruijn index *)

  val view : t -> view
    (** Type-centric view of the head of this type.
        @raise Invalid_argument if the argument is not a type. *)

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  val is_var : t -> bool
  val is_bvar : t -> bool
  val is_app : t -> bool
  val is_fun : t -> bool
  val is_forall : t -> bool

  (** {2 Constructors} *)

  val (<==) : t -> t list -> t
    (** General function type. [x <== l] is the same as [x] if [l]
        is empty. Invariant: the return type is never a function type. *)

  val (<=.) : t -> t -> t
    (** Unary function type. [x <=. y] is the same as [x <== [y]]. *)

  val (@@) : string -> t list -> t
    (** [s @@ args] applies the sort [s] to arguments [args]. *)

  val var : int -> t
    (** Build a type variable. The integer must be >= 0 *)

  val app : string -> t list -> t
    (** Parametrized type *)

  val const : string -> t
    (** Constant sort *)

  val mk_fun : t -> t list -> t
    (** Function type. The first argument is the return type.
        see {!(<==)}. *)

  val forall : t list -> t -> t
    (** [forall vars ty] quantifies [ty] over [vars].
        If [vars] is the empty list, returns [ty].
        @raise Invalid_argument if some element of [vars] is not a variable *)

  val __forall : t -> t
    (** not documented. *)

  val of_term : T.t -> t option
    (** Check whether the given term is a valid representation of a type,
        in which case return it *)

  (** {2 Basic types} *)

  val i : t       (* individuals *)
  val o : t       (* propositions *)
  val tType : t   (* "type" of types *)

  (** {2 Utils} *)

  val free_vars_set : T.Set.t -> t -> T.Set.t
    (** Add the free variables to the given set *)

  val free_vars : t -> t list
    (** List of free variables ({!Var}) that are not bound *)

  val close_forall : t -> t
    (** bind free variables *)

  val arity : t -> int * int
    (** Number of arguments the type expects.
       If [arity ty] returns [a, b] that means that it
       expects [a] arguments to be used as arguments of Forall, and
       [b] arguments to be used for function application. *)

  val expected_args : t -> t list
    (** Types expected as function argument by [ty]. The length of the
        list [expected_args ty] is the same as [snd (arity ty)]. *)

  val is_ground : t -> bool
    (** Is the type ground? (means that no {!Var} not {!BVar} occurs in it) *)

  val size : t -> int
    (** Size of type, in number of "nodes" *)

  val apply : t -> t list -> t
    (** Given a function/forall type, and a list of arguments, return the
        type that results from applying the function/forall to the arguments.
        No unification is done, types must check exactly.
        @raise Error if the types do not match *)

  (** {2 IO} *)

  include Interfaces.PRINT with type t := t
  include Interfaces.SERIALIZABLE with type t := t

  (** {2 Misc} *)

  val __var : int -> t
    (** Escape hatch to generate fresh variables with negative indexes.
        Use at your own risk... *)
end

module TPTP(S : sig type t val of_string : string -> t end) = struct
  type t = S.t

  let arrow = S.of_string ">"
  let forall_ty = S.of_string "!>"
  let i = S.of_string "$i"
  let o = S.of_string "$o"
  let tType = S.of_string "$tType"
end

module Make(T : ScopedTerm.S)(Base : BASE_SYMBOLS with type t = T.Sym.t) = struct
  module T = T
  module Sym = T.Sym
  module Base = Base

  type symbol = Sym.t

  type t = T.t

  type ty = t

  type view =
  | Var of int              (** type variable *)
  | BVar of int             (** Bound variable (De Bruijn index) *)
  | App of symbol * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)
  | Forall of t             (** explicit quantification *)

  let view t = match T.view t with
    | T.Var i -> Var i
    | T.BVar i -> BVar i
    | T.Bind (s, t') when Sym.eq s Base.forall_ty ->
      Forall t'
    | T.App (head, ((t'::l') as l)) ->
      begin match T.view head with
      | T.Const s when Sym.eq s Base.arrow -> Fun (t', l')
      | T.Const s -> App (s, l)
      | _ -> raise (Invalid_argument "Type.view")
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

  let tType = T.const Base.tType
  let i = T.const ~ty:tType Base.i
  let o = T.const ~ty:tType Base.o

  let var i =
    if i < 0 then raise (Invalid_argument "Type.var");
    T.var ~ty:tType i

  let app s l = T.app ~ty:tType (T.const s) l

  let const s = T.const ~ty:tType s

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
      | _ -> T.app ~ty:tType (T.const Base.arrow) (ret :: args)

  let forall vars ty =
    T.bind_vars ~ty:tType Base.forall_ty vars ty

  let __forall ty = T.bind Base.forall_ty ty

  let (<==) = mk_fun
  let (<=.) ret a = mk_fun ret [a]
  let (@@) = app

  let free_vars_set set t = T.Seq.add_set set (T.Seq.vars t)

  let free_vars ty =
    T.Set.elements (free_vars_set T.Set.empty ty)

  let close_forall ty =
    let vars = free_vars ty in
    forall vars ty

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

  let rec pp_rec depth buf t = match view t with
    | Var i -> Printf.bprintf buf "T%d" i
    | BVar i -> Printf.bprintf buf "Tb%i" (depth-i-1)
    | App (p, []) -> Buffer.add_string buf (Sym.to_string p)
    | App (p, args) ->
      Printf.bprintf buf "%s(%a)" (Sym.to_string p) (Util.pp_list (pp_rec depth)) args
    | Fun (ret, []) -> assert false
    | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
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

  let rec pp_tstp_rec depth buf t = match view t with
    | Var i -> Printf.bprintf buf "T%d" i
    | BVar i -> Printf.bprintf buf "Tb%d" (depth-i-1)
    | App (p, []) -> Buffer.add_string buf (Sym.to_string p)
    | App (p, args) ->
      Printf.bprintf buf "%s(%a)" (Sym.to_string p) (Util.pp_list (pp_tstp_rec depth)) args
    | Fun (ret, []) -> assert false
    | Fun (ret, [arg]) -> Printf.bprintf buf "%a > %a" (pp_inner depth) arg (pp_inner depth) ret
    | Fun (ret, l) ->
      Printf.bprintf buf "(%a) > %a"
        (Util.pp_list ~sep:" * " (pp_inner depth)) l (pp_tstp_rec depth) ret
    | Forall ty' ->
      Printf.bprintf buf "!>[Tb%d:$tType]: %a" depth (pp_inner (depth+1)) ty'
  and pp_inner depth buf t = match view t with
    | Fun (_, _::_) ->
      Buffer.add_char buf '('; pp_tstp_rec depth buf t; Buffer.add_char buf ')'
    | _ -> pp_tstp_rec depth buf t

  let pp_tstp buf t = pp_tstp_rec 0 buf t

  let to_string t =
    let b = Buffer.create 15 in
    pp b t;
    Buffer.contents b

  let fmt fmt t = Format.pp_print_string fmt (to_string t)


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

  let bij = T.bij

  let __var =
    let r = ref ~-1 in
    fun () ->
      let n = !r in
      decr r;
      T.var ~ty:tType n
end

module TPTP(S : sig type t val of_string : string -> t end) = struct
  type t = S.t
end

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
*)
