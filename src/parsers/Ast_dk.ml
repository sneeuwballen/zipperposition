
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 AST utils for dedukti} *)

open Logtk

module A = UntypedAST
module T = STerm

include T

type statement = UntypedAST.statement
type ty = T.t

let cast t ty : term = T.app_builtin Builtin.has_type [t;ty]
let const s = T.const s
let mk_const s ty : term = cast (T.const s) ty
let mk_const_t s = mk_const s T.tType
let mk_var v = V v

(* Global list of type aliases *)
let ty_aliases : (string, ty) Hashtbl.t = Hashtbl.create 16

let find_alias ~or_else(s:string) : ty =
  try Hashtbl.find ty_aliases s
  with Not_found -> or_else

let add_alias s ty : unit =
  Util.debugf 2 "Registering alias %s := %a" (fun k->k s T.pp ty);
  Hashtbl.replace ty_aliases s ty

(* Needed for parsing number literals *)
let type_nat = mk_const_t "dk_nat.nat"

exception Unknown_builtin of string
exception Bad_arity of string * int

exception Application_head_is_not_a_var of term

let mk_app f l = T.app f l
let mk_app_const f l = T.app (T.const f) l
let mk_arrow_l a b = T.fun_ty a b
let mk_arrow a b = mk_arrow_l [a] b
let mk_fun l t = T.lambda l t
let mk_forall l t = T.forall l t
let mk_int x = T.int_ (Z.of_string x)
let ty_prop = T.prop

let mk_ty_decl id ty = A.decl id ty
let mk_assert ~name t = A.assert_ ~attrs:[A.attr_name name] t
let mk_goal ~name t = A.goal ~attrs:[A.attr_name name] t
let mk_def id ty body = A.def [A.mk_def id ty [body]]
let mk_rewrite t = A.rewrite t

