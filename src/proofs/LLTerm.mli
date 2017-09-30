
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms For Proofs} *)

open Logtk

type t

type var = t HVar.t

type view =
  | Type
  | Const of ID.t
  | App of t * t list
  | Var of var (** bound var *)
  | Bind of {
      binder: Binder.t;
      ty_var: t;
      body: t;
    }
  | AppBuiltin of Builtin.t * t list

val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int

val ty : t -> t option
val ty_exn : t -> t

val t_type : t
val var : var -> t
val const : ty:t -> ID.t -> t
val app : ty:t -> t -> t list -> t
val bind : ty:t -> Binder.t -> ty_var:t -> t -> t
val app_builtin : ty:t -> Builtin.t -> t list -> t
val builtin : ty:t -> Builtin.t -> t
