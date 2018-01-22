
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for "Enum Types"} *)

open Logtk
open Libzipperposition

type term = Term.t

(** {2 Inference rules} *)

(** An Enum Type is a possibly parametrized type
    [c : type -> type -> ... -> type] with an exhaustiveness axiom
    [forall a1....an:type,
       forall x:(c a1...an),
         x = t_1 or x = t_2 or ... or x = t_m]
    where the [t_i] are non-variable terms that contain
      only [x] as a variable.


    This calculus is designed to remove the axiom (which is very prolific
    in superposition) and do its job more efficiently.

    Inductive types (algebraic types) belong to this category of types,
    and have additional axioms that are dealt with elsewhere.

    We require that the type is as general as possible: either
    a constant, or a polymorphic type that has only type variables as
    arguments. Enum types for things like [list(int)] would be dangerous
    because if we remove the axiom, because instantiation is a
    simplification, we won't deal properly with [list(rat)] (no unification
    whatsoever)

    The rules are:


    instantiation of variables:

       C[x] where x:tau unshielded     enum(tau, x in t1....tm)
    -----------------------------------------------------------
                   C[t_1] or ... or C[tm]

    specialization of exhaustiveness for f:

       f : a1 -> ... -> ak -> tau      enum(tau, x in t1....tm)
    ------------------------------------------------------------
        forall x1:a1, ... xk:ak,
           f(x1...xk)=t1\sigma or .... or f(x1...xk) = tm\sigma
    where sigma = {x -> f(x1...xk)}

*)

exception Error of string

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  type decl

  val pp_decl : decl CCFormat.printer

  type declare_result =
    | New of decl
    | AlreadyDeclared of decl

  val declare_ty :
    proof:Proof.t ->
    ty_id:ID.t ->
    ty_vars:Type.t HVar.t list ->
    var:Type.t HVar.t ->
    term list ->
    declare_result
  (** Declare that the domain of the type [ty_id] is restricted to
      given list of [cases], in the form [forall var. Or_{c in cases} var = c].
      The type of [var] must be [ty_id ty_vars].
      Will be ignored if the type already has a enum declaration, and the old
      declaration will be returned instead.
      @return either the new declaration, or the already existing one for
        this type if any
      @raise Error if some of the preconditions is not filled *)

  val instantiate_vars : Env.multi_simpl_rule
  (** Instantiate variables whose type is a known enumerated type,
      with all cases of this type. *)

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

(** {2 As Extension} *)

val extension : Extensions.t
