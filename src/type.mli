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

(** Be careful that types may contain {!GVAr}s (destructively modifiable
    variables). Those variables may be pointers to other types, after
    unification, so you may want to use {!deref} often to be sure that
    you followed the pointers properly in the whole subtype.

    Types are hashconsed, excepted {!GVar}.
*)

type t = private
  | Var of string           (** Type variable, universally quantified *)
  | GVar of int * t ref     (** Variable instance. The int is unique *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

val eq : t -> t -> bool     (* syntactic equality *)
val cmp : t -> t -> int     (* syntactic comparison *)
val hash : t -> int         (* hash of the structure *)

(* TODO: a "view" of [t] that enforces some invariants (mostly
    that GVars are fully dereferenced); needs to make [t] abstract,
    provide a copy of it as [view], and a function [view : t -> view]
    that enforces invariants *)

exception Error of string
  (** Type error *)

val is_var : t -> bool
val is_gvar : t -> bool
val is_app : t -> bool
val is_fun : t -> bool

(** {2 Infix constructors} *)

module Infix : sig
  val (<==) : t -> t list -> t
    (** General function type *)

  val (<=.) : t -> t -> t
    (** Unary function type *)

  val (@@) : string -> t list -> t
    (** [s @@ args] applies the sort [s] to arguments [args]. *)
end

val var : string -> t
  (** Build a type variable. The integer must be >= 0 *)

val new_var : unit -> t
  (** Use a fresh name to make a fresh variable *)

val new_gvar : unit -> t
  (** New GVar, a free variable that can be instantiated once. It points to
      itself by default *)

val app : string -> t list -> t
  (** Parametrized type *)

val const : string -> t
  (** Constant sort *)

val mk_fun : t -> t list -> t
  (** Function type. The first argument is the return type *)

(** {2 Basic types} *)

val i : t       (* individuals *)
val o : t       (* propositions *)
val int : t     (* ints *)
val rat : t     (* rational numbers *)
val real : t    (* real numbers *)
val tType : t   (* "type" of types *)

(** {2 Utils} *)

(** All those functions but {!bind} follow pointers of {!GVar}s. *)

val free_vars : t -> t list
  (** List of free GVars *)

val bound_vars : t -> t list
  (** List of bound variables (Var) *)

val is_closed : t -> bool
  (** No GVar in this type? Corresponds to {! free_vars} returning
      an empty list *)

val deref : t -> t
  (** Replace all GVars by the type they point to (if any). *)

val bind : t -> t -> unit
  (** Bind the GVar to the given type.
      @raise Invalid_argument if the type is not a GVar *)

val arity : t -> int
  (** Number of arguments of the type (If it's a function, else 0)*)

val is_ground : t -> bool
  (** Is the type ground? *)

val curry : t -> t
  (** Curry the type *)

val uncurry : t -> t
  (** Uncurry the type. It {b must} be curried. *)

val size : t -> int
  (** Size of type, in number of "nodes" *)

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : t Bij.t
  (** Bijection. Note that GVar cannot be decoded nor encoded. Only
      closed types work. *)

val arbitrary : t QCheck.Arbitrary.t         (* closed types *)
val arbitrary_ground : t QCheck.Arbitrary.t  (* ground types *)

(** {2 Unification} *)

module Stack : sig
  type t
    (** A binding stack *)

  type pos
    (** Position in the stack *)

  val create : unit -> t
    (** New stack *)

  val clear : t -> unit
    (** Clear all bindings *)

  val bottom : pos
    (** Position of the empty stack *)

  val save : t -> pos
    (** Get current position *)

  val restore : t -> pos -> unit
    (** Reset to their old state the bindings between the current pos,
        and the one given (which must be valid) *)

  val restore_all : t -> unit
    (** Restore all bindings *)

  val bind : t -> ty -> ty -> unit
    (** Bind a GVar to a type, pushing its old binding on the stack. Next call
        to {! restore} with a position lower than the current one will
        restore the current binding of the GVar.
        @raise Invalid_argument if the second argument is not a GVar *)

  val protect : t -> (unit -> 'a) -> 'a
    (** [protect st f] will save the current position [pos] of [st],
        call [f ()], then call [restore st pos] whatever happens, and
        finally return the result of the call to [f]. *)

  val unwind_protect : t -> (unit -> 'a) -> 'a
    (** [unwind_protect st f] will save the current position [pos] of [st],
        and then call [f ()]. If the call succeeds, returns its result;
        otherwise call [restore st pos] and re-raise *)
end

val instantiate : t -> t
  (** Instantiate all bound variables with fresh GVars *)

val close : t -> t
  (** Call {!deref} on the type, then replace all free variables (GVar) by
      bound variables (Var). This corresponds to a generalization of the type,
      since variables are universally quantified. *)

val close_var : t -> unit
  (** [close_var var] replaces the GVar [var] by a fresh universal variable,
      if it's still not bound.
      @raise Invalid_argument if [var] is not a GVar *)

val unify : Stack.t -> t -> t -> unit
  (** Unify two types.
      @raise Error if the types are not unifiable (and restore bindings) *)

val alpha_equiv : t -> t -> bool
  (** Are the types alpha equivalent after instantiation?
      Does not change bindings. *)

val unifiable : t -> t -> bool
  (** Are the types unifiable after instantiation?
      Does not change bindings *)
