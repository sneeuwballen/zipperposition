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

val eq : t -> t -> bool
val cmp : t -> t -> int
val hash : t -> int

exception Error of string
  (** Type error *)

(** {2 Infix constructors} *)

module Infix : sig
  val (<==) : t -> t list -> t
    (** General function type *)

  val (<=.) : t -> t -> t
    (** Unary function type *)

  val (@@) : string -> t list -> t
    (** [s @@ args] applies the sort [s] to arguments [args]. *)
end

(** {2 Basic types} *)

val i : t
val o : t
val int : t
val rat : t
val real : t

val var : int -> t
  (** Build a type variable. The integer must be >= 0 *)

val app : string -> t list -> t
  (** Parametrized type *)

val const : string -> t
  (** Constant sort *)

val mk_fun : t -> t list -> t

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : t Bij.t

(** {2 Type unification} *)

module VarSet : Sequence.Set.S with type elt = int

module Subst : sig
  type t
  
  val empty : t
    (** Empty subst *)

  val bind : t -> int -> ty -> t
    (** Bind a variable *)

  val bind_seq : t -> (int * ty) Sequence.t -> t
    (** BInd several variables at once *)

  val apply : t -> ty -> ty
    (** Replace bound variables, recursively *)

  val apply_not_rec : t -> ty -> ty
    (** Replace bound variables, at most once *)
  
  val to_seq : t -> (int * ty) Sequence.t
  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

val free_vars : ?init:VarSet.t -> t -> VarSet.t
  (** List of free variables *)

val max_var : t -> int

val arity : t -> int
  (** Number of arguments of the type (If it's a function, else 0)*)

val is_ground : t -> bool
  (** Is the type ground? *)

val normalize : t -> t
  (** Rename variables from 0 to n-1, where n is the number of varibles
      in the type *)

val rename : int -> t -> t
  (** Shift all variables by the given offset *)

val unify : ?subst:Subst.t -> t -> t -> Subst.t
  (** Unify two types.
      @raise Error if the types are not unifiable *)

val alpha_equiv : t -> t -> bool
  (** Are the types alpha equivalent? *)
