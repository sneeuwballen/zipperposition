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

(** {2 Main Type representation}

Common representation of types, including higher-order
and polymorphic types. Types are hashconsed and all type variables
are assumed to be universally quantified in the outermost possible
scope (outside any other quantifier).

See {!TypeInference} for inferring types from terms and formulas,
and {!Signature} to associate types with symbols.
*)

type t = private
  | Var of int              (** Type variable, universally quantified *)
  | App of string * t list  (** parametrized type *)
  | Fun of t * t list       (** Function type *)

type ty = t

val eq : t -> t -> bool     (* syntactic equality *)
val cmp : t -> t -> int     (* syntactic comparison *)
val hash : t -> int         (* hash of the structure *)

val is_var : t -> bool
val is_app : t -> bool
val is_fun : t -> bool

module Tbl : Hashtbl.S with type key = ty
module Set : Sequence.Set.S with type elt = ty

(** {2 Infix constructors} *)

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

(** {2 Basic types} *)

val i : t       (* individuals *)
val o : t       (* propositions *)
val int : t     (* ints *)
val rat : t     (* rational numbers *)
val real : t    (* real numbers *)
val tType : t   (* "type" of types *)

(** {2 Utils} *)

val free_vars : t -> t list
  (** List of free variables ({!Var}) that are not bound *)

val arity : t -> int
  (** Number of arguments of the type (If it's a function, else 0)*)

val is_ground : t -> bool
  (** Is the type ground? (means that no {!Var} occur in it) *)

val curry : t -> t
  (** Curry the type *)

val uncurry : t -> t
  (** Uncurry the type. It {b must} be curried.
      @raise Failure if the type is not fully curried. *)

val size : t -> int
  (** Size of type, in number of "nodes" *)

val apply_fun : t -> t list -> t
  (** Given a function type, and a list of arguments, return the
      type that results from applying the function to the arguments.
      No unification is done, types must check exactly.
      @raise Failure if the types do not match *)

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : t Bij.t
  (** Bijection. Note that GVar cannot be decoded nor encoded. Only
      closed types work. *)

(** {2 Parsed types}
This module exports a very simple representation of types, typically
obtained right after parsing. No hashconsing is performed,
and variables are still strings.
*)

module Parsed : sig
  type t = private
    | Var of string
    | App of string * t list
    | Fun of t * t list

  val eq : t -> t -> bool
  val cmp : t -> t -> int
  val hash : t -> int

  val var : string -> t
  val app : string -> t list -> t
  val const : string -> t
  val mk_fun : t -> t list -> t
  val (<==) : t -> t list -> t
  val (<=.) : t -> t -> t

  val i : t
  val o : t
  val int : t
  val rat : t
  val real : t
  val tType : t

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

type ctx = (string, t) Hashtbl.t

val create_ctx : unit -> ctx

val of_parsed : ?ctx:ctx -> Parsed.t -> t
  (** Conversion from a {!Parsed.t} term representation.
      An optional {!ctx} can be used to map named variables
      to {!Var}s. *)

val to_parsed : t -> Parsed.t
  (** Convert back to "parsed" raw type *)

(** {2 Misc} *)

val __var : int -> t
  (** Escape hatch to generate fresh variables with negative indexes.
      Use at your own risk... *)
