
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

(** {1 S-like Terms}.

Those terms are not hashconsed, nor do they use De Bruijn indices. Their
simplicity make them good for heavy AST transformations, output of parsing,
etc.

Terms are only compared, hashsed, etc. by their "term" component (the algebraic
variant). Additional fields (location...) are ignored for almost every
operation.
*)

type location = ParseLocation.t

type t = private {
  term : view;
  loc : location option;
}
and view = private
  | Var of string                   (** variable *)
  | Int of Z.t                      (** integer *)
  | Rat of Q.t                      (** rational *)
  | Const of Symbol.t               (** constant *)
  | AppBuiltin of Builtin.t * t list
  | App of t * t list               (** apply term *)
  | Bind of Binder.t * t list * t   (** bind n variables *)
  | List of t list                  (** special constructor for lists *)
  | Record of (string * t) list * t option  (** extensible record *)
  | Column of t * t                 (** t:t (useful for typing, e.g.) *)

type term = t

val view : t -> view
val loc : t -> location option

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val var : ?loc:location -> ?ty:t -> string -> t
val int_ : Z.t -> t
val of_int : int -> t
val rat : Q.t -> t
val app : ?loc:location -> t -> t list -> t
val builtin : ?loc:location -> Builtin.t -> t
val app_builtin : ?loc:location -> Builtin.t -> t list -> t
val const : ?loc:location -> Symbol.t -> t
val bind : ?loc:location -> Binder.t -> t list -> t -> t
val list_ : ?loc:location -> t list -> t
val nil : t
val column : ?loc:location -> t -> t -> t
val record : ?loc:location -> (string*t) list -> rest:t option -> t
val at_loc : loc:location -> t -> t

val wildcard : t

val is_app : t -> bool
val is_var : t -> bool

module Set : Sequence.Set.S with type elt = term
module Map : Sequence.Map.S with type key = term
module Tbl : Hashtbl.S with type key = term

module Seq : sig
  val vars : t -> t Sequence.t
  val free_vars : t -> t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_with_bound : t -> (t * Set.t) Sequence.t
    (** subterm and variables bound at this subterm *)

  val symbols : t -> Symbol.t Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
end

val ground : t -> bool
val close_all : Binder.t -> t -> t  (** Bind all free vars with the symbol *)
val subterm : strict:bool -> t -> sub:t -> bool
  (** is [sub] a (strict?) subterm of the other arg? *)

include Interfaces.PRINT with type t := t

(** {2 TPTP constructors and printing}

The constructors take the semantics of TPTP into consideration. In
particular, wildcard (fresh variables) are inserted in front of
ad-hoc polymorphic symbols such as [eq] or [neq]. *)
module TPTP : sig
  val true_ : t
  val false_ : t

  val var : ?loc:location -> ?ty:t -> string -> t
  val const : ?loc:location -> Symbol.t -> t
  val app : ?loc:location -> t -> t list -> t
  val bind : ?loc:location -> Binder.t -> t list -> t -> t

  val and_ : ?loc:location -> t list -> t
  val or_ : ?loc:location -> t list -> t
  val not_ : ?loc:location -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  val xor : ?loc:location -> t -> t -> t
  val imply : ?loc:location -> t -> t -> t
  val eq : ?loc:location -> ?ty:t -> t -> t -> t
  val neq : ?loc:location -> ?ty:t -> t -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  val lambda : ?loc:location -> t list -> t -> t

  val mk_fun_ty : ?loc:location -> t list -> t -> t
  val tType : t
  val forall_ty : ?loc:location -> t list -> t -> t

  include Interfaces.PRINT with type t := t
end
