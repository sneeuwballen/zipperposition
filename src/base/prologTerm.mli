
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

(** {1 Prolog-like Terms}.

Those terms are not hashconsed, nor do they use De Bruijn indices. Their
simplicity make them good for heavy AST transformations, output of parsing,
etc.
*)

type t =
  | Var of string             (** variable *)
  | Int of Z.t                (** integer *)
  | Rat of Q.t                (** rational *)
  | App of string * t list   (** apply symbol *)
  | Bind of string * t list * t   (** bind n variables *)
  | List of t list            (** special constructor for lists *)
  | Column of t * t           (** t:t (useful for typing, e.g.) *)

type term = t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

val var : string -> t
val int_ : Z.t -> t
val of_int : int -> t
val rat : Q.t -> t
val app : string -> t list -> t
val const : string -> t
val bind : string -> t list -> t -> t
val list_ : t list -> t
val nil : t
val column : t -> t -> t

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

  val symbols : t -> string Sequence.t
  val add_set : Set.t -> t Sequence.t -> Set.t
end

val ground : t -> bool
val close_all : string -> t -> t  (** Bind all free vars with the symbol *)

include Interfaces.PRINT with type t := t
