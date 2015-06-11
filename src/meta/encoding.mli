
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

(** {1 Encoding of clauses} *)

open Logtk

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Base definitions} *)

type 'a lit =
  | Eq of 'a * 'a * bool
  | Prop of 'a * bool
  | Bool of bool

val fmap_lit : ('a -> 'b) -> 'a lit -> 'b lit

type 'a clause = 'a lit list

val fmap_clause : ('a -> 'b) -> 'a clause -> 'b clause

type foterm = FOTerm.t
type hoterm = HOTerm.t

type foclause = foterm clause
type hoclause = hoterm clause

val foclause_of_clause : Formula.FO.t list -> foclause
  (** @raise Invalid_argument if the argument is not a proper clause *)

val clause_of_foclause : foclause -> Formula.FO.t list
  (** Convert back to a list of formulas
      @since 0.8 *)

val pp_clause : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a clause -> unit
  (** Printer of clauses *)

val print_clause : 'a printer -> 'a clause printer
  (** Printer of clauses *)

(** {6 Encoding abstraction} *)

class type ['a, 'b] t = object
  method encode : 'a -> 'b
  method decode : 'b -> 'a option
end

val id : ('a,'a) t
  (** Identity encoding *)

val compose : ('a,'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Compose two encodings together *)

val (>>>) : ('a,'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Infix notation for composition *)

(** {6 Currying} *)

val currying : (foterm clause, hoterm clause) t

(** {6 Clause encoding}

Encode the whole clause into a {!Reasoner.Property.t}, ie a higher-order term
that represents a meta-level property. *)

module EncodedClause : sig
  type t = private Reasoner.term

  include Interfaces.PRINT with type t := t
  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  val __magic : hoterm -> t
    (** Don't use unless you know what you're doing. *)
end

val signature : Signature.t
  (** Signature to use with this encoding *)

val clause_prop : (hoclause, EncodedClause.t) t

