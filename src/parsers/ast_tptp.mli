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

(** {1 TPTP Ast} *)

open Logtk

exception ParseError of ParseLocation.t

type name =
  | NameInt of int
  | NameString of string
  (** name of a formula *)
and role =
  | R_axiom       (* true *)
  | R_hypothesis  (* true *)
  | R_definition  (* symbol definition *)
  | R_assumption  (* true, but must be proved before *)
  | R_lemma       (* must be proved before use *)
  | R_theorem     (* must be proved before use *)
  | R_conjecture  (* to be proven *)
  | R_negated_conjecture  (* negation of conjecture, must prove 'false' *)
  | R_plain       (* no specific semantics (proof...) *)
  | R_finite of string   (* finite interpretation, don't care *)
  | R_question    (* existential question *)
  | R_type        (* type declaration *)
  | R_unknown     (* error *)
  (** formula role *)
and optional_info = general_data list
and general_data =
  | GString of string
  | GVar of string   (* variable *)
  | GInt of int
  | GColumn of general_data * general_data
  | GNode of string * general_data list
  | GList of general_data list

val role_of_string : string -> role
val string_of_role : role -> string
val pp_role : Buffer.t -> role -> unit
val fmt_role : Format.formatter -> role -> unit

val string_of_name : name -> string
val pp_name : Buffer.t -> name -> unit
val fmt_name : Format.formatter -> name -> unit

val pp_general : Buffer.t -> general_data -> unit
val pp_general_debug : Buffer.t -> general_data -> unit  (* ugly version *)
val fmt_general : Format.formatter -> general_data -> unit
val pp_generals : Buffer.t -> general_data list -> unit
val fmt_generals : Format.formatter -> general_data list -> unit


module type S = sig
  type hoterm
  type form
  type ty

  type t =
    | CNF of name * role * form list * optional_info
    | FOF of name * role * form * optional_info
    | TFF of name * role * form * optional_info
    | THF of name * role * hoterm * optional_info  (* XXX not parsed yet *)
    | TypeDecl of name * string * ty  (* type declaration for a symbol *)
    | NewType of name * string * ty (* declare new type constant... *)
    | Include of string
    | IncludeOnly of string * name list   (* include a subset of names *)
    (** top level declaration *)

  type declaration = t

  val get_name : t -> name
    (** Find the name of the declaration, or
        @raise Invalid_argument if the declaration is an include directive *)

  class ['a] visitor : object
    method clause : 'a -> role -> form list -> 'a
    method fof : 'a -> role -> form -> 'a
    method tff : 'a -> role -> form -> 'a
    method thf : 'a -> role -> hoterm -> 'a
    method any_form : 'a -> role -> form -> 'a
    method tydecl : 'a -> string -> ty -> 'a
    method new_ty : 'a -> string -> ty -> 'a
    method include_ : 'a -> string -> 'a
    method include_only : 'a -> string -> name list -> 'a
    method visit : 'a -> t -> 'a
  end

  val map :
    ?form:(form -> form) ->
    ?hoterm:(hoterm -> hoterm) ->
    t -> t
  (** Map terms to other terms *)

  (** {2 IO} *)

  include Interfaces.PRINT with type t := t
end

(** default is with prolog terms everywhere *)
module Untyped : S
  with type hoterm = PrologTerm.t
  and type form = PrologTerm.t
  and type ty = PrologTerm.t

(** Typed version *)
module Typed : S
  with type hoterm = HOTerm.t
  and type form = Formula.FO.t
  and type ty = Type.t

module type MAP = sig
  module From : S
  module To : S

  val map :
    form:(From.form -> To.form) ->
    ho:(From.hoterm -> To.hoterm) ->
    ty:(From.ty -> To.ty) ->
    From.t -> To.t

  val flat_map :
    cnf:(From.form list -> To.form list list) ->
    form:(From.form -> To.form list) ->
    ho:(From.hoterm -> To.hoterm list) ->
    ty:(From.ty -> To.ty) ->
    From.t list -> To.t list
end

(** Mappings! *)
module Map(From:S)(To:S) : MAP
  with module From = From
  and module To = To
