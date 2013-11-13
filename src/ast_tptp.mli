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

exception ParseError of Location.t

type declaration =
  | CNF of name * role * Basic.Form.t list * optional_info
  | FOF of name * role * Basic.Form.t * optional_info
  | TFF of name * role * Basic.Form.t * optional_info
  | THF of name * role * Basic.HO.t * optional_info  (* XXX not parsed yet *)
  | TypeDecl of name * Symbol.t * Basic.Ty.t  (* type declaration *)
  | NewType of name * string * Basic.Ty.t (* declare new type constant... *)
  | Include of string
  | IncludeOnly of string * name list   (* include a subset of names *)
  (** top level declaration *)
and name =
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

val name_of_decl : declaration -> name
  (** Find the name of the declaration, or
      @raise Invalid_argument if the declaration is an include directive *)

(** {2 IO} *)

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

val pp_declaration : Buffer.t -> declaration -> unit
val fmt_declaration : Format.formatter -> declaration -> unit

