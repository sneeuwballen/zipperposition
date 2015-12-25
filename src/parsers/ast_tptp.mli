
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 TPTP Ast} *)

(* TODO: keep parsing locations? *)

open Libzipperposition

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
val pp_role : role CCFormat.printer

val string_of_name : name -> string
val pp_name : name CCFormat.printer

val pp_general : general_data CCFormat.printer
val pp_general_debugf : general_data CCFormat.printer  (* ugly version *)
val pp_generals : general_data list CCFormat.printer

type 'a t =
  | CNF of name * role * 'a list * optional_info
  | FOF of name * role * 'a * optional_info
  | TFF of name * role * 'a * optional_info
  | THF of name * role * 'a * optional_info (* XXX not parsed yet *)
  | TypeDecl of name * ID.t * 'a * optional_info  (* type declaration for a symbol *)
  | NewType of name * ID.t * 'a * optional_info (* declare new type constant... *)
  | Include of string
  | IncludeOnly of string * name list   (* include a subset of names *)
(** top level declaration *)

type 'a declaration = 'a t

val get_name : _ t -> name
(** Find the name of the declaration, or
    @raise Invalid_argument if the declaration is an include directive *)

class ['a, 't] visitor : object
  method clause : 'a -> role -> 't list -> 'a
  method fof : 'a -> role -> 't -> 'a
  method tff : 'a -> role -> 't -> 'a
  method thf : 'a -> role -> 't -> 'a
  method any_form : 'a -> role -> 't -> 'a
  method tydecl : 'a -> ID.t -> 't -> 'a
  method new_ty : 'a -> ID.t -> 't -> 'a
  method include_ : 'a -> string -> 'a
  method include_only : 'a -> string -> name list -> 'a
  method visit : 'a -> 't t -> 'a
end

val map :
  form:('a -> 'b) ->
  ho:('a -> 'b) ->
  ty:('a -> 'b) ->
  'a t -> 'b t
(** Map terms to other terms *)

val flat_map :
  cnf:('a list -> 'b list list) ->
  form:('a -> 'b list) ->
  ho:('a -> 'b list) ->
  ty:('a -> 'b) ->
  ('a t, CCVector.ro) CCVector.t ->
  ('b t, CCVector.ro) CCVector.t

module Seq : sig
  val forms : 'a t -> 'a Sequence.t
  val hoterms : 'a t -> 'a Sequence.t
end

(** {2 IO} *)

include Interfaces.PRINT1 with type 'a t := 'a t
