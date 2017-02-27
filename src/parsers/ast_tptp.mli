
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 TPTP Ast} *)

(* TODO: keep parsing locations? *)

open Logtk

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
  | TypeDecl of name * string * 'a * optional_info  (* type declaration for a symbol *)
  | NewType of name * string * 'a * optional_info (* declare new type constant... *)
  | Include of string
  | IncludeOnly of string * name list   (* include a subset of names *)
(** top level declaration *)

type 'a declaration = 'a t

val get_name : _ t -> name
(** Find the name of the declaration, or
    @raise Invalid_argument if the declaration is an include directive *)

(** {2 IO} *)

include Interfaces.PRINT1 with type 'a t := 'a t
