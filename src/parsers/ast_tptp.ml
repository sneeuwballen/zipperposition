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

module PT = PrologTerm
module HOT = HOTerm
module F = Formula.FO
module Loc = ParseLocation

exception ParseError of Loc.t

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

let role_of_string = function
  | "axiom" -> R_axiom
  | "hypothesis" -> R_hypothesis
  | "definition" -> R_definition
  | "assumption" ->  R_assumption
  | "lemma" -> R_lemma
  | "theorem" -> R_theorem
  | "conjecture" -> R_conjecture
  | "negated_conjecture" -> R_negated_conjecture
  | "plain" -> R_plain
  | "fi_domain" -> R_finite "domain"
  | "fi_functors" -> R_finite "functors"
  | "fi_predicates" -> R_finite "predicates"
  | "question" -> R_question
  | "type" -> R_type
  | "unknown" -> R_unknown
  | s -> failwith ("not a proper TPTP role: " ^ s)

let string_of_role = function
  | R_axiom -> "axiom"
  | R_hypothesis -> "hypothesis"
  | R_definition -> "definition"
  | R_assumption -> "assumption"
  | R_lemma -> "lemma"
  | R_theorem -> "theorem"
  | R_conjecture -> "conjecture"
  | R_negated_conjecture -> "negated_conjecture"
  | R_plain -> "plain"
  | R_finite what -> "fi_" ^ what
  | R_question -> "question"
  | R_type -> "type"
  | R_unknown -> "unknown"

let pp_role buf r =
  Buffer.add_string buf (string_of_role r)

let fmt_role fmt r =
  Format.pp_print_string fmt (string_of_role r)

let string_of_name = function
  | NameInt i -> string_of_int i
  | NameString s -> s

let pp_name buf n =
  Buffer.add_string buf (string_of_name n)

let fmt_name fmt n =
  Format.pp_print_string fmt (string_of_name n)

let rec pp_general buf d = match d with
  | GString s -> Buffer.add_string buf s
  | GInt i -> Printf.bprintf buf "%d" i
  | GVar s -> Buffer.add_string buf s
  | GColumn (a, b) -> Printf.bprintf buf "%a: %a" pp_general a pp_general b
  | GNode (f, l) ->
    Printf.bprintf buf "%s(%a)" f (Util.pp_list pp_general) l
  | GList l ->
    Printf.bprintf buf "[%a]" (Util.pp_list pp_general) l

let rec pp_general_debug buf d = match d with
  | GString s -> Printf.bprintf buf "GSstr %s" s
  | GInt i -> Printf.bprintf buf "GInt %d" i
  | GVar s -> Printf.bprintf buf "GVar %s" s
  | GColumn (a, b) -> Printf.bprintf buf "%a: %a" pp_general_debug a pp_general_debug b
  | GNode (f, l) ->
    Printf.bprintf buf "GNode(%s[%a])" f (Util.pp_list pp_general_debug) l
  | GList l ->
    Printf.bprintf buf "[%a]" (Util.pp_list pp_general_debug) l

let fmt_general fmt d =
  Format.pp_print_string fmt (Util.sprintf "%a" pp_general d)

let pp_generals buf l =
  Util.pp_list pp_general buf l
let fmt_generals fmt l =
  Sequence.pp_seq fmt_general fmt (Sequence.of_list l)

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
  end

  val fold : 'a visitor -> 'a -> t Sequence.t -> 'a
    (** Fold over declarations *)

  (** {2 IO} *)

  include Interfaces.PRINT with type t := t
end

module Untyped = struct
  type hoterm = PT.t
  type form = PT.t
  type ty = PT.t

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

  let get_name = function
    | CNF (n, _, _, _) -> n
    | FOF (n, _, _, _) -> n
    | TFF (n, _, _, _) -> n
    | THF (n, _, _, _) -> n
    | TypeDecl (n, _, _) -> n
    | NewType (n, _, _) -> n
    | IncludeOnly _
    | Include _ ->
      raise (Invalid_argument "Ast_tptp.name_of_decl: include directive has no name")

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
  end = object
    method clause acc _r _c = acc
    method fof acc _r _f = acc
    method tff acc _r _f = acc
    method thf acc _r _f = acc
    method any_form acc _r _f = acc
    method tydecl acc _s _ty = acc
    method new_ty acc _s _ty = acc
    method include_ acc _file = acc
    method include_only acc _file _names = acc
  end

  let fold visitor acc seq =
    Sequence.fold
      (fun acc decl -> match decl with
        | CNF (_, r, c, _) -> visitor#clause acc r c
        | FOF (_, r, f, _) -> visitor#any_form (visitor#fof acc r f) r f
        | TFF (_, r, f, _) -> visitor#any_form (visitor#tff acc r f) r f
        | THF (_, r, f, _) -> visitor#thf acc r f
        | TypeDecl (_, s, ty) -> visitor#tydecl acc s ty
        | NewType (_, s, ty) -> visitor#new_ty acc s ty
        | Include f -> visitor#include_ acc f
        | IncludeOnly (f,names) -> visitor#include_only acc f names
      ) acc seq

  (** {2 IO} *)

  let __pp_formula buf pp (logic, name, role, f, generals) =
    match generals with
    | [] ->
      Printf.bprintf buf "%s(%a, %a, (%a))."
        logic pp_name name pp_role role pp f
    | _::_ ->
      Printf.bprintf buf "%s(%a, %a, (%a), %a)." logic pp_name name pp_role role
        pp f pp_generals generals

  let pp buf = function
    | Include filename -> Printf.bprintf buf "include('%s')." filename
    | IncludeOnly (filename, names) ->
      Printf.bprintf buf "include('%s', [%a])." filename (Util.pp_list pp_name) names
    | TypeDecl (name, s, ty) ->
      Printf.bprintf buf "tff(%a, type, (%s : %a))."
        pp_name name s PT.TPTP.pp ty
    | NewType (name, s, kind) ->
      Printf.bprintf buf "tff(%a, type, (%s: %a))." pp_name name s PT.TPTP.pp kind
    | CNF (name, role, c, generals) ->
      __pp_formula buf (Util.pp_list ~sep:" | " PT.TPTP.pp) ("cnf", name, role, c, generals)
    | FOF (name, role, f, generals) ->
      __pp_formula buf PT.TPTP.pp  ("fof", name, role, f, generals)
    | TFF (name, role, f, generals) ->
      __pp_formula buf PT.TPTP.pp ("tff", name, role, f, generals)
    | THF (name, role, f, generals) ->
      __pp_formula buf PT.TPTP.pp ("thf", name, role, f, generals)

  let to_string = Util.on_buffer pp

  let fmt fmt d = Format.pp_print_string fmt (to_string d)
end

module Typed = struct
  type hoterm = HOTerm.t
  type form = Formula.FO.t
  type ty = Type.t

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

  let get_name = function
    | CNF (n, _, _, _) -> n
    | FOF (n, _, _, _) -> n
    | TFF (n, _, _, _) -> n
    | THF (n, _, _, _) -> n
    | TypeDecl (n, _, _) -> n
    | NewType (n, _, _) -> n
    | IncludeOnly _
    | Include _ ->
      raise (Invalid_argument "Ast_tptp.name_of_decl: include directive has no name")

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
  end = object
    method clause acc _r _c = acc
    method fof acc _r _f = acc
    method tff acc _r _f = acc
    method thf acc _r _f = acc
    method any_form acc _r _f = acc
    method tydecl acc _s _ty = acc
    method new_ty acc _s _ty = acc
    method include_ acc _file = acc
    method include_only acc _file _names = acc
  end

  let fold visitor acc seq =
    Sequence.fold
      (fun acc decl -> match decl with
        | CNF (_, r, c, _) -> visitor#clause acc r c
        | FOF (_, r, f, _) -> visitor#any_form (visitor#fof acc r f) r f
        | TFF (_, r, f, _) -> visitor#any_form (visitor#tff acc r f) r f
        | THF (_, r, f, _) -> visitor#thf acc r f
        | TypeDecl (_, s, ty) -> visitor#tydecl acc s ty
        | NewType (_, s, ty) -> visitor#new_ty acc s ty
        | Include f -> visitor#include_ acc f
        | IncludeOnly (f,names) -> visitor#include_only acc f names
      ) acc seq

  (** {2 IO} *)

  let __pp_formula buf pp (logic, name, role, f, generals) =
    match generals with
    | [] ->
      Printf.bprintf buf "%s(%a, %a, (%a))."
        logic pp_name name pp_role role pp f
    | _::_ ->
      Printf.bprintf buf "%s(%a, %a, (%a), %a)." logic pp_name name pp_role role
        pp f pp_generals generals

  let pp buf = function
    | Include filename -> Printf.bprintf buf "include('%s')." filename
    | IncludeOnly (filename, names) ->
      Printf.bprintf buf "include('%s', [%a])." filename (Util.pp_list pp_name) names
    | TypeDecl (name, s, ty) ->
      Printf.bprintf buf "tff(%a, type, (%s : %a))."
        pp_name name s Type.TPTP.pp ty
    | NewType (name, s, kind) ->
      Printf.bprintf buf "tff(%a, type, (%s: %a))." pp_name name s Type.TPTP.pp kind
    | CNF (name, role, c, generals) ->
      __pp_formula buf (Util.pp_list ~sep:" | " Formula.FO.TPTP.pp) ("cnf", name, role, c, generals)
    | FOF (name, role, f, generals) ->
      __pp_formula buf Formula.FO.TPTP.pp ("fof", name, role, f, generals)
    | TFF (name, role, f, generals) ->
      __pp_formula buf Formula.FO.TPTP.pp ("tff", name, role, f, generals)
    | THF (name, role, f, generals) ->
      __pp_formula buf HOTerm.TPTP.pp ("thf", name, role, f, generals)

  let to_string = Util.on_buffer pp

  let fmt fmt d = Format.pp_print_string fmt (to_string d)
end

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

module Map(From : S)(To : S) = struct
  module From = From
  module To = To

  let map ~form ~ho ~ty = function
    | From.CNF (n,r, l, i) ->
        To.CNF (n,r, List.map form l, i)
    | From.FOF (n,r, f, i) ->
        To.FOF (n,r, form f, i)
    | From.TFF (n,r, f, i) ->
        To.TFF (n,r, form f, i)
    | From.THF (n,r, f, i) ->
        To.THF (n,r, ho f, i)
    | From.TypeDecl (n, s, t) ->
        To.TypeDecl (n, s, ty t)
    | From.NewType (n, s, t) ->
        To.NewType (n, s, ty t)
    | From.Include s -> To.Include s
    | From.IncludeOnly (s,l) -> To.IncludeOnly (s,l)

  let flat_map ~cnf ~form ~ho ~ty l =
    Util.list_flatmap
      begin function
      | From.CNF (n,r, l, i) ->
          let l' = cnf l in
          List.map
            (fun clause -> To.CNF(n,r, clause, i))
            l'
      | From.FOF (n,r, f, i) ->
          List.map
            (fun f' -> To.FOF (n,r, f', i))
            (form f)
      | From.TFF (n,r, f, i) ->
          List.map
            (fun f' -> To.TFF (n,r, f', i))
            (form f)
      | From.THF (n,r, f, i) ->
          List.map
            (fun f' -> To.THF (n,r, f', i))
            (ho f)
      | From.TypeDecl (n, s, t) ->
          [To.TypeDecl (n, s, ty t)]
      | From.NewType (n, s, t) ->
          [To.NewType (n, s, ty t)]
      | From.Include s -> [To.Include s]
      | From.IncludeOnly (s,l) -> [To.IncludeOnly (s,l)]
      end
      l
end
