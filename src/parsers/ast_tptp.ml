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

module PT = STerm
module HOT = HOTerm
module F = Formula.FO
module Loc = ParseLocation

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

let pp_role out r =
  CCFormat.string out (string_of_role r)

let string_of_name = function
  | NameInt i -> string_of_int i
  | NameString s -> s

let pp_name out n =
  CCFormat.string out (string_of_name n)

let rec pp_general out d = match d with
  | GString s -> CCFormat.string out s
  | GInt i -> CCFormat.int out i
  | GVar s -> CCFormat.string out s
  | GColumn (a, b) -> Format.fprintf out "%a: %a" pp_general a pp_general b
  | GNode (f, l) ->
    Format.fprintf out "%s(%a)" f (CCFormat.list pp_general) l
  | GList l ->
    Format.fprintf out "[%a]" (CCFormat.list pp_general) l

let rec pp_general_debugf out d = match d with
  | GString s -> Format.fprintf out "GSstr %s" s
  | GInt i -> Format.fprintf out "GInt %d" i
  | GVar s -> Format.fprintf out "GVar %s" s
  | GColumn (a, b) -> Format.fprintf out "%a: %a" pp_general_debugf a pp_general_debugf b
  | GNode (f, l) ->
    Format.fprintf out "GNode(%s[%a])" f (CCFormat.list pp_general_debugf) l
  | GList l ->
    CCFormat.list pp_general_debugf out l

let pp_generals out l = match l with
  | [] -> ()
  | _::_ ->
      Format.fprintf out ",@ ";
      CCFormat.list ~start:"" ~stop:"" ~sep:", " pp_general out l

module type S = sig
  type hoterm
  type form
  type ty

  type t =
    | CNF of name * role * form list * optional_info
    | FOF of name * role * form * optional_info
    | TFF of name * role * form * optional_info
    | THF of name * role * hoterm * optional_info  (* XXX not parsed yet *)
    | TypeDecl of name * string * ty * optional_info  (* type declaration for a symbol *)
    | NewType of name * string * ty * optional_info (* declare new type constant... *)
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

  module Seq : sig
    val forms : t -> form Sequence.t
    val hoterms : t -> hoterm Sequence.t
  end

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
    | TypeDecl of name * string * ty * optional_info  (* type declaration for a symbol *)
    | NewType of name * string * ty * optional_info (* declare new type constant... *)
    | Include of string
    | IncludeOnly of string * name list   (* include a subset of names *)
    (** top level declaration *)

  type declaration = t

  let get_name = function
    | CNF (n, _, _, _) -> n
    | FOF (n, _, _, _) -> n
    | TFF (n, _, _, _) -> n
    | THF (n, _, _, _) -> n
    | TypeDecl (n, _, _, _) -> n
    | NewType (n, _, _, _) -> n
    | IncludeOnly _
    | Include _ ->
      raise (Invalid_argument "Ast_tptp.name_of_decl: include directive has no name")

  class ['a] visitor = object (self)
    method clause (acc:'a) _r _c = acc
    method fof (acc:'a) _r _f = acc
    method tff (acc:'a) _r _f = acc
    method thf (acc:'a) _r _f = acc
    method any_form (acc:'a) _r _f = acc
    method tydecl (acc:'a) _s _ty = acc
    method new_ty (acc:'a) _s _ty = acc
    method include_ (acc:'a) _file = acc
    method include_only (acc:'a) _file _names = acc
    method visit (acc:'a) decl = match decl with
      | CNF (_, r, c, _) -> self#clause acc r c
      | FOF (_, r, f, _) -> self#any_form (self#fof acc r f) r f
      | TFF (_, r, f, _) -> self#any_form (self#tff acc r f) r f
      | THF (_, r, f, _) -> self#thf acc r f
      | TypeDecl (_, s, ty, _) -> self#tydecl acc s ty
      | NewType (_, s, ty, _) -> self#new_ty acc s ty
      | Include f -> self#include_ acc f
      | IncludeOnly (f,names) -> self#include_only acc f names
  end

  let __id f = f

  let map ?(form=__id) ?(hoterm=__id) = function
    | CNF (n,r,c,i) -> CNF(n,r, List.map form c, i)
    | FOF (n,r,f,i) -> FOF(n,r, form f, i)
    | TFF (n,r,f,i) -> TFF(n,r, form f, i)
    | THF (n,r,f,i) -> THF(n,r, hoterm f, i)
    | (TypeDecl _ | NewType _ | IncludeOnly _ | Include _) as d -> d

  module Seq = struct
    let forms decl k = match decl with
      | FOF (_,_,f,_)
      | TFF (_,_,f,_) -> k f
      | CNF (_,_,c,_) -> List.iter k c
      | _ -> ()

    let hoterms decl k = match decl with
      | THF (_,_,f,_) -> k f
      | _ -> ()
  end

  (** {2 IO} *)

  let pp_form_ out pp (logic, name, role, f, generals) =
    Format.fprintf out "@[<2>%s(%a,@ %a,@ @[(%a)@]%a@])."
      logic pp_name name pp_role role pp f pp_generals generals

  let pp out = function
    | Include filename -> Format.fprintf out "include('%s')." filename
    | IncludeOnly (filename, names) ->
      Format.fprintf out "@[include('%s',2 [%a]@])." filename (CCFormat.list pp_name) names
    | TypeDecl (name, s, ty, g) ->
      Format.fprintf out "@[<2>tff(%a, type,@ (%s : %a)%a@])."
        pp_name name s PT.TPTP.pp ty pp_generals g
    | NewType (name, s, kind, g) ->
      Format.fprintf out "@[<2>tff(%a, type,@ (%s: %a)%a@])."
        pp_name name s PT.TPTP.pp kind pp_generals g
    | CNF (name, role, c, generals) ->
      pp_form_ out
        (CCFormat.list ~start:"" ~stop:"" ~sep:" | " PT.TPTP.pp)
        ("cnf", name, role, c, generals)
    | FOF (name, role, f, generals) ->
      pp_form_ out PT.TPTP.pp  ("fof", name, role, f, generals)
    | TFF (name, role, f, generals) ->
      pp_form_ out PT.TPTP.pp ("tff", name, role, f, generals)
    | THF (name, role, f, generals) ->
      pp_form_ out PT.TPTP.pp ("thf", name, role, f, generals)

  let to_string = CCFormat.to_string pp
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
    | TypeDecl of name * string * ty * optional_info (* type declaration for a symbol *)
    | NewType of name * string * ty * optional_info (* declare new type constant... *)
    | Include of string
    | IncludeOnly of string * name list   (* include a subset of names *)
    (** top level declaration *)

  type declaration = t

  let get_name = function
    | CNF (n, _, _, _) -> n
    | FOF (n, _, _, _) -> n
    | TFF (n, _, _, _) -> n
    | THF (n, _, _, _) -> n
    | TypeDecl (n, _, _, _) -> n
    | NewType (n, _, _, _) -> n
    | IncludeOnly _
    | Include _ ->
      raise (Invalid_argument "Ast_tptp.name_of_decl: include directive has no name")

  class ['a] visitor = object (self)
    method clause (acc:'a) _r _c = acc
    method fof (acc:'a) _r _f = acc
    method tff (acc:'a) _r _f = acc
    method thf (acc:'a) _r _f = acc
    method any_form (acc:'a) _r _f = acc
    method tydecl (acc:'a) _s _ty = acc
    method new_ty (acc:'a) _s _ty = acc
    method include_ (acc:'a) _file = acc
    method include_only (acc:'a) _file _names = acc
    method visit (acc:'a) decl = match decl with
      | CNF (_, r, c, _) -> self#clause acc r c
      | FOF (_, r, f, _) -> self#any_form (self#fof acc r f) r f
      | TFF (_, r, f, _) -> self#any_form (self#tff acc r f) r f
      | THF (_, r, f, _) -> self#thf acc r f
      | TypeDecl (_, s, ty, _) -> self#tydecl acc s ty
      | NewType (_, s, ty, _) -> self#new_ty acc s ty
      | Include f -> self#include_ acc f
      | IncludeOnly (f,names) -> self#include_only acc f names
  end

  let __id f = f

  let map ?(form=__id) ?(hoterm=__id) = function
    | CNF (n,r,c,i) -> CNF(n,r, List.map form c, i)
    | FOF (n,r,f,i) -> FOF(n,r, form f, i)
    | TFF (n,r,f,i) -> TFF(n,r, form f, i)
    | THF (n,r,f,i) -> THF(n,r, hoterm f, i)
    | (TypeDecl _ | NewType _ | IncludeOnly _ | Include _) as d -> d

  module Seq = struct
    let forms decl k = match decl with
      | FOF (_,_,f,_)
      | TFF (_,_,f,_) -> k f
      | CNF (_,_,c,_) -> List.iter k c
      | _ -> ()

    let hoterms decl k = match decl with
      | THF (_,_,f,_) -> k f
      | _ -> ()
  end

  (** {2 IO} *)

  let pp_form_ out pp (logic, name, role, f, generals) =
    Format.fprintf out "@[<2>%s(%a,@ %a,@ @[(%a)@]%a@])."
      logic pp_name name pp_role role pp f pp_generals generals

  let pp out = function
    | Include filename -> Format.fprintf out "include('%s')." filename
    | IncludeOnly (filename, names) ->
      Format.fprintf out "@[include('%s',2 [%a]@])." filename (CCFormat.list pp_name) names
    | TypeDecl (name, s, ty, g) ->
      Format.fprintf out "@[<2>tff(%a, type,@ (%s : %a)%a@])."
        pp_name name s Type.TPTP.pp ty pp_generals g
    | NewType (name, s, kind, g) ->
      Format.fprintf out "@[<2>tff(%a, type,@ (%s: %a)%a@])."
        pp_name name s Type.TPTP.pp kind pp_generals g
    | CNF (name, role, c, generals) ->
      pp_form_ out
        (CCFormat.list ~start:"" ~stop:"" ~sep:" | " Formula.FO.TPTP.pp)
        ("cnf", name, role, c, generals)
    | FOF (name, role, f, generals) ->
      pp_form_ out Formula.FO.TPTP.pp  ("fof", name, role, f, generals)
    | TFF (name, role, f, generals) ->
      pp_form_ out Formula.FO.TPTP.pp ("tff", name, role, f, generals)
    | THF (name, role, f, generals) ->
      pp_form_ out HOTerm.TPTP.pp ("thf", name, role, f, generals)

  let to_string = CCFormat.to_string pp
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
    | From.TypeDecl (n, s, t, i) ->
        To.TypeDecl (n, s, ty t, i)
    | From.NewType (n, s, t, i) ->
        To.NewType (n, s, ty t, i)
    | From.Include s -> To.Include s
    | From.IncludeOnly (s,l) -> To.IncludeOnly (s,l)

  let flat_map ~cnf ~form ~ho ~ty l =
    CCList.flat_map
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
      | From.TypeDecl (n, s, t, i) ->
          [To.TypeDecl (n, s, ty t, i)]
      | From.NewType (n, s, t, i) ->
          [To.NewType (n, s, ty t, i)]
      | From.Include s -> [To.Include s]
      | From.IncludeOnly (s,l) -> [To.IncludeOnly (s,l)]
      end
      l
end
