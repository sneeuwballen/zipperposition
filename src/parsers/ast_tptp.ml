
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 TPTP Ast} *)

open Logtk

module PT = STerm
module HOT = HOTerm
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
      Format.fprintf out "%s(%a)" f (Util.pp_list pp_general) l
  | GList l ->
      Format.fprintf out "[%a]" (Util.pp_list pp_general) l

let rec pp_general_debugf out d = match d with
  | GString s -> Format.fprintf out "GSstr %s" s
  | GInt i -> Format.fprintf out "GInt %d" i
  | GVar s -> Format.fprintf out "GVar %s" s
  | GColumn (a, b) -> Format.fprintf out "%a: %a" pp_general_debugf a pp_general_debugf b
  | GNode (f, l) ->
      Format.fprintf out "GNode(%s[%a])" f (Util.pp_list pp_general_debugf) l
  | GList l ->
      CCFormat.list pp_general_debugf out l

let pp_generals out l = match l with
  | [] -> ()
  | _::_ ->
      Format.fprintf out ",@ ";
      Util.pp_list ~sep:", " pp_general out l

type 'a t =
  | CNF of name * role * 'a list * optional_info
  | FOF of name * role * 'a * optional_info
  | TFF of name * role * 'a * optional_info
  | THF of name * role * 'a * optional_info  (* XXX not parsed yet *)
  | TypeDecl of name * ID.t * 'a * optional_info  (* type declaration for a symbol *)
  | NewType of name * ID.t * 'a * optional_info (* declare new type constant... *)
  | Include of string
  | IncludeOnly of string * name list   (* include a subset of names *)
(** top level declaration *)

type 'a declaration = 'a t

let get_name = function
  | CNF (n, _, _, _) -> n
  | FOF (n, _, _, _) -> n
  | TFF (n, _, _, _) -> n
  | THF (n, _, _, _) -> n
  | TypeDecl (n, _, _, _) -> n
  | NewType (n, _, _, _) -> n
  | IncludeOnly _
  | Include _ ->
      invalid_arg "Ast_tptp.name_of_decl: include directive has no name"

class ['a,'t] visitor = object (self)
  method clause (acc:'a) _r _c = acc
  method fof (acc:'a) _r _f = acc
  method tff (acc:'a) _r _f = acc
  method thf (acc:'a) _r _f = acc
  method any_form (acc:'a) _r _f = acc
  method tydecl (acc:'a) _s _ty = acc
  method new_ty (acc:'a) _s _ty = acc
  method include_ (acc:'a) _file = acc
  method include_only (acc:'a) _file _names = acc
  method visit (acc:'a) (decl:'t t) = match decl with
    | CNF (_, r, c, _) -> self#clause acc r c
    | FOF (_, r, f, _) -> self#any_form (self#fof acc r f) r f
    | TFF (_, r, f, _) -> self#any_form (self#tff acc r f) r f
    | THF (_, r, f, _) -> self#thf acc r f
    | TypeDecl (_, s, ty, _) -> self#tydecl acc s ty
    | NewType (_, s, ty, _) -> self#new_ty acc s ty
    | Include f -> self#include_ acc f
    | IncludeOnly (f,names) -> self#include_only acc f names
end

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

let map ~form ~ho ~ty = function
  | CNF (n,r, l, i) ->
      CNF (n,r, List.map form l, i)
  | FOF (n,r, f, i) ->
      FOF (n,r, form f, i)
  | TFF (n,r, f, i) ->
      TFF (n,r, form f, i)
  | THF (n,r, f, i) ->
      THF (n,r, ho f, i)
  | TypeDecl (n, s, t, i) ->
      TypeDecl (n, s, ty t, i)
  | NewType (n, s, t, i) ->
      NewType (n, s, ty t, i)
  | Include s -> Include s
  | IncludeOnly (s,l) -> IncludeOnly (s,l)

let flat_map ~cnf ~form ~ho ~ty v =
  let res = CCVector.create() in
  CCVector.iter
    (function
      | CNF (n,r, l, i) ->
          let l' = cnf l in
          List.iter
            (fun clause -> CCVector.push res (CNF(n,r, clause, i)))
            l'
      | FOF (n,r, f, i) ->
          List.iter
            (fun f' -> CCVector.push res (FOF (n,r, f', i)))
            (form f)
      | TFF (n,r, f, i) ->
          List.iter
            (fun f' -> CCVector.push res (TFF (n,r, f', i)))
            (form f)
      | THF (n,r, f, i) ->
          List.iter
            (fun f' -> CCVector.push res (THF (n,r, f', i)))
            (ho f)
      | TypeDecl (n, s, t, i) ->
          CCVector.push res (TypeDecl (n, s, ty t, i))
      | NewType (n, s, t, i) ->
          CCVector.push res (NewType (n, s, ty t, i))
      | Include s -> CCVector.push res (Include s)
      | IncludeOnly (s,l) -> CCVector.push res (IncludeOnly (s,l))
    )
    v;
  CCVector.freeze res

(** {2 IO} *)

let fpf = Format.fprintf

let pp_form_ pp out (logic, name, role, f, generals) =
  Format.fprintf out "@[<2>%s(%a,@ %a,@ (@[%a@])%a@])."
    logic pp_name name pp_role role pp f pp_generals generals

let pp pp_t out = function
  | Include filename -> fpf out "@[<hv2>include(@,'%s'@,).@]" filename
  | IncludeOnly (filename, names) ->
      fpf out "@[<2>include('%s',@ [@[%a@]]@])." filename (Util.pp_list pp_name) names
  | TypeDecl (name, s, ty, g) ->
      fpf out "@[<2>tff(%a, type,@ (@[%a : %a@])%a@])."
        pp_name name ID.pp s pp_t ty pp_generals g
  | NewType (name, s, kind, g) ->
      fpf out "@[<2>tff(%a, type,@ (@[%a : %a@])%a@])."
        pp_name name ID.pp s pp_t kind pp_generals g
  | CNF (name, role, c, generals) ->
      pp_form_
        (Util.pp_list ~sep:" | " pp_t) out
        ("cnf", name, role, c, generals)
  | FOF (name, role, f, generals) ->
      pp_form_ pp_t out ("fof", name, role, f, generals)
  | TFF (name, role, f, generals) ->
      pp_form_ pp_t out ("tff", name, role, f, generals)
  | THF (name, role, f, generals) ->
      pp_form_ pp_t out ("thf", name, role, f, generals)

let to_string ppt = CCFormat.to_string (pp ppt)
