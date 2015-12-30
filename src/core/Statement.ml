
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement} *)

(** A statement for formulas of representation ['f] and types ['ty] *)
type ('f, 'ty) view =
  | TyDecl of ID.t * 'ty
  | Assert of 'f

type ('f, 'ty, 'meta) t = {
  view: ('f, 'ty) view;
  src: 'meta;
}

type ('f, 'ty) sourced_t = ('f, 'ty, StatementSrc.t) t

type clause = FOTerm.t SLiteral.t list
type clause_t = (clause, Type.t, StatementSrc.t) t

let view t = t.view
let src t = t.src

let ty_decl ~src id ty = {src; view=TyDecl (id,ty); }
let assert_ ~src c = {src; view=Assert c; }

let signature ?(init=Signature.empty) seq =
  Sequence.fold
    (fun sigma st -> match view st with
      | TyDecl (id, ty) -> Signature.declare sigma id ty
      | Assert _ -> sigma)
    init seq

let map ~form ~ty st =
  let map_view ~form ~ty = function
    | Assert f -> Assert (form f)
    | TyDecl (id, t) -> TyDecl (id, ty t)
  in
  {st with view = map_view ~form ~ty st.view; }

let map_src ~f st = {st with src=f st.src; }

(** {2 Iterators} *)

module Seq = struct
  let ty_decls st k = match view st with
    | TyDecl (id, ty) -> k (id,ty)
    | Assert _ -> ()

  let forms st k = match view st with
    | TyDecl _ -> ()
    | Assert c -> k c

  let lits st = forms st |> Sequence.flat_map Sequence.of_list

  let terms st = lits st |> Sequence.flat_map SLiteral.to_seq
end

(** {2 IO} *)

let fpf = Format.fprintf

let pp ppf ppty out st = match st.view with
  | TyDecl (id,ty) ->
      fpf out "@[<2>val %a :@ @[%a@]@]." ID.pp id ppty ty
  | Assert f ->
      fpf out "@[<2>assert@ (@[%a@])@]." ppf f

let to_string ppf ppty = CCFormat.to_string (pp ppf ppty)

let pp_clause =
  pp (Util.pp_list ~sep:" ∨ " (SLiteral.pp FOTerm.pp)) Type.pp

module TPTP = struct
  let pp ppf ppty out st =
    let name = match StatementSrc.name st.src with
      | None -> "no_name"
      | Some n -> n
    in
    match st.view with
    | TyDecl (id,ty) ->
        fpf out "@[<2>tff(%s, type,@ %a :@ @[%a@])@]." name ID.pp id ppty ty
    | Assert f ->
        let role =
          if StatementSrc.is_conjecture st.src
          then "negated_conjecture" else "axiom"
        in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]."
          name role
          ppf
          f

  let to_string ppf ppty = CCFormat.to_string (pp ppf ppty)
end

