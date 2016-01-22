
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement} *)


(** A datatype declaration *)
type 'ty data = {
  data_id: ID.t;
    (** Name of the type *)
  data_args: 'ty Var.t list;
    (** type parameters *)
  data_ty: 'ty;
    (** type of Id, that is,   [type -> type -> ... -> type] *)
  data_cstors: (ID.t * 'ty) list;
    (** Each constructor is [id, ty]. [ty] must be of the form
     [ty1 -> ty2 -> ... -> id args] *)
}

type ('f, 't, 'ty) view =
  | TyDecl of ID.t * 'ty (** id: ty *)
  | Data of 'ty data
  | Def of ID.t * 'ty * 't
  | Assert of 'f (** assert form *)

type ('f, 't, 'ty, 'meta) t = {
  view: ('f, 't, 'ty) view;
  src: 'meta; (** additional data *)
}

type ('f, 't, 'ty) sourced_t = ('f, 't, 'ty, StatementSrc.t) t

type clause = FOTerm.t SLiteral.t list
type clause_t = (clause, FOTerm.t, Type.t) sourced_t

let view t = t.view
let src t = t.src

let mk_data id ~args ty cstors =
  {data_id=id; data_args=args; data_ty=ty; data_cstors=cstors; }

let ty_decl ~src id ty = {src; view=TyDecl (id,ty); }
let def ~src id ty t = {src; view=Def (id,ty,t); }
let data ~src d = {src; view=Data d; }
let assert_ ~src c = {src; view=Assert c; }

let signature ?(init=Signature.empty) seq =
  Sequence.fold
    (fun sigma st -> match view st with
      | TyDecl (id, ty)
      | Def (id, ty, _) -> Signature.declare sigma id ty
      | Data d ->
          let sigma = Signature.declare sigma d.data_id d.data_ty in
          List.fold_left
            (fun sigma (id, ty_cstor) -> Signature.declare sigma id ty_cstor)
            sigma d.data_cstors
      | Assert _ -> sigma)
    init seq

let map ~form ~term ~ty st =
  let map_view ~form ~term ~ty:fty = function
    | Def (id, ty, t) -> Def (id, fty ty, term t)
    | Data d ->
        let d = {
          d with
          data_args = List.map (Var.update_ty ~f:fty) d.data_args;
          data_ty = fty d.data_ty;
          data_cstors = List.map (fun (id,ty) -> id, fty ty) d.data_cstors;
        } in
        Data d
    | Assert f -> Assert (form f)
    | TyDecl (id, ty) -> TyDecl (id, fty ty)
  in
  {st with view = map_view ~form ~term ~ty st.view; }

let map_src ~f st = {st with src=f st.src; }

(** {2 Iterators} *)

module Seq = struct
  let ty_decls st k = match view st with
    | Def (id, ty, _)
    | TyDecl (id, ty) -> k (id,ty)
    | Data d ->
        k (d.data_id, d.data_ty);
        List.iter k d.data_cstors
    | Assert _ -> ()

  let forms st k = match view st with
    | Def _
    | Data _
    | TyDecl _ -> ()
    | Assert c -> k c

  let lits st = forms st |> Sequence.flat_map Sequence.of_list

  let terms st = lits st |> Sequence.flat_map SLiteral.to_seq
end

(** {2 IO} *)

let fpf = Format.fprintf

let pp ppf ppt ppty out st = match st.view with
  | TyDecl (id,ty) ->
      fpf out "@[<2>val %a :@ @[%a@]@]." ID.pp id ppty ty
  | Def (id,ty,t) ->
      fpf out "@[<2>def %a :@ @[%a@]@ := @[%a@]@]." ID.pp id ppty ty ppt t
  | Data d ->
      let pp_cstor out (id,ty) =
        fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp id ppty ty in
      fpf out "@[<2>data @[%a : %a@] :=@ @[<v>%a@]."
        ID.pp d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
  | Assert f ->
      fpf out "@[<2>assert@ (@[%a@])@]." ppf f

let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)

let pp_clause =
  pp (Util.pp_list ~sep:" ∨ " (SLiteral.pp FOTerm.pp)) FOTerm.pp Type.pp

module TPTP = struct
  let pp ppf ppt ppty out st =
    let name = match StatementSrc.name st.src with
      | None -> "no_name"
      | Some n -> n
    in
    let pp_decl out (id,ty) =
      fpf out "@[<2>tff(%s, type,@ %a :@ @[%a@])@]." name ID.pp id ppty ty
    in
    match st.view with
    | TyDecl (id,ty) -> pp_decl out (id,ty)
    | Assert f ->
        let role =
          if StatementSrc.is_conjecture st.src
          then "negated_conjecture" else "axiom"
        in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]."
          name role
          ppf
          f
    | Def (id, ty, t) ->
        pp_decl out (id,ty);
        fpf out "@[<2>tff(%s, axiom,@ %a =@ @[%a@])@]." name ID.pp id ppt t
    | Data _ -> failwith "cannot print `data` to TPTP"

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

