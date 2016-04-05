
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
  | Data of 'ty data list (** Mutally recursive types *)
  | Def of ID.t * 'ty * 't
  | Assert of 'f (** assert form *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'f list (** goal after negation *)

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
let data ~src l = {src; view=Data l; }
let assert_ ~src c = {src; view=Assert c; }
let goal ~src c = {src; view=Goal c; }
let neg_goal ~src l = {src; view=NegatedGoal l; }

let map_data ~ty:fty d =
  { d with
    data_args = List.map (Var.update_ty ~f:fty) d.data_args;
    data_ty = fty d.data_ty;
    data_cstors = List.map (fun (id,ty) -> id, fty ty) d.data_cstors;
  }

let map ~form ~term ~ty st =
  let map_view ~form ~term ~ty:fty = function
    | Def (id, ty, t) -> Def (id, fty ty, term t)
    | Data l ->
        let l = List.map (map_data ~ty:fty) l in
        Data l
    | Goal f -> Goal (form f)
    | NegatedGoal l -> NegatedGoal (List.map form l)
    | Assert f -> Assert (form f)
    | TyDecl (id, ty) -> TyDecl (id, fty ty)
  in
  {st with view = map_view ~form ~term ~ty st.view; }

let map_src ~f st = {st with src=f st.src; }

(** {2 Iterators} *)

module Seq = struct
  let to_seq st k =
    let decl id ty = k (`ID id); k (`Ty ty) in
    match view st with
      | TyDecl (id,ty) -> decl id ty;
      | Def (id,ty,t) -> decl id ty; k (`Term t)
      | Data l ->
        List.iter
          (fun d ->
             decl d.data_id d.data_ty;
             List.iter (CCFun.uncurry decl) d.data_cstors)
          l
      | Assert f
      | Goal f -> k (`Form f)
      | NegatedGoal l -> List.iter (fun f -> k (`Form f)) l

  let ty_decls st k = match view st with
    | Def (id, ty, _)
    | TyDecl (id, ty) -> k (id,ty)
    | Data l ->
        List.iter
          (fun d ->
            k (d.data_id, d.data_ty);
            List.iter k d.data_cstors)
          l
    | Goal _
    | NegatedGoal _
    | Assert _ -> ()

  let forms st k = match view st with
    | Def _
    | Data _
    | TyDecl _ -> ()
    | Goal c -> k c
    | NegatedGoal l -> List.iter k l
    | Assert c -> k c

  let lits st = forms st |> Sequence.flat_map Sequence.of_list

  let terms st = lits st |> Sequence.flat_map SLiteral.to_seq

  let symbols st =
    to_seq st
    |> Sequence.flat_map
      (function
        | `ID id -> Sequence.return id
        | `Form f ->
          Sequence.of_list f
            |> Sequence.flat_map SLiteral.to_seq
            |> Sequence.flat_map FOTerm.Seq.symbols
        | `Term t -> FOTerm.Seq.symbols t
        | `Ty ty -> Type.Seq.symbols ty)
end

let signature ?(init=Signature.empty) seq =
  seq
  |> Sequence.flat_map Seq.ty_decls
  |> Sequence.fold (fun sigma (id,ty) -> Signature.declare sigma id ty) init

let add_src ~file st =
  map_src st
    ~f:(fun {UntypedAST.name;_} -> StatementSrc.make ?name file)

(** {2 IO} *)

let fpf = Format.fprintf

let pp ppf ppt ppty out st = match st.view with
  | TyDecl (id,ty) ->
      fpf out "@[<2>val %a :@ @[%a@]@]." ID.pp id ppty ty
  | Def (id,ty,t) ->
      fpf out "@[<2>def %a :@ @[%a@]@ := @[%a@]@]." ID.pp id ppty ty ppt t
  | Data l ->
      let pp_cstor out (id,ty) =
        fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp id ppty ty in
      let pp_data out d =
        fpf out "@[%a : %a@] :=@ @[<v>%a@]"
          ID.pp d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
      in
      fpf out "@[<v>data %a@]" (Util.pp_list ~sep:" and " pp_data) l
  | Assert f ->
      fpf out "@[<2>assert@ @[%a@]@]." ppf f
  | Goal f ->
      fpf out "@[<2>goal@ @[%a@]@]." ppf f
  | NegatedGoal l ->
      fpf out "@[<2>negated_goal@ @[<hv>%a@]@]."
        (Util.pp_list ~sep:", " (CCFormat.hovbox ppf)) l

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
        let role = "axiom" in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f
    | Goal f ->
        let role = "conjecture" in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f
    | NegatedGoal l ->
        let role = "negated_conjecture" in
        List.iter
          (fun f -> fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f)
          l
    | Def (id, ty, t) ->
        pp_decl out (id,ty);
        fpf out "@[<2>tff(%s, axiom,@ %a =@ @[%a@])@]." name ID.pp id ppt t
    | Data _ -> failwith "cannot print `data` to TPTP"

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

