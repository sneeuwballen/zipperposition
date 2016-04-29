
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

type attr =
  | A_AC

type attrs = attr list

type ('f, 't, 'ty) view =
  | TyDecl of ID.t * 'ty (** id: ty *)
  | Data of 'ty data list
  | Def of ID.t * 'ty * 't
  | RewriteTerm of ID.t * 'ty * 't list * 't (* args, rhs *)
  | RewriteForm of 't SLiteral.t * 'f list (* lhs atomic form, rhs conjunction *)
  | Assert of 'f (** assert form *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'f list (** goal after negation *)

type ('f, 't, 'ty, 'meta) t = {
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  src: 'meta; (** additional data *)
}

type ('f, 't, 'ty) sourced_t = ('f, 't, 'ty, StatementSrc.t) t

type clause = FOTerm.t SLiteral.t list
type clause_t = (clause, FOTerm.t, Type.t) sourced_t

let view t = t.view
let src t = t.src

let mk_data id ~args ty cstors =
  {data_id=id; data_args=args; data_ty=ty; data_cstors=cstors; }

let mk_ ?(attrs=[]) ~src view = {src; view; attrs; }

let ty_decl ?attrs ~src id ty = mk_ ?attrs ~src (TyDecl (id,ty))
let def ?attrs ~src id ty t = mk_ ?attrs ~src (Def (id,ty,t))
let rewrite_term ?attrs ~src id ty args rhs = mk_ ?attrs ~src (RewriteTerm(id,ty,args,rhs))
let rewrite_form ?attrs ~src lhs rhs = mk_ ?attrs ~src (RewriteForm(lhs,rhs))
let data ?attrs ~src l = mk_ ?attrs ~src (Data l)
let assert_ ?attrs ~src c = mk_ ?attrs ~src (Assert c)
let goal ?attrs ~src c = mk_ ?attrs ~src (Goal c)
let neg_goal ?attrs ~src l = mk_ ?attrs ~src (NegatedGoal l)

let map_data ~ty:fty d =
  { d with
    data_args = List.map (Var.update_ty ~f:fty) d.data_args;
    data_ty = fty d.data_ty;
    data_cstors = List.map (fun (id,ty) -> id, fty ty) d.data_cstors;
  }

let map ~form ~term ~ty st =
  let map_view ~form ~term ~ty:fty = function
    | Def (id, ty, t) -> Def (id, fty ty, term t)
    | RewriteTerm (id, ty, args, rhs) ->
      RewriteTerm (id, fty ty, List.map term args, term rhs)
    | RewriteForm (lhs,rhs) ->
      RewriteForm (SLiteral.map ~f:term lhs, List.map form rhs)
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

(** {2 Defined Constants} *)

exception Payload_defined_cst of int

let as_defined_cst id =
  CCList.find_map
    (function
      | Payload_defined_cst l -> Some l
      | _ -> None)
    (ID.payload id)

let is_defined_cst id = as_defined_cst id <> None

let declare_defined_cst id l =
  (* declare that [id] is a defined constant of level [l+1] *)
  Util.debugf ~section:Util.Section.zip 1 "declare %a as defined constant of level %d"
    (fun k->k ID.pp id l);
  ID.add_payload id (Payload_defined_cst l)

let scan_stmt_for_defined_cst st = match view st with
  | Def (id, _ty, def) ->
    let l =
      FOTerm.Seq.symbols def
      |> Sequence.filter_map as_defined_cst
      |> Sequence.max
      |> CCOpt.get 0
      |> succ
    in
    declare_defined_cst id l
  | _ -> ()

(** {2 Iterators} *)

module Seq = struct
  let to_seq st k =
    let decl id ty = k (`ID id); k (`Ty ty) in
    match view st with
      | TyDecl (id,ty) -> decl id ty;
      | Def (id,ty,t) -> decl id ty; k (`Term t)
      | RewriteTerm (_,ty,args,rhs) ->
        k (`Ty ty);
        List.iter (fun t -> k (`Term t)) args;
        k (`Term rhs)
      | RewriteForm (lhs,rhs) ->
        SLiteral.iter ~f:(fun t -> k (`Term t)) lhs;
        List.iter (fun f -> k (`Form f)) rhs
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
    | RewriteTerm _
    | RewriteForm _
    | Goal _
    | NegatedGoal _
    | Assert _ -> ()

  let forms st k = match view st with
    | Def _
    | RewriteTerm _
    | Data _
    | TyDecl _ -> ()
    | Goal c -> k c
    | RewriteForm (_, l)
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
    ~f:(fun l ->
      let name =
        CCList.find_map
          (function UntypedAST.A_name n-> Some n | _ -> None)
          l
      in
      StatementSrc.make ?name file)

(** {2 IO} *)

let fpf = Format.fprintf

let pp_attr out = function
  | A_AC -> fpf out "AC"

let pp_attrs out = function
  | [] -> ()
  | l -> fpf out "@ [@[%a@]]" (Util.pp_list ~sep:"," pp_attr) l

let pp ppf ppt ppty out st = match st.view with
  | TyDecl (id,ty) ->
      fpf out "@[<2>val%a %a :@ @[%a@]@]." pp_attrs st.attrs ID.pp id ppty ty
  | Def (id,ty,t) ->
      fpf out "@[<2>def%a %a :@ @[%a@]@ := @[%a@]@]." pp_attrs st.attrs ID.pp id ppty ty ppt t
  | RewriteTerm (id, _, args, rhs) ->
      fpf out "@[<2>rewrite%a @[%a %a@]@ = @[%a@]@]" pp_attrs st.attrs
        ID.pp id (Util.pp_list ~sep:" " ppt) args ppt rhs
  | RewriteForm (lhs, rhs) ->
      fpf out "@[<2>rewrite%a @[%a@]@ <=> @[%a@]@]" pp_attrs st.attrs
        (SLiteral.pp ppt) lhs (Util.pp_list ~sep:" && " ppf) rhs
  | Data l ->
      let pp_cstor out (id,ty) =
        fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp id ppty ty in
      let pp_data out d =
        fpf out "@[%a : %a@] :=@ @[<v>%a@]"
          ID.pp d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
      in
      fpf out "@[<hv>data%a@ %a@]" pp_attrs st.attrs (Util.pp_list ~sep:" and " pp_data) l
  | Assert f ->
      fpf out "@[<2>assert%a@ @[%a@]@]." pp_attrs st.attrs ppf f
  | Goal f ->
      fpf out "@[<2>goal%a@ @[%a@]@]." pp_attrs st.attrs ppf f
  | NegatedGoal l ->
      fpf out "@[<2>negated_goal%a@ @[<hv>%a@]@]." pp_attrs st.attrs
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
    | RewriteTerm (id, _, args, rhs) ->
        fpf out "@[<2>tff(%s, axiom,@ %a(%a) =@ @[%a@])@]."
          name ID.pp id (Util.pp_list ~sep:", " ppt) args ppt rhs
    | RewriteForm (lhs, rhs) ->
        fpf out "@[<2>tff(%s, axiom,@ %a <=>@ (@[%a@]))@]."
          name (SLiteral.TPTP.pp ppt) lhs (Util.pp_list ~sep:" & " ppf) rhs
    | Data _ -> failwith "cannot print `data` to TPTP"

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

