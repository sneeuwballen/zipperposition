
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
  data_cstors: (ID.t * 'ty * ('ty * (ID.t * 'ty)) list) list;
  (** Each constructor is [id, ty, args].
      [ty] must be of the form [ty1 -> ty2 -> ... -> id args].
      [args] has the form [(ty1, p1), (ty2,p2), …] where each [p]
      is a projector. *)
}

type attr =
  | A_AC
  | A_infix of string
  | A_prefix of string
  | A_sos (** set of support *)

type attrs = attr list

type 'ty skolem = ID.t * 'ty

(** polarity for rewrite rules *)
type polarity = [`Equiv | `Imply]

type ('f, 't, 'ty) def_rule =
  | Def_term of {
      vars: 'ty Var.t list;
      id: ID.t;
      ty: 'ty;
      args: 't list;
      rhs: 't;
      as_form: 'f;
    } (** [forall vars, id args = rhs] *)

  | Def_form of {
      vars: 'ty Var.t list;
      lhs: 't SLiteral.t;
      rhs: 'f list;
      polarity: polarity;
      as_form: 'f list;
    } (** [forall vars, lhs op bigand rhs] where [op] depends on
          [polarity] (in [{=>, <=>, <=}]) *)

type ('f, 't, 'ty) def = {
  def_id: ID.t;
  def_ty: 'ty; (* def_ty = def_vars -> def_ty_ret *)
  def_rules: ('f, 't, 'ty) def_rule list;
  def_rewrite: bool; (* rewrite rule or mere assertion? *)
}

type ('f, 't, 'ty) view =
  | TyDecl of ID.t * 'ty (** id: ty *)
  | Data of 'ty data list
  | Def of ('f, 't, 'ty) def list
  | Rewrite of ('f, 't, 'ty) def_rule
  | Assert of 'f (** assert form *)
  | Lemma of 'f list (** lemma to prove and use, using Avatar cut *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'ty skolem list * 'f list (** goal after negation, with skolems *)

type lit = Term.t SLiteral.t
type formula = TypedSTerm.t
type input_def = (TypedSTerm.t,TypedSTerm.t,TypedSTerm.t) def
type clause = lit list

type ('f, 't, 'ty) t = {
  id: int;
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  proof: proof;
  mutable name: string option;
}

and proof = Proof.Step.t
and input_t = (TypedSTerm.t, TypedSTerm.t, TypedSTerm.t) t
and clause_t = (clause, Term.t, Type.t) t

let compare a b = CCInt.compare a.id b.id
let view t = t.view
let attrs t = t.attrs
let proof_step t = t.proof

let mk_data id ~args ty cstors =
  {data_id=id; data_args=args; data_ty=ty; data_cstors=cstors; }

let mk_def ?(rewrite=false) id ty rules =
  { def_id=id; def_ty=ty; def_rules=rules; def_rewrite=rewrite; }

let id_n_ = ref 0
let mk_ ?(attrs=[]) ~proof view: (_,_,_) t =
  {id=CCRef.incr_then_get id_n_; proof; view; attrs; name=None}

let ty_decl ?attrs ~proof id ty = mk_ ?attrs ~proof (TyDecl (id,ty))
let def ?attrs ~proof l = mk_ ?attrs ~proof (Def l)
let rewrite ?attrs ~proof d = mk_ ?attrs ~proof (Rewrite d)
let data ?attrs ~proof l = mk_ ?attrs ~proof (Data l)
let assert_ ?attrs ~proof c = mk_ ?attrs ~proof (Assert c)
let lemma ?attrs ~proof l = mk_ ?attrs ~proof (Lemma l)
let goal ?attrs ~proof c = mk_ ?attrs ~proof (Goal c)
let neg_goal ?attrs ~proof ~skolems l = mk_ ?attrs ~proof (NegatedGoal (skolems, l))

let map_data ~ty:fty d =
  { d with
      data_args = List.map (Var.update_ty ~f:fty) d.data_args;
      data_ty = fty d.data_ty;
      data_cstors =
        List.map (fun (id,ty,args) ->
          id, fty ty, List.map (fun (ty,(p_id,p_ty)) -> fty ty,(p_id, fty p_ty)) args)
          d.data_cstors;
  }

let map_def_rule ~form:fform ~term:fterm ~ty:fty d = match d with
  | Def_term {vars;id;ty;args;rhs;as_form} ->
    let vars = List.map (Var.update_ty ~f:fty) vars in
    Def_term {vars;id;ty=fty ty;args=List.map fterm args;
              rhs=fterm rhs; as_form=fform as_form}
  | Def_form {vars;lhs;rhs;polarity;as_form} ->
    let vars = List.map (Var.update_ty ~f:fty) vars in
    Def_form {vars;lhs=SLiteral.map ~f:fterm lhs;
              rhs=List.map fform rhs;polarity;
              as_form=List.map fform as_form}

let map_def ~form:fform ~term:fterm ~ty:fty d =
  { d with
      def_ty=fty d.def_ty;
      def_rules=
        List.map (map_def_rule ~form:fform ~term:fterm ~ty:fty) d.def_rules;
  }

let map ~form ~term ~ty st =
  let map_view ~form ~term ~ty:fty = function
    | Def l ->
      let l = List.map (map_def ~form ~term ~ty) l in
      Def l
    | Rewrite d ->
      let d = map_def_rule ~form ~term ~ty d in
      Rewrite d
    | Data l ->
      let l = List.map (map_data ~ty:fty) l in
      Data l
    | Lemma l -> Lemma (List.map form l)
    | Goal f -> Goal (form f)
    | NegatedGoal (sk,l) ->
      let sk = List.map (fun (i,ty)->i, fty ty) sk in
      NegatedGoal (sk,List.map form l)
    | Assert f -> Assert (form f)
    | TyDecl (id, ty) -> TyDecl (id, fty ty)
  in
  {st with view = map_view ~form ~term ~ty st.view; }

(** {2 Defined Constants} *)

type definition = Rewrite.rule_set

let as_defined_cst id =
  ID.payload_find id
    ~f:(function
      | Rewrite.Payload_defined_cst c ->
        Some (Rewrite.Defined_cst.level c, Rewrite.Defined_cst.rules c)
      | _ -> None)

let as_defined_cst_level id = CCOpt.map fst @@ as_defined_cst id

let is_defined_cst id = as_defined_cst id <> None

let declare_defined_cst id ~level (rules:definition) : unit =
  let _ = Rewrite.Defined_cst.declare ~level id rules in
  ()

let conv_rule ~proof (r:_ def_rule) : Rewrite.rule = match r with
  | Def_term {id;ty;args;rhs;_} ->
    let rhs = Lambda.snf rhs in
    Rewrite.T_rule (Rewrite.Term.Rule.make id ty args rhs ~proof)
  | Def_form {lhs;rhs;_} ->
    (* returns either a term or a lit rule (depending on whether RHS is atomic) *)
    let lhs = Literal.Conv.of_form lhs in
    let rhs = List.map (List.map Literal.Conv.of_form) rhs in
    Rewrite.Rule.make_lit lhs rhs ~proof

(* convert rules *)
let conv_rules (l:_ def_rule list) proof : definition =
  assert (l <> []);
  List.map (conv_rule ~proof) l
  |> Rewrite.Rule_set.of_list

let terms_of_rule (d:_ def_rule): _ Sequence.t = match d with
  | Def_term {args;rhs;_} ->
    Sequence.of_list (rhs::args)
  | Def_form {lhs;rhs;_} ->
    Sequence.cons lhs (Sequence.of_list rhs |> Sequence.flat_map Sequence.of_list)
    |> Sequence.flat_map SLiteral.to_seq

let level_of_rule (d:_ def_rule): int =
  terms_of_rule d
  |> Sequence.flat_map Term.Seq.symbols
  |> Sequence.filter_map as_defined_cst_level
  |> Sequence.max
  |> CCOpt.get_or ~default:0

(** {2 Inductive Types} *)

(* add rewrite rules for functions associated
   with this datatype (projectors, etc.) *)
let decl_data_functions ity proof : unit =
  let one_cstor = List.length ity.Ind_ty.ty_constructors = 1 in
  List.iter
    (fun cstor ->
       (* projectors *)
       List.iter
         (fun (_, proj) -> Rewrite.Defined_cst.declare_proj ~proof proj)
         cstor.Ind_ty.cstor_args;
       (* if there is exactly one cstor, add [cstor (proj_1 x)…(proj_n x) --> x] *)
       if one_cstor then (
         Rewrite.Defined_cst.declare_cstor ~proof cstor
       );)
    ity.Ind_ty.ty_constructors

(** {2 Iterators} *)

module Seq = struct
  let mk_term t = `Term t
  let mk_form f = `Form f

  let seq_of_rule (d:_ def_rule): _ Sequence.t = fun k -> match d with
    | Def_term {vars; ty; args; rhs; _} ->
      k (`Ty ty);
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      List.iter (fun t->k (`Term t)) (rhs::args);
    | Def_form {vars;lhs;rhs;_} ->
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      SLiteral.to_seq lhs |> Sequence.map mk_term |> Sequence.iter k;
      Sequence.of_list rhs |> Sequence.map mk_form |> Sequence.iter k

  let to_seq st k =
    let decl id ty = k (`ID id); k (`Ty ty) in
    begin match view st with
      | TyDecl (id,ty) -> decl id ty;
      | Def l ->
        List.iter
          (fun {def_id; def_ty; def_rules; _} ->
             decl def_id def_ty;
             Sequence.of_list def_rules
             |> Sequence.flat_map seq_of_rule |> Sequence.iter k)
          l
      | Rewrite d ->
        begin match d with
          | Def_term {ty;args;rhs;_}  ->
            k (`Ty ty);
            List.iter (fun t -> k (`Term t)) args;
            k (`Term rhs)
          | Def_form {lhs;rhs;_} ->
            SLiteral.iter ~f:(fun t -> k (`Term t)) lhs;
            List.iter (fun f -> k (`Form f)) rhs
        end
      | Data l ->
        let decl_cstor (id,ty,args) =
          decl id ty;
          List.iter (fun (_,(p_id,p_ty)) -> decl p_id p_ty) args;
        in
        List.iter
          (fun d ->
             decl d.data_id d.data_ty;
             List.iter decl_cstor d.data_cstors)
          l
      | Lemma l -> List.iter (fun f -> k (`Form f)) l
      | Assert f
      | Goal f -> k (`Form f)
      | NegatedGoal (_,l) -> List.iter (fun f -> k (`Form f)) l
    end

  let ty_decls st k = match view st with
    | Def l ->
      List.iter
        (fun {def_id; def_ty; _} -> k (def_id, def_ty))
        l
    | TyDecl (id, ty) -> k (id,ty)
    | Data l ->
      let decl_cstor (id,ty,args) =
        k(id,ty);
        List.iter (fun (_,p) -> k p) args;
      in
      List.iter
        (fun d ->
           k (d.data_id, d.data_ty);
           List.iter decl_cstor d.data_cstors)
        l
    | Rewrite _
    | Lemma _
    | Goal _
    | NegatedGoal _
    | Assert _ -> ()

  let forms st =
    let forms_def = function
      | Def_term {as_form;_} -> [as_form]
      | Def_form {as_form;_} -> as_form
    in
    begin match view st with
      | Rewrite d -> Sequence.of_list (forms_def d)
      | Def l ->
        Sequence.of_list l
        |> Sequence.flat_map_l (fun d -> d.def_rules)
        |> Sequence.flat_map_l forms_def
      | _ ->
        to_seq st
        |> Sequence.filter_map (function `Form f -> Some f | _ -> None)
    end

  let lits st = forms st |> Sequence.flat_map Sequence.of_list

  let terms st =
    to_seq st
    |> Sequence.flat_map
      (function
        | `Form f -> Sequence.of_list f |> Sequence.flat_map SLiteral.to_seq
        | `Term t -> Sequence.return t
        | _ -> Sequence.empty)

  let symbols st =
    to_seq st
    |> Sequence.flat_map
      (function
        | `ID id -> Sequence.return id
        | `Form f ->
          Sequence.of_list f
          |> Sequence.flat_map SLiteral.to_seq
          |> Sequence.flat_map Term.Seq.symbols
        | `Term t -> Term.Seq.symbols t
        | `Ty ty -> Type.Seq.symbols ty)
end

let signature ?(init=Signature.empty) seq =
  seq
  |> Sequence.flat_map Seq.ty_decls
  |> Sequence.fold (fun sigma (id,ty) -> Signature.declare sigma id ty) init

let conv_attrs =
  let module A = UntypedAST in
  CCList.filter_map
    (function
      | A.A_app (("ac" | "AC"), []) -> Some A_AC
      | A.A_app (("sos" | "SOS"), []) -> Some A_sos
      | A.A_app ("infix", [A.A_quoted s]) -> Some (A_infix s)
      | A.A_app ("prefix", [A.A_quoted s]) -> Some (A_prefix s)
      | _ -> None)

let attr_to_ua : attr -> UntypedAST.attr =
  let open UntypedAST.A in
  function
    | A_AC -> str "AC"
    | A_sos -> str "sos"
    | A_prefix s -> app "prefix" [quoted s]
    | A_infix s -> app "infix" [quoted s]

(** {2 IO} *)

let fpf = Format.fprintf

let pp_attr out a = UntypedAST.pp_attr out (attr_to_ua a)
let pp_attrs out l = UntypedAST.pp_attrs out (List.map attr_to_ua l)

let pp_typedvar ppty out v = fpf out "(@[%a:%a@])" Var.pp v ppty (Var.ty v)
let pp_typedvar_l ppty = Util.pp_list ~sep:" " (pp_typedvar ppty)

let pp_def_rule ppf ppt ppty out d =
  let pp_arg = CCFormat.within "(" ")" ppt in
  let pp_args out = function
    | [] -> ()
    | l -> fpf out "@ %a" (Util.pp_list ~sep:" " pp_arg) l
  and pp_vars out = function
    | [] -> ()
    | l -> fpf out "forall %a.@ " (pp_typedvar_l ppty) l
  in
  begin match d with
    | Def_term {vars;id;args;rhs;_} ->
      fpf out "@[<2>%a@[<2>%a%a@] =@ %a@]"
        pp_vars vars ID.pp id pp_args args ppt rhs
    | Def_form {vars;lhs;rhs;polarity=pol;_} ->
      let op = match pol with `Equiv-> "<=>" | `Imply -> "=>" in
      fpf out "@[<2>%a%a %s@ (@[<hv>%a@])@]"
        pp_vars vars
        (SLiteral.pp ppt) lhs
        op
        (Util.pp_list ~sep:" && " ppf) rhs
  end

let pp_def ppf ppt ppty out d =
  fpf out "@[<2>@[%a : %a@]@ where@ @[<hv>%a@]@]"
    ID.pp d.def_id ppty d.def_ty
    (Util.pp_list ~sep:";" (pp_def_rule ppf ppt ppty)) d.def_rules

let pp_input_def = pp_def TypedSTerm.pp TypedSTerm.pp TypedSTerm.pp

let attrs_ua st =
  let src_attrs = Proof.Step.to_attrs st.proof in
  List.rev_append src_attrs (List.map attr_to_ua st.attrs)

let pp ppf ppt ppty out st =
  let attrs = attrs_ua st in
  let pp_attrs = UntypedAST.pp_attrs in
  begin match st.view with
    | TyDecl (id,ty) ->
      fpf out "@[<2>val%a %a :@ @[%a@]@]." pp_attrs attrs ID.pp id ppty ty
    | Def l ->
      fpf out "@[<2>def%a@ %a@]."
        pp_attrs attrs (Util.pp_list ~sep:" and " (pp_def ppf ppt ppty)) l
    | Rewrite d ->
      begin match d with
        | Def_term {id;args;rhs;_} ->
          fpf out "@[<2>rewrite%a@ @[%a %a@]@ = @[%a@]@]." pp_attrs attrs
            ID.pp id (Util.pp_list ~sep:" " ppt) args ppt rhs
        | Def_form {lhs;rhs;polarity=pol;_} ->
          let op = match pol with `Equiv-> "<=>" | `Imply -> "=>" in
          fpf out "@[<2>rewrite%a@ @[%a@]@ %s @[%a@]@]." pp_attrs attrs
            (SLiteral.pp ppt) lhs op (Util.pp_list ~sep:" && " ppf) rhs
      end
    | Data l ->
      let pp_cstor out (id,ty,_) =
        fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp id ppty ty in
      let pp_data out d =
        fpf out "@[<hv2>@[%a : %a@] :=@ %a@]"
          ID.pp d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
      in
      fpf out "@[<hv2>data%a@ %a@]." pp_attrs attrs (Util.pp_list ~sep:" and " pp_data) l
    | Assert f ->
      fpf out "@[<2>assert%a@ @[%a@]@]." pp_attrs attrs ppf f
    | Lemma l ->
      fpf out "@[<2>lemma%a@ @[%a@]@]."
        pp_attrs attrs (Util.pp_list ~sep:" && " ppf) l
    | Goal f ->
      fpf out "@[<2>goal%a@ @[%a@]@]." pp_attrs attrs ppf f
    | NegatedGoal (sk, l) ->
      let pp_sk out (id,ty) = fpf out "(%a:%a)" ID.pp id ppty ty in
      fpf out "@[<hv2>negated_goal%a@ @[<hv>%a@]@ # skolems: [@[<hv>%a@]]@]."
        pp_attrs attrs
        (Util.pp_list ~sep:", " (CCFormat.hovbox ppf)) l
        (Util.pp_list pp_sk) sk
  end

let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)

let pp_clause =
  pp (Util.pp_list ~sep:" ∨ " (SLiteral.pp Term.pp)) Term.pp Type.pp

let pp_input = pp TypedSTerm.pp TypedSTerm.pp TypedSTerm.pp

let name_gen_ =
  let n = ref 0 in
  fun () -> Printf.sprintf "zf_stmt_%d" (CCRef.get_then_incr n)

let name (st:(_,_,_)t) : string =
  let from_src = match Proof.Step.src st.proof with
    | Some {Proof.src_view=Proof.From_file (f,_);_} -> Proof.Src.name f
    | _ -> None
  in
  begin match st.name, from_src with
    | Some s, _ -> s
    | None, Some s -> s
    | None, None ->
      let s = name_gen_ () in
      st.name <- Some s;
      s
  end

module ZF = struct
  module UA = UntypedAST.A

  let pp ppf ppt ppty out st =
    let pp_var out v= Format.fprintf out "(%a:%a)" Var.pp v ppty (Var.ty v) in
    let pp_vars out = function
      | [] -> ()
      | vars -> Format.fprintf out "forall %a.@ " (Util.pp_list ~sep:" " pp_var) vars
    in
    let attrs = attrs_ua st in
    let pp_attrs = UntypedAST.pp_attrs_zf in
    match st.view with
      | TyDecl (id,ty) ->
        fpf out "@[<2>val%a %a :@ @[%a@]@]." pp_attrs attrs ID.pp_zf id ppty ty
      | Def l ->
        fpf out "@[<2>def%a@ %a@]."
          pp_attrs attrs (Util.pp_list ~sep:" and " (pp_def ppf ppt ppty)) l
      | Rewrite d ->
        begin match d with
          | Def_term {vars;id;args;rhs;_} ->
            fpf out "@[<2>rewrite%a@ @[<2>%a@[%a %a@]@ = @[%a@]@]@]." pp_attrs attrs
              pp_vars vars ID.pp_zf id (Util.pp_list ~sep:" " ppt) args ppt rhs
          | Def_form {vars;lhs;rhs;polarity=pol;_} ->
            let op = match pol with `Equiv-> "<=>" | `Imply -> "=>" in
            fpf out "@[<2>rewrite%a@ @[<2>%a@[%a@]@ %s @[%a@]@]@]." pp_attrs attrs
              pp_vars vars (SLiteral.ZF.pp ppt) lhs op
              (Util.pp_list ~sep:" && " ppf) rhs
        end
      | Data l ->
        let pp_cstor out (id,ty,_) =
          fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp_zf id ppty ty in
        let pp_data out d =
          fpf out "@[<hv2>@[%a : %a@] :=@ %a@]"
            ID.pp_zf d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
        in
        fpf out "@[<hv2>data%a@ %a@]." pp_attrs attrs (Util.pp_list ~sep:" and " pp_data) l
      | Assert f ->
        fpf out "@[<2>assert%a@ @[%a@]@]." pp_attrs attrs ppf f
      | Lemma l ->
        fpf out "@[<2>lemma%a@ @[%a@]@]."
          pp_attrs attrs (Util.pp_list ~sep:" && " ppf) l
      | Goal f ->
        fpf out "@[<2>goal%a@ @[%a@]@]." pp_attrs attrs ppf f
      | NegatedGoal (_, l) ->
        fpf out "@[<hv2>goal%a@ ~(@[<hv>%a@])@]."
          pp_attrs attrs
          (Util.pp_list ~sep:", " (CCFormat.hovbox ppf)) l

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

module TPTP = struct
  let pp ppf ppt ppty out st =
    let name = name st in
    let pp_decl out (id,ty) =
      if ID.is_distinct_object id
      then
        fpf out "%% (omitted type declaration for distinct object %a.)" ID.pp_tstp id
      else
        fpf out "tff(@[%s, type,@ %a :@ @[%a@]@])." name ID.pp_tstp id ppty ty
    and pp_quant_vars out = function
      | [] -> ()
      | l ->
        let pp_typedvar out v =
          fpf out "%a:%a" Var.pp v ppty (Var.ty v)
        in
        fpf out "![@[%a@]]:@ " (Util.pp_list pp_typedvar) l
    in
    let pp_name = Util.pp_str_tstp in
    (* print a single definition as an axiom *)
    let pp_def_axiom out d =
      let pp_args out = function
        | [] -> ()
        | l -> fpf out "(@[%a@])" (Util.pp_list ~sep:"," ppt) l
      in
      let pp_rule out = function
        | Def_term {vars;id;args;rhs;_} ->
          fpf out "%a(@[%a%a@] =@ %a)" pp_quant_vars vars ID.pp_tstp id pp_args args ppt rhs
        | Def_form {vars;lhs;rhs;polarity=pol;_} ->
          let op = match pol with `Equiv-> "<=>" | `Imply -> "=>" in
          fpf out "%a(@[%a@] %s@ (@[<hv>%a@]))"
            pp_quant_vars vars (SLiteral.pp ppt) lhs op
            (Util.pp_list ~sep:" & " ppf) rhs
      in
      let pp_top_rule out r =
        fpf out "@[<2>tff(%s, axiom,@ %a)@]." name pp_rule r
      in
      Util.pp_list ~sep:"" pp_top_rule out d.def_rules
    in
    match st.view with
      | TyDecl (id,ty) -> pp_decl out (id,ty)
      | Assert f ->
        let role = "axiom" in
        fpf out "@[<2>tff(%a, %s,@ (@[%a@]))@]." pp_name name role ppf f
      | Lemma l ->
        let role = "lemma" in
        fpf out "@[<2>tff(%a, %s,@ (@[%a@]))@]." pp_name name role
          (Util.pp_list ~sep:" & " ppf) l
      | Goal f ->
        let role = "conjecture" in
        fpf out "@[<2>tff(%a, %s,@ (@[%a@]))@]." pp_name name role ppf f
      | NegatedGoal (_,l) ->
        let role = "negated_conjecture" in
        List.iter
          (fun f ->
             fpf out "@[<2>tff(%a, %s,@ (@[%a@]))@]." pp_name name role ppf f)
          l
      | Def l ->
        Format.fprintf out "@[<v>";
        (* declare *)
        List.iter
          (fun {def_id; def_ty; _} -> Format.fprintf out "%a@," pp_decl (def_id,def_ty))
          l;
        (* define *)
        Util.pp_list ~sep:"" pp_def_axiom out l;
        Format.fprintf out "@]";
      | Rewrite d ->
        begin match d with
          | Def_term {id;args;rhs;_} ->
            fpf out "@[<2>tff(%a, axiom,@ %a(%a) =@ @[%a@])@]."
              pp_name name ID.pp_tstp id (Util.pp_list ~sep:", " ppt) args ppt rhs
          | Def_form {lhs;rhs;polarity=pol;_} ->
            let op = match pol with `Equiv-> "<=>" | `Imply -> "=>" in
            fpf out "@[<2>tff(%a, axiom,@ %a %s@ (@[%a@]))@]."
              pp_name name (SLiteral.TPTP.pp ppt) lhs op
              (Util.pp_list ~sep:" & " ppf) rhs
        end
      | Data _ -> failwith "cannot print `data` to TPTP"

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

let pp_in pp_f pp_t pp_ty = function
  | Output_format.O_zf -> ZF.pp pp_f pp_t pp_ty
  | Output_format.O_tptp -> TPTP.pp pp_f pp_t pp_ty
  | Output_format.O_normal -> pp pp_f pp_t pp_ty
  | Output_format.O_none -> CCFormat.silent

let pp_clause_in o =
  let pp_t = Term.pp_in o in
  let pp_ty = Type.pp_in o in
  pp_in (Util.pp_list ~sep:" ∨ " (SLiteral.pp_in o pp_t)) pp_t pp_ty o

let pp_input_in o =
  let pp_t = TypedSTerm.pp_in o in
  pp_in pp_t pp_t pp_t o

(** {2 Proofs} *)

exception E_i of input_t
exception E_c of clause_t

let res_tc_i : input_t Proof.result_tc =
  Proof.Result.make_tc
    ~of_exn:(function E_i c -> Some c | _ -> None)
    ~to_exn:(fun i -> E_i i)
    ~compare:compare
    ~pp_in:pp_input_in
    ~is_stmt:true
    ~name
    ~to_form:(fun ~ctx:_ st ->
      Seq.forms st |> Sequence.to_list |> TypedSTerm.Form.and_)
    ()

let res_tc_c : clause_t Proof.result_tc =
  Proof.Result.make_tc
    ~of_exn:(function E_c c -> Some c | _ -> None)
    ~to_exn:(fun i -> E_c i)
    ~compare:compare
    ~pp_in:pp_clause_in
    ~is_stmt:true
    ~name
    ~to_form:(fun ~ctx st ->
      let module F = TypedSTerm.Form in
      let conv_c (c:clause) : formula =
        c
        |> List.rev_map
          (fun lit ->
             SLiteral.map lit ~f:(Term.Conv.to_simple_term ctx)
             |> SLiteral.to_form)
        |> F.or_
        |> F.close_forall
      in
      Seq.forms st
      |> Sequence.map conv_c
      |> Sequence.to_list
      |> F.and_)
    ()

let as_proof_i t = Proof.S.mk t.proof (Proof.Result.make res_tc_i t)
let as_proof_c t = Proof.S.mk t.proof (Proof.Result.make res_tc_c t)

(** {2 Scanning} *)

let scan_stmt_for_defined_cst (st:(clause,Term.t,Type.t) t): unit = match view st with
  | Def [] -> assert false
  | Def l ->
    (* define all IDs at the same level (the max of those computed) *)
    let proof = as_proof_c st in
    let ids_and_levels =
      l
      |> List.filter
        (fun {def_ty=ty; def_rewrite=b; _} ->
           (* definitions require [b=true] or the LHS be a constant *)
           let _, args, _ = Type.open_poly_fun ty in
           b || CCList.is_empty args)
      |> List.map
        (fun {def_id; def_rules; _} ->
           let lev =
             Sequence.of_list def_rules
             |> Sequence.map level_of_rule
             |> Sequence.max
             |> CCOpt.get_or ~default:0
           and def =
             conv_rules def_rules proof
           in
           def_id, lev, def)
    in
    let level =
      Sequence.of_list ids_and_levels
      |> Sequence.map (fun (_,l,_) -> l)
      |> Sequence.max |> CCOpt.map_or ~default:0 succ
    in
    List.iter
      (fun (id,_,def) ->
         let _ = Rewrite.Defined_cst.declare ~level id def in
         ())
      ids_and_levels
  | Rewrite d ->
    let proof = as_proof_c st in
    let r = conv_rule ~proof d in
    begin match r with
      | Rewrite.T_rule r ->
        let id = Rewrite.Term.Rule.head_id r in
        Rewrite.Defined_cst.declare_or_add id (Rewrite.T_rule r)
      | Rewrite.L_rule lr ->
        begin match Rewrite.Lit.Rule.head_id lr with
          | Some id ->
            Rewrite.Defined_cst.declare_or_add id r
          | None ->
            assert (Rewrite.Lit.Rule.is_equational lr);
            Rewrite.Defined_cst.add_eq_rule lr
        end
    end
  | _ -> ()

let scan_stmt_for_ind_ty st = match view st with
  | Data l ->
    let proof = as_proof_c st in
    List.iter
      (fun d ->
         let ty_vars =
           List.mapi (fun i v -> HVar.make ~ty:(Var.ty v) i) d.data_args
         and cstors =
           List.map
             (fun (c,ty,args) -> Ind_ty.mk_constructor c ty args)
             d.data_cstors
         in
         let ity = Ind_ty.declare_ty d.data_id ~ty_vars cstors ~proof in
         decl_data_functions ity proof;
         ())
      l
  | _ -> ()

let scan_simple_stmt_for_ind_ty st = match view st with
  | Data l ->
    let conv = Type.Conv.create() in
    let conv_ty = Type.Conv.of_simple_term_exn conv in
    let proof = as_proof_i st in
    List.iter
      (fun d ->
         let ty_vars =
           List.mapi (fun i v -> HVar.make ~ty:(Var.ty v |> conv_ty) i) d.data_args
         and cstors =
           List.map
             (fun (c,ty,args) ->
                let args =
                  List.map
                    (fun (ty,(p_id,p_ty)) -> conv_ty ty, (p_id, conv_ty p_ty))
                    args in
                Ind_ty.mk_constructor c (conv_ty ty) args)
             d.data_cstors
         in
         let ity = Ind_ty.declare_ty d.data_id ~ty_vars cstors ~proof in
         decl_data_functions ity proof;
         ())
      l
  | _ -> ()
