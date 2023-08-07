
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement} *)

module OptionSet = Set.Make(
  struct
    let compare x y = Pervasives.compare x y
    type t = int option
  end)

module IdMap = ID.Map
module US = Unif_subst
module TST = TypedSTerm

let section = Util.Section.make "stm"


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

let map_def_rule ~form:fform ~term:fterm ~ty:fty d = 
  match d with
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

let conv_rule_i ~proof (r:_ def_rule) = match r with
  | Def_term {id;ty;args;rhs;_} ->
    let ctx = Type.Conv.create () in
    let ty = Type.Conv.of_simple_term_exn ctx ty in
    let args = List.map (Term.Conv.of_simple_term_exn ctx) args in
    let rhs = Lambda.snf (Term.Conv.of_simple_term_exn ctx rhs) in
    let rhs_rewritten, rw_rules = Rewrite.Term.normalize_term rhs in
       let proof_parents = Proof.Parent.from proof :: Rewrite.Rule.set_as_proof_parents rw_rules in
       let form = Proof.Result.to_form (Proof.S.result proof) in
       let proof = Proof.S.mk_f (Proof.Step.simp ~rule:(Proof.Rule.mk "simplify_rw_rule") proof_parents) form in
    let rule = Rewrite.Term.Rule.make id ty args (rhs) ~proof in
    Rewrite.T_rule rule
  | Def_form {lhs;rhs;_} ->
    let ctx = Type.Conv.create () in
    match lhs with 
    | SLiteral.Atom(lhs, true) -> 
      let lhs = Term.Conv.of_simple_term_exn ctx lhs in
      let hd, args = Term.as_app lhs in
      let ty = Term.ty hd in
      if List.length rhs = 1 then (
        let rhs = Term.Conv.of_simple_term_exn ctx (List.hd rhs) in
        let rule = Rewrite.Term.Rule.make (Term.as_const_exn hd) ty args rhs ~proof in
        Rewrite.T_rule rule
      ) else invalid_arg "rhs must be a singleton list."
    | _ -> 
      CCFormat.printf "@[%a@]@." (SLiteral.pp TST.pp) lhs;
      invalid_arg "only positive polarity is supported."

(* convert rules *)
let conv_rules (l:_ def_rule list) proof : definition =
  assert (l <> []);
  List.map (conv_rule ~proof) l
  |> Rewrite.Rule_set.of_list

let terms_of_rule (d:_ def_rule): _ Iter.t = match d with
  | Def_term {args;rhs;_} ->
    Iter.of_list (rhs::args)
  | Def_form {lhs;rhs;_} ->
    Iter.cons lhs (Iter.of_list rhs |> Iter.flat_map Iter.of_list)
    |> Iter.flat_map SLiteral.to_iter

let level_of_rule (d:_ def_rule): int =
  terms_of_rule d
  |> Iter.flat_map Term.Seq.symbols
  |> Iter.filter_map as_defined_cst_level
  |> Iter.max
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

let get_formulas_from_defs st =
  let get_from_rule rule =
    match rule with
    | Def_term {vars;id;ty;args;rhs;as_form} -> 
      ignore(vars,id,ty,args,rhs);
      [as_form]
    | Def_form {vars;lhs;rhs;polarity;as_form} -> 
      ignore(vars,lhs,polarity,rhs);
      as_form in


  match view st with 
  | Def defs -> CCList.flat_map (fun d -> CCList.flat_map get_from_rule d.def_rules) defs
  | Rewrite def_rule  -> get_from_rule def_rule
  | _ -> []




(** {2 Iterators} *)

module Seq = struct
  let mk_term t = `Term t
  let mk_form f = `Form f

  let seq_of_rule (d:_ def_rule): _ Iter.t = fun k -> match d with
    | Def_term {vars; ty; args; rhs; _} ->
      k (`Ty ty);
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      List.iter (fun t->k (`Term t)) (rhs::args);
    | Def_form {vars;lhs;rhs;_} ->
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      SLiteral.to_iter lhs |> Iter.map mk_term |> Iter.iter k;
      Iter.of_list rhs |> Iter.map mk_form |> Iter.iter k

  let to_iter st k =
    let decl id ty = k (`ID id); k (`Ty ty) in
    begin match view st with
      | TyDecl (id,ty) -> decl id ty;
      | Def l ->
        List.iter
          (fun {def_id; def_ty; def_rules; _} ->
             decl def_id def_ty;
             Iter.of_list def_rules
             |> Iter.flat_map seq_of_rule |> Iter.iter k)
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
      | Rewrite d -> Iter.of_list (forms_def d)
      | Def l ->
        Iter.of_list l
        |> Iter.flat_map_l (fun d -> d.def_rules)
        |> Iter.flat_map_l forms_def
      | _ ->
        to_iter st
        |> Iter.filter_map (function `Form f -> Some f | _ -> None)
    end

  let lits st = forms st |> Iter.flat_map Iter.of_list

  let terms st =
    to_iter st
    |> Iter.flat_map
      (function
        | `Form f -> Iter.of_list f |> Iter.flat_map SLiteral.to_iter
        | `Term t -> Iter.return t
        | _ -> Iter.empty)

  let symbols st =
    to_iter st
    |> Iter.flat_map
      (function
        | `ID id -> Iter.return id
        | `Form f ->
          Iter.of_list f
          |> Iter.flat_map SLiteral.to_iter
          |> Iter.flat_map Term.Seq.symbols
        | `Term t -> Term.Seq.symbols t
        | `Ty ty -> Type.Seq.symbols ty)
end

let signature ?(init=Signature.empty) ?(conj_syms=Iter.empty) seq =
  let signtr =
    seq
    |> Iter.flat_map Seq.ty_decls
    |> Iter.fold (fun sigma (id,ty) -> Signature.declare sigma id ty) init in
  conj_syms
  |> Iter.fold (fun sigma symb -> Signature.set_sym_in_conj symb sigma) signtr

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


module AsKey = struct
  type t = input_t
  let equal a b = compare a b = 0
  let hash a = a.id
  let compare = compare
end

module InpStmSet = Set.Make(AsKey)
module RW = Rewrite
module DC = RW.Defined_cst

let eliminate_long_implications ?(is_goal=false) f =
  let _elim_long_imps f =
    let rec aux f =
      match TST.Form.view f with
      | TST.Form.Imply(lhs, rhs) ->
        let is_form t = 
          match TST.Form.view t with 
          | Atom _ -> false
          | _ -> true in
        if not (is_form lhs) then (
          let premises, concl = aux rhs in
          lhs::premises, concl
        ) else ([], f)
      | _ -> ([], f) in
    let premises, concl = aux f in
    if CCList.length premises > 5 
    then (
      Util.debugf ~section 2 "trimmed @[%a@] into @[%a@]@." 
        (fun k -> k TST.pp f TST.pp concl);
      concl)
    else f in
  
  if not is_goal then f
  else _elim_long_imps f

let sine_axiom_selector
  ?(ignore_k_most_common_symbols=None)
  ?(take_conj_defs=true)
  ?(take_only_defs=false) 
  ?(trim_implications=false) 
  ?(depth_start=1) 
  ?(depth_end=3) 
  ?(tolerance=2.0) formulas =
  let formulas = Iter.to_list formulas in

  let symset_of_ax ~trim_implications ?(is_goal=false) ax =
     Seq.forms ax
    |> Iter.map 
      (if trim_implications 
       then eliminate_long_implications ~is_goal 
       else CCFun.id)
    |> Iter.flat_map TST.Seq.symbols
    |> ID.Set.of_iter in

  let symset_of_axs ~trim_implications ?(is_goal=false) axs =
    List.fold_left (fun acc c -> 
      ID.Set.union acc (symset_of_ax ~trim_implications ~is_goal c)) 
    ID.Set.empty axs in

  let triggered_by_syms ~triggers syms =
    ID.Set.fold (fun id acc -> 
        let axs = ID.Tbl.get_or triggers id ~default:InpStmSet.empty in
        (InpStmSet.elements axs) @ acc) 
      syms [] in

  let count_occ ~tbl ax = 
    symset_of_ax ~trim_implications:false ax
    |> ID.Set.iter (fun k ->
        ID.Tbl.update tbl ~f:(fun _ vopt -> match vopt with
            | Some v -> Some (v+1)
            | None -> Some 1) ~k) in

  let create_trigger_map ~trim_implications ~tbl axioms = 
    let map = ID.Tbl.create (Iter.length @@ ID.Tbl.keys tbl) in
    CCList.iter (fun ax ->
      let symset = ID.Set.to_iter @@ symset_of_ax ~trim_implications:false ax in
      let min_occ = ref (max_int) in
      Iter.iter (fun id -> 
          let cnt = ID.Tbl.get_or tbl id ~default:max_int in
          if cnt < !min_occ && (not trim_implications || cnt != 1) then min_occ := cnt;
        ) symset;
      (* now we calculate trigger map based on the min_occ *)
      let threshold = int_of_float @@ tolerance *. (float_of_int !min_occ) in
      Iter.iter (fun id -> 
          let cnt = ID.Tbl.get_or tbl id ~default:max_int in
          if cnt <= threshold then (
            ID.Tbl.update ~f:(fun k vopt -> 
                match vopt with 
                | Some ax_set -> Some (InpStmSet.add ax ax_set)
                | None -> Some (InpStmSet.singleton ax)) ~k:id map)) symset) 
      axioms;
    map in

  let ids_to_defs_compute defs =
    let rec aux map d = 
      let update_map map id stm =
        let prev = ID.Map.get_or ~default:InpStmSet.empty id map in
        ID.Map.add id (InpStmSet.add stm prev) map in

      match view d with
      | Def l ->
        List.fold_left
          (fun map {def_id; _} -> update_map map def_id d)
        map l;
      | Rewrite r ->
          begin match r with 
          | Def_term {id;_} -> update_map map id d
          | Def_form {lhs;rhs;polarity=pol;_} ->
            begin match lhs with 
            | Atom(t,_) -> 
              begin match TST.head t with 
              | Some hd -> update_map map hd d
              | None -> map 
              end
            | _ -> map 
            end
          end
      | _ -> map in
    List.fold_left (fun map d -> aux map d) ID.Map.empty defs in


  let categorize_formulas forms =
    let rec do_categorize (defs, helpers, axioms, conjs) f =
      match view f with 
      | Def _ | Rewrite _ -> (f::defs, helpers, axioms, conjs)
      | Assert _ -> (defs, helpers, f::axioms, conjs)
      | Goal _ | NegatedGoal _ -> (defs, helpers, axioms, f :: conjs)
      | _ -> (defs, f::helpers, axioms, conjs) in
    
    let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (do_categorize acc x) xs in
    
    aux ([],[],[],[]) forms in

  let defs,helper_axioms,axioms,goals = 
    categorize_formulas formulas in

  let axioms = if take_only_defs then defs else defs @ axioms in

  let ids_to_defs = ids_to_defs_compute defs in
  
  let tbl = ID.Tbl.create 1024 in
  List.iter (count_occ ~tbl) axioms;

  let most_commmon_syms = 
    match ignore_k_most_common_symbols with
    | None -> ID.Set.empty
    | Some k ->
      ID.Tbl.to_list tbl
      |> CCList.sort (fun (s1, occ1) (s2, occ2) -> CCInt.compare occ2 occ1)
      |> CCList.take k
      |> CCList.map fst
      |> ID.Set.of_list in
  
  Util.debugf ~section 3 "most common symbols are: @[%a@]@." 
    (fun k -> k (ID.Set.pp ID.pp) most_commmon_syms);

  (* now tbl contains occurrences of all symbols *)

  let triggers = create_trigger_map ~trim_implications ~tbl (axioms) in
  let syms_in_conj = symset_of_axs ~trim_implications ~is_goal:true goals in
  let conj_syms =
    ID.Set.diff syms_in_conj  most_commmon_syms in
  Util.debugf ~section 1 "conj_syms:@[%a@]" (fun k -> k (ID.Set.pp ID.pp) conj_syms);
  let triggered_1 = triggered_by_syms ~triggers conj_syms in

  ID.Tbl.iter (fun id set -> 
    Util.debugf ~section 1 "@[%a/%d@] > @[%a@]" (fun k -> k ID.pp id (ID.id id) (CCList.pp CCString.pp) (List.map name (InpStmSet.elements set)))
  ) triggers; 

  Util.debugf ~section 2 "layer 0" CCFun.id;
  Util.debugf ~section 2 "symbols: @[%a@]" (fun k -> k (ID.Set.pp ID.pp) conj_syms);
  Util.debugf ~section 2 "axs: @[%a@]" (fun k -> k (CCList.pp CCString.pp) (List.map name triggered_1));

  let rec take_axs k processed_syms k_triggered_axs = 
    if k >= depth_end then []
    else (
      let taken = if k >= depth_start then k_triggered_axs else [] in
      let new_syms = symset_of_axs ~trim_implications:false k_triggered_axs in
      let unprocessed = ID.Set.diff new_syms processed_syms in
      let k_p_1_triggered_ax = triggered_by_syms ~triggers unprocessed in
      Util.debugf ~section 2 "layer @[%d@]" (fun c -> c k );
      Util.debugf ~section 2 "symbols: @[%a@]" (fun k -> k (ID.Set.pp ID.pp) unprocessed);
      Util.debugf ~section 2 "axs: @[%a@]" (fun k -> k (CCList.pp CCString.pp) (List.map name k_p_1_triggered_ax));
      taken @ (take_axs (k+1) (ID.Set.union processed_syms unprocessed) k_p_1_triggered_ax)) 
  in

  let conj_defined_syms =
    if take_conj_defs then (
      ID.Set.fold (fun s_id conj_defs -> 
        InpStmSet.union conj_defs
          (ID.Map.get_or ~default:InpStmSet.empty s_id ids_to_defs)
      ) conj_syms (InpStmSet.empty))
    else InpStmSet.empty
  in

  let taken_axs = 
    CCList.sort_uniq ~cmp:compare
      ((InpStmSet.elements conj_defined_syms) @
      (take_axs 1 conj_syms triggered_1)) in

  Util.debugf ~section 1 "taken %d/%d axioms:@ @[%a@]@." 
    (fun k -> k (List.length taken_axs) (List.length axioms) (CCList.pp CCString.pp) (List.map name taken_axs));
  Util.debugf ~section 2 "take_conj_defs:%b@." (fun k -> k take_conj_defs);

  let res = helper_axioms @ taken_axs @ goals in
  Iter.of_list (res)

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
  let namespace = Proof.S.Tbl.create 8
  let pp ppf ppt ppty out st =
    let name = name st in
    let pp_decl out (id,ty) =
      if ID.is_distinct_object id
      then
        fpf out "%% (omitted type declaration for distinct object %a.)" ID.pp_tstp id
      else
        fpf out "thf(@[%s, type,@ %a :@ @[%a@]@])." name ID.pp_tstp id ppty ty
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
          fpf out "%a (@[%a%a@] =@ %a)" pp_quant_vars vars ID.pp_tstp id pp_args args ppt rhs
        | Def_form {vars;lhs;rhs;polarity=pol;_} ->
          let op = match pol with `Equiv-> "=" | `Imply -> "=>" in
          fpf out "%a(@[%a@] %s@ (@[<hv>%a@]))"
            pp_quant_vars vars (SLiteral.pp ppt) lhs op
            (Util.pp_list ~sep:" & " ppf) rhs
      in
      let pp_top_rule out r =
        fpf out "@[<2>thf(%s, axiom,@ (%a))@]." name pp_rule r
      in
      Util.pp_list ~sep:"" pp_top_rule out d.def_rules
    in
    match st.view with
    | TyDecl (id,ty) -> pp_decl out (id,ty)
    | Assert f ->
      let role = "axiom" in
      fpf out "@[<2>thf(%a, %s,@ (@[%a@]))@]." pp_name name role ppf f
    | Lemma l ->
      let role = "lemma" in
      fpf out "@[<2>thf(%a, %s,@ (@[%a@]))@]." pp_name name role
        (Util.pp_list ~sep:" & " ppf) l
    | Goal f ->
      let role = "conjecture" in
      fpf out "@[<2>thf(%a, %s,@ (@[%a@]))@]." pp_name name role ppf f
    | NegatedGoal (_,l) ->
      let role = "negated_conjecture" in
      let parents = 
        List.map (fun p -> `Name (Proof.S.name ~namespace @@ Proof.Parent.proof p))
             (Proof.Step.parents @@ st.proof)
      in
      List.iter
        (fun f ->
           fpf out "@[<2>thf(%a, %s,@ (@[%a@]),@ @[%a@])@]." pp_name name role
             ppf f Proof.Kind.pp_tstp (Proof.Step.kind @@ st.proof, parents))
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
          fpf out "@[<2>thf(%a, axiom,((@ %a %s %a) =@ (@[%a@])))@]."
            pp_name name ID.pp_tstp id 
            (if CCList.is_empty args then "" else "@")
            (Util.pp_list ~sep:"@ " ppt) args ppt rhs
        | Def_form {lhs;rhs;polarity=pol;_} ->
          let op = match pol with `Equiv-> "=" | `Imply -> "=>" in
          fpf out "@[<2>thf(%a, axiom,@ %a %s@ (@[%a@]))@]."
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
  pp_in (Util.pp_list ~sep:" | " (SLiteral.pp_in o pp_t)) pp_t pp_ty o

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
        Seq.forms st |> Iter.to_list |> TypedSTerm.Form.and_)
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
        |> Iter.map conv_c
        |> Iter.to_list
        |> F.and_)
    ()

let as_proof_i t = Proof.S.mk t.proof (Proof.Result.make res_tc_i t)
let as_proof_c t = Proof.S.mk t.proof (Proof.Result.make res_tc_c t)

(** {2 Scanning} *)

let define_rw_rule ~proof r =
  ignore(proof);
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
             Iter.of_list def_rules
             |> Iter.map level_of_rule
             |> Iter.max
             |> CCOpt.get_or ~default:0
           and def =
             conv_rules def_rules proof
           in
           def_id, lev, def)
    in
    let level =
      Iter.of_list ids_and_levels
      |> Iter.map (fun (_,l,_) -> l)
      |> Iter.max |> CCOpt.map_or ~default:0 succ
    in
    List.iter
      (fun (id,_,def) ->
         let _ = Rewrite.Defined_cst.declare ~level id def in
         ())
      ids_and_levels
  | Rewrite d  ->
    let proof = as_proof_c st in
    define_rw_rule ~proof (conv_rule ~proof d)
  | _ -> ()

let scan_tst_rewrite (st:(TypedSTerm.t,TypedSTerm.t,TypedSTerm.t) t): unit = match view st with
  | Rewrite d  ->
    let proof = as_proof_i st in
    define_rw_rule ~proof (conv_rule_i ~proof d)
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

(** TODO: Ask Simon how to hide this in the fun *)
let def_sym = ref IdMap.empty;;

let get_rw_rule ?weight_incr:(w_i=1000000) c  =
  let distinct_free_vars l =
    l |> List.map (fun t -> Term.as_var t |>
                            (fun v -> match v with
                               | Some x -> Some (HVar.id x)
                               | None -> None) )
    |> OptionSet.of_list
    |> (fun set -> not (OptionSet.mem None set) && OptionSet.cardinal set = List.length l) in

  let make_rw sym vars rhs =
    let ty_vars = List.filter (fun v -> Type.is_tType (Term.ty v)) vars in
    let vars = List.filter (fun v -> not (Type.is_tType (Term.ty v))) vars in
    let n_new = List.length vars in
    let var_db_map =
      CCList.foldi (fun acc i v -> Term.Map.add v (n_new-i-1) acc) Term.Map.empty vars in
    let vars_to_db = Term.DB.map_vars_shift var_db_map rhs in
    let abs_rhs =  (Term.fun_l ((CCList.map Term.ty vars)) vars_to_db) in
    assert(Term.DB.is_closed abs_rhs);
    let r = Rewrite.Term.Rule.make ~proof:(as_proof_c c) sym (Type.close_forall (Term.ty abs_rhs)) ty_vars abs_rhs in
    let rule = Rewrite.T_rule r in
    Util.debugf 5 "Defined %a with %a" (fun k -> k ID.pp sym Rewrite.Term.Rule.pp r);
    rule in

  let build_from_head sym vars rhs =
    let rhs = Lambda.eta_reduce @@ Lambda.snf (fst (Rewrite.Term.normalize_term rhs)) in
    let vars_lhs = Term.VarSet.of_iter (Iter.fold (fun acc v -> 
        Iter.append acc (Term.Seq.vars v)) 
        Iter.empty (Iter.of_list vars)) in
    if not (Term.symbols rhs |> ID.Set.mem sym) &&
       Term.VarSet.cardinal
         (Term.VarSet.diff (Term.vars rhs) vars_lhs) = 0 then
      (* Here I skipped proof object creation *)
      let res_rw =  Some (sym, make_rw sym vars rhs) in
      (def_sym := IdMap.add sym (rhs, res_rw) !def_sym;
       res_rw)
    else None in

  let conv_terms_rw t1 t2 =
    let reduced = Lambda.eta_reduce t1 in
    let t2' = Lambda.snf (fst (Rewrite.Term.normalize_term t2)) in
    let hd, l = Term.as_app reduced in
    if (Term.is_const hd && distinct_free_vars l && Type.is_fun (Term.ty hd)) then (
      let sym = (Term.as_const_exn hd) in
      (match IdMap.find_opt sym !def_sym with
       | Some (rhs, rw_rule) ->  (
           let rhs = Lambda.eta_reduce rhs in
           if  not (Unif.FO.are_variant rhs t2') then (
             None)
           else rw_rule )
       | _ -> build_from_head sym l t2)
    ) 
    else None in

  let all_lits =  Seq.lits c in
  if Iter.length all_lits = 1 then
    match Iter.head_exn all_lits with
    | SLiteral.Eq (t1,t2) when not (List.mem t1 [Term.true_; Term.false_]) &&
                               not (List.mem t2 [Term.true_; Term.false_]) ->
      assert(Type.equal (Term.ty t1) (Term.ty t2));
      let ty = Term.ty t1 in
      let fresh_vars = List.map (fun ty -> Term.var (HVar.fresh ~ty ())) (Type.expected_args ty) in
      let t1, t2 = Lambda.snf @@ Term.app t1 fresh_vars, Lambda.snf @@ Term.app t2 fresh_vars in
      if (Term.weight t2 - Term.weight t1 <= w_i) then (
        match conv_terms_rw t1 t2 with
        | Some rhs -> Some rhs
        | None -> if Term.weight t1 - Term.weight t2 <= w_i then
            conv_terms_rw t2 t1 else None)
      else (if Term.weight t1 - Term.weight t2 <= w_i then
              conv_terms_rw t2 t1 else None)
    | _ -> None
  else None
