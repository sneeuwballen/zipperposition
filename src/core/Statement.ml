
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

type 'ty skolem = ID.t * 'ty

type ('t, 'ty) term_rule = 'ty Var.t list * ID.t * 'ty * 't list * 't
(** [forall vars, id args = rhs] *)

type ('f, 't, 'ty) form_rule = 'ty Var.t list * 't SLiteral.t * 'f list
(** [forall vars, lhs <=> bigand rhs] *)

type ('f, 't, 'ty) def_rule =
  | Def_term of ('t, 'ty) term_rule
  | Def_form of ('f, 't, 'ty) form_rule

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
  | RewriteTerm of ('t, 'ty) term_rule
  | RewriteForm of ('f, 't, 'ty) form_rule
  | Assert of 'f (** assert form *)
  | Lemma of 'f list (** lemma to prove and use, using Avatar cut *)
  | Goal of 'f (** goal to prove *)
  | NegatedGoal of 'ty skolem list * 'f list (** goal after negation, with skolems *)

(* a statement in a file *)
type from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

type lit = FOTerm.t SLiteral.t
type clause = lit list

type ('f, 't, 'ty) t = {
  view: ('f, 't, 'ty) view;
  attrs: attrs;
  src: source;
}

and role =
  | R_assert
  | R_goal
  | R_def
  | R_decl

and source = {
  src_id: int;
  src_view: source_view;
}
and source_view =
  | Input of UntypedAST.attrs * role
  | From_file of from_file * role
  | Internal of role
  | Neg of sourced_t
  | CNF of sourced_t

and result =
  | Sourced_input of TypedSTerm.t
  | Sourced_clause of clause

and sourced_t = result * source

type input_t = (TypedSTerm.t, TypedSTerm.t, TypedSTerm.t) t
type clause_t = (clause, FOTerm.t, Type.t) t

let view t = t.view
let attrs t = t.attrs
let src t = t.src

let mk_data id ~args ty cstors =
  {data_id=id; data_args=args; data_ty=ty; data_cstors=cstors; }

let mk_def ?(rewrite=false) id ty rules =
  { def_id=id; def_ty=ty; def_rules=rules; def_rewrite=rewrite; }

let mk_ ?(attrs=[]) ~src view = {src; view; attrs; }

let ty_decl ?attrs ~src id ty = mk_ ?attrs ~src (TyDecl (id,ty))
let def ?attrs ~src l = mk_ ?attrs ~src (Def l)
let rewrite_term ?attrs ~src r = mk_ ?attrs ~src (RewriteTerm r)
let rewrite_form ?attrs ~src r = mk_ ?attrs ~src (RewriteForm r)
let data ?attrs ~src l = mk_ ?attrs ~src (Data l)
let assert_ ?attrs ~src c = mk_ ?attrs ~src (Assert c)
let lemma ?attrs ~src l = mk_ ?attrs ~src (Lemma l)
let goal ?attrs ~src c = mk_ ?attrs ~src (Goal c)
let neg_goal ?attrs ~src ~skolems l = mk_ ?attrs ~src (NegatedGoal (skolems, l))

let map_data ~ty:fty d =
  { d with
      data_args = List.map (Var.update_ty ~f:fty) d.data_args;
      data_ty = fty d.data_ty;
      data_cstors = List.map (fun (id,ty) -> id, fty ty) d.data_cstors;
  }

let map_def ~form:fform ~term:fterm ~ty:fty d =
  { d with
      def_ty=fty d.def_ty;
      def_rules=
        List.map
          (function
            | Def_term (vars,id,ty,args,rhs) ->
              let vars = List.map (Var.update_ty ~f:fty) vars in
              Def_term (vars, id, fty ty, List.map fterm args, fterm rhs)
            | Def_form (vars,lhs,rhs) ->
              let vars = List.map (Var.update_ty ~f:fty) vars in
              Def_form (vars, SLiteral.map ~f:fterm lhs, List.map fform rhs))
          d.def_rules;
  }

let map ~form ~term ~ty st =
  let map_view ~form ~term ~ty:fty = function
    | Def l ->
      let l = List.map (map_def ~form ~term ~ty) l in
      Def l
    | RewriteTerm (vars, id, ty, args, rhs) ->
      let vars = List.map (Var.update_ty ~f:fty) vars in
      RewriteTerm (vars, id, fty ty, List.map term args, term rhs)
    | RewriteForm (vars,lhs,rhs) ->
      let vars = List.map (Var.update_ty ~f:fty) vars in
      RewriteForm (vars, SLiteral.map ~f:term lhs, List.map form rhs)
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

(** {2 Statement Source} *)

module Src = struct
  type t = source

  let file x = x.file
  let name x = x.name
  let loc x = x.loc

  let equal a b = a.src_id = b.src_id
  let hash a = a.src_id
  let view a = a.src_view

  let mk_ =
    let n = ref 0 in
    fun src_view -> {src_view; src_id=CCRef.get_then_incr n}

  let from_input attrs r : t = mk_ (Input (attrs, r))
  let from_file ?loc ?name file r : t = mk_ (From_file ({ name; loc; file; }, r))
  let internal r : t = mk_ (Internal r)
  let neg x : t = mk_ (Neg x)
  let cnf x : t = mk_ (CNF x)

  let neg_input f src = neg (Sourced_input f, src)
  let neg_clause c src = neg (Sourced_clause c, src)

  let cnf_input f src = cnf (Sourced_input f, src)
  let cnf_clause c src = cnf (Sourced_clause c, src)

  let pp_from_file out x =
    let pp_name out = function
      | None -> ()
      | Some n -> Format.fprintf out "at %s " n
    in
    Format.fprintf out "@[<2>%ain@ `%s`@,%a@]"
      pp_name x.name x.file ParseLocation.pp_opt x.loc

  let pp_role out = function
    | R_decl -> CCFormat.string out "decl"
    | R_assert -> CCFormat.string out "assert"
    | R_goal -> CCFormat.string out "goal"
    | R_def -> CCFormat.string out "def"

  (*
  let rec pp out t =
    match t with
      | From_file x -> pp_from_file out x
      | Neg t -> Format.fprintf out "@[<2>neg(%a)@]" pp t
      | CNF t -> Format.fprintf out "@[<2>cnf(%a)@]" pp t

  let to_string = CCFormat.to_string pp
     *)
end

(** {2 Defined Constants} *)

type form_rewrite = lit * clause list
(** Basic rewrite rule for literals *)

type form_definition = form_rewrite list
(** Definition for a literal *)

type definition =
  | D_term of Rewrite_term.defined_cst
  | D_form of form_rewrite list

exception Payload_defined_form of int * form_definition ref

let as_defined_cst id = match ID.payload id with
  | Payload_defined_form (l,d) -> Some (l, D_form !d)
  | Rewrite_term.Payload_defined_cst c ->
    Some (Rewrite_term.Defined_cst.level c, D_term c)
  | _ -> None

let as_defined_cst_level id = CCOpt.map fst @@ as_defined_cst id

let is_defined_cst id = as_defined_cst id <> None

let declare_defined_form id ~level def =
  (* declare that [id] is a defined constant of level [l+1] *)
  Util.debugf ~section:Util.Section.zip 1
    "@[<2>declare %a@ as defined constant of level %d@]"
    (fun k->k ID.pp id level);
  ID.set_payload id (Payload_defined_form (level, ref def));
  ()

let declare_defined_form_or_add id (r:form_rewrite): unit =
  begin match ID.payload id with
    | Payload_defined_form (_,l) -> l := r :: !l
    | _ -> declare_defined_form ~level:0 id [r]
  end

let declare_defined_cst_term id ~level rules: unit =
  let _ = Rewrite_term.declare_defined_cst ~level id rules in
  ()

let conv_term_rule (r:_ term_rule): Rewrite_term.rule =
  let _, id, ty, args, rhs = r in
  Rewrite_term.Rule.make id ty args rhs

let as_term_rules (l:_ def_rule list): Rewrite_term.rule list option =
  assert (l <> []);
  if List.for_all (function Def_term _ -> true | _ -> false) l
  then (
    l
    |> List.map
      (function
        | Def_term r -> conv_term_rule r
        | Def_form _ -> assert false)
    |> CCOpt.return
  ) else None

let as_clause_rules (l:_ def_rule list): form_definition option =
  assert (l <> []);
  if List.for_all (function Def_form _ -> true | _ -> false) l
  then (
    l
    |> List.map
      (function
        | Def_form (_,lit,rhs) -> lit, rhs
        | Def_term _ -> assert false)
    |> CCOpt.return
  ) else None

let terms_of_rule (d:_ def_rule): _ Sequence.t = match d with
  | Def_term (_, _, _, args, rhs) ->
    Sequence.of_list (rhs::args)
  | Def_form (_, lhs, rhs) ->
    Sequence.cons lhs (Sequence.of_list rhs |> Sequence.flat_map Sequence.of_list)
    |> Sequence.flat_map SLiteral.to_seq

let level_of_rule (d:_ def_rule): int =
  terms_of_rule d
  |> Sequence.flat_map FOTerm.Seq.symbols
  |> Sequence.filter_map as_defined_cst_level
  |> Sequence.max
  |> CCOpt.get_or ~default:0

let max_exn seq =
  seq
  |> Sequence.max
  |> CCOpt.get_lazy (fun () -> assert false)

let scan_stmt_for_defined_cst (st:(clause,FOTerm.t,Type.t) t): unit = match view st with
  | Def [] -> assert false
  | Def l ->
    (* define all IDs at the same level (the max of those computed) *)
    let ids_and_levels =
      List.map
        (fun {def_id; def_rules; _} ->
           let lev =
             Sequence.of_list def_rules
             |> Sequence.map level_of_rule
             |> max_exn
           and def =
             let open CCOpt.Infix in
             ((as_term_rules def_rules >|= fun l -> `Term l)
              <+>
                (as_clause_rules def_rules >|= fun l -> `Form l))
             |> CCOpt.get_lazy (fun _ -> assert false)
           in
           def_id, lev, def)
        l
    in
    let level =
      Sequence.of_list ids_and_levels
      |> Sequence.map (fun (_,l,_) -> l)
      |> max_exn
      |> succ
    in
    List.iter
      (fun (id,_,def) -> match def with
         | `Term l -> declare_defined_cst_term ~level id l
         | `Form def -> declare_defined_form ~level id def)
      ids_and_levels
  | RewriteTerm rule ->
    (* declare the rule, possibly making its head defined *)
    let r = conv_term_rule rule in
    let id = Rewrite_term.Rule.head_id r in
    Rewrite_term.declare_cst_or_add id r
  | RewriteForm (_,lhs,rhs) ->
    let r = lhs, rhs in
    (* is this rule associated to an ID? *)
    let lhs, _ = r in
    let head_id = match lhs with
      | SLiteral.Atom (t,_) -> FOTerm.head t
      | _ -> None
    in
    CCOpt.iter
      (fun id -> declare_defined_form_or_add id r)
      head_id
  | _ -> ()

(** {2 Iterators} *)

module Seq = struct
  let mk_term t = `Term t
  let mk_form f = `Form f

  let seq_of_rule (d:_ def_rule): _ Sequence.t = fun k -> match d with
    | Def_term (vars, _, ty, args, rhs) ->
      k (`Ty ty);
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      List.iter (fun t->k (`Term t)) (rhs::args);
    | Def_form (vars, lhs, rhs) ->
      List.iter (fun v->k (`Ty (Var.ty v))) vars;
      SLiteral.to_seq lhs |> Sequence.map mk_term |> Sequence.iter k;
      Sequence.of_list rhs |> Sequence.map mk_form |> Sequence.iter k

  let to_seq st k =
    let decl id ty = k (`ID id); k (`Ty ty) in
    match view st with
      | TyDecl (id,ty) -> decl id ty;
      | Def l ->
        List.iter
          (fun {def_id; def_ty; def_rules; _} ->
             decl def_id def_ty;
             Sequence.of_list def_rules
             |> Sequence.flat_map seq_of_rule |> Sequence.iter k)
          l
      | RewriteTerm (_,_,ty,args,rhs) ->
        k (`Ty ty);
        List.iter (fun t -> k (`Term t)) args;
        k (`Term rhs)
      | RewriteForm (_,lhs,rhs) ->
        SLiteral.iter ~f:(fun t -> k (`Term t)) lhs;
        List.iter (fun f -> k (`Form f)) rhs
      | Data l ->
        List.iter
          (fun d ->
             decl d.data_id d.data_ty;
             List.iter (CCFun.uncurry decl) d.data_cstors)
          l
      | Lemma l -> List.iter (fun f -> k (`Form f)) l
      | Assert f
      | Goal f -> k (`Form f)
      | NegatedGoal (_,l) -> List.iter (fun f -> k (`Form f)) l

  let ty_decls st k = match view st with
    | Def l ->
      List.iter
        (fun {def_id; def_ty; _} -> k (def_id, def_ty))
        l
    | TyDecl (id, ty) -> k (id,ty)
    | Data l ->
      List.iter
        (fun d ->
           k (d.data_id, d.data_ty);
           List.iter k d.data_cstors)
        l
    | RewriteTerm _
    | RewriteForm _
    | Lemma _
    | Goal _
    | NegatedGoal _
    | Assert _ -> ()

  let forms st k = match view st with
    | Def _
    | RewriteTerm _
    | Data _
    | TyDecl _ -> ()
    | Goal c -> k c
    | RewriteForm (_,_, _) -> ()
    | Lemma l
    | NegatedGoal (_, l) -> List.iter k l
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

let add_src ~file st = match st.src.src_view with
  | Input (attrs,r) ->
    let module A = UntypedAST in
    let attrs =
      CCList.filter_map
        (function A.A_AC -> Some A_AC | A.A_name _ -> None)
        attrs
    and name =
      CCList.find_map
        (function A.A_name n-> Some n | _ -> None)
        attrs
    in
    { st with
        src=Src.from_file ?name file r;
        attrs;
    }
  | _ -> st

(** {2 IO} *)

let fpf = Format.fprintf

let pp_attr out = function
  | A_AC -> fpf out "AC"

let pp_attrs out = function
  | [] -> ()
  | l -> fpf out "@ [@[%a@]]" (Util.pp_list ~sep:"," pp_attr) l

let pp_typedvar ppty out v = fpf out "(%a:%a)" Var.pp v ppty (Var.ty v)
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
    | Def_term (vars,id,_,args,rhs) ->
      fpf out "@[<2>%a@[<2>%a%a@] =@ %a@]"
        pp_vars vars ID.pp id pp_args args ppt rhs
    | Def_form (vars,lhs,rhs) ->
      fpf out "@[<2>%a%a =@ (@[<hv>%a@])@]"
        pp_vars vars
        (SLiteral.pp ppt) lhs
        (Util.pp_list ~sep:" && " ppf) rhs
  end

let pp_def ppf ppt ppty out d =
  fpf out "@[<2>@[%a : %a@]@ where@ @[<hv>%a@]@]"
    ID.pp d.def_id ppty d.def_ty
    (Util.pp_list ~sep:";" (pp_def_rule ppf ppt ppty)) d.def_rules

let pp ppf ppt ppty out st = match st.view with
  | TyDecl (id,ty) ->
    fpf out "@[<2>val%a %a :@ @[%a@]@]." pp_attrs st.attrs ID.pp id ppty ty
  | Def l ->
    fpf out "@[<2>def%a %a@]."
      pp_attrs st.attrs (Util.pp_list ~sep:" and " (pp_def ppf ppt ppty)) l
  | RewriteTerm (_, id, _, args, rhs) ->
    fpf out "@[<2>rewrite%a @[%a %a@]@ = @[%a@]@]" pp_attrs st.attrs
      ID.pp id (Util.pp_list ~sep:" " ppt) args ppt rhs
  | RewriteForm (_, lhs, rhs) ->
    fpf out "@[<2>rewrite%a @[%a@]@ <=> @[%a@]@]" pp_attrs st.attrs
      (SLiteral.pp ppt) lhs (Util.pp_list ~sep:" && " ppf) rhs
  | Data l ->
    let pp_cstor out (id,ty) =
      fpf out "@[<2>| %a :@ @[%a@]@]" ID.pp id ppty ty in
    let pp_data out d =
      fpf out "@[<hv2>@[%a : %a@] :=@ %a@]"
        ID.pp d.data_id ppty d.data_ty (Util.pp_list ~sep:"" pp_cstor) d.data_cstors
    in
    fpf out "@[<hv2>data%a@ %a@]" pp_attrs st.attrs (Util.pp_list ~sep:" and " pp_data) l
  | Assert f ->
    fpf out "@[<2>assert%a@ @[%a@]@]." pp_attrs st.attrs ppf f
  | Lemma l ->
    fpf out "@[<2>lemma%a@ @[%a@]@]."
      pp_attrs st.attrs (Util.pp_list ~sep:" && " ppf) l
  | Goal f ->
    fpf out "@[<2>goal%a@ @[%a@]@]." pp_attrs st.attrs ppf f
  | NegatedGoal (sk, l) ->
    let pp_sk out (id,ty) = fpf out "(%a:%a)" ID.pp id ppty ty in
    fpf out "@[<hv2>negated_goal%a@ @[<hv>%a@]@ # skolems: [@[<hv>%a@]]@]."
      pp_attrs st.attrs
      (Util.pp_list ~sep:", " (CCFormat.hovbox ppf)) l
      (Util.pp_list pp_sk) sk

let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)

let pp_clause =
  pp (Util.pp_list ~sep:" ∨ " (SLiteral.pp FOTerm.pp)) FOTerm.pp Type.pp

let pp_input = pp TypedSTerm.pp TypedSTerm.pp TypedSTerm.pp

module TPTP = struct
  let pp ppf ppt ppty out st =
    let name = match st.src.src_view with
      | From_file (f,_) -> CCOpt.get_or ~default:"no_name" (Src.name f)
      | _ -> "no_name"
    in
    let pp_decl out (id,ty) =
      fpf out "@[<2>tff(%s, type,@ %a :@ @[%a@])@]." name ID.pp id ppty ty
    and pp_quant_vars out = function
      | [] -> ()
      | l ->
        let pp_typedvar out v =
          fpf out "%a:%a" Var.pp v ppty (Var.ty v)
        in
        fpf out "@[<2>![@[%a@]]:@ " (Util.pp_list pp_typedvar) l
    in
    (* print a single definition as an axiom *)
    let pp_def_axiom out d =
      let pp_args out = function
        | [] -> ()
        | l -> fpf out "(@[%a@])" (Util.pp_list ~sep:"," ppt) l
      in
      let pp_rule out = function
        | Def_term (vars,id,_,args,rhs) ->
          fpf out "%a(@[%a%a@] =@ %a)" pp_quant_vars vars ID.pp id pp_args args ppt rhs
        | Def_form (vars,lhs,rhs) ->
          fpf out "%a(@[%a@] <=>@ (@[<hv>%a@]))" pp_quant_vars vars (SLiteral.pp ppt) lhs
            (Util.pp_list ~sep:" & " ppf) rhs
      in
      let pp_top_rule out r =
        fpf out "@[<2>tff(%s, axiom,@ %a)@].@," name pp_rule r
      in
      fpf out "@[<hv>%a@]" (Util.pp_list ~sep:"" pp_top_rule) d.def_rules
    in
    match st.view with
      | TyDecl (id,ty) -> pp_decl out (id,ty)
      | Assert f ->
        let role = "axiom" in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f
      | Lemma l ->
        let role = "lemma" in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role
          (Util.pp_list ~sep:" & " ppf) l
      | Goal f ->
        let role = "conjecture" in
        fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f
      | NegatedGoal (_,l) ->
        let role = "negated_conjecture" in
        List.iter
          (fun f ->
             fpf out "@[<2>tff(%s, %s,@ (@[%a@])@]." name role ppf f)
          l
      | Def l ->
        (* declare *)
        List.iter
          (fun {def_id; def_ty; _} -> pp_decl out (def_id,def_ty))
          l;
        (* define *)
        List.iter (pp_def_axiom out) l
      | RewriteTerm (_, id, _, args, rhs) ->
        fpf out "@[<2>tff(%s, axiom,@ %a(%a) =@ @[%a@])@]."
          name ID.pp id (Util.pp_list ~sep:", " ppt) args ppt rhs
      | RewriteForm (_, lhs, rhs) ->
        fpf out "@[<2>tff(%s, axiom,@ %a <=>@ (@[%a@]))@]."
          name (SLiteral.TPTP.pp ppt) lhs (Util.pp_list ~sep:" & " ppf) rhs
      | Data _ -> failwith "cannot print `data` to TPTP"

  let to_string ppf ppt ppty = CCFormat.to_string (pp ppf ppt ppty)
end

