
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for Algebraic types} *)

open Logtk
open Libzipperposition

module T = Term
module S = Subst
module Lit = Literal
module Lits = Literals
module Stmt = Statement

type term = T.t

let prof_detect = Util.mk_profiler "enum_types.detect"
let prof_instantiate = Util.mk_profiler "enum_types.instantiate_vars"

let stat_declare = Util.mk_stat "enum_types.declare"
let stat_simplify = Util.mk_stat "enum_types.simplify"
let stat_instantiate = Util.mk_stat "enum_types.instantiate_axiom"

let section = Util.Section.make ~parent:Const.section "enum_ty"

(* flag for clauses that are declarations of enumerated types *)
let flag_enumeration_clause = SClause.new_flag ()

exception Error of string

let () = Printexc.register_printer
    (function Error s -> Some ("error in enum_types: " ^s)
            | _ -> None)

let error_ s = raise (Error s)
let errorf_ msg = CCFormat.ksprintf msg ~f:error_

type id_or_ty_builtin =
  | I of ID.t
  | B of Type.builtin

let pp_id_or_builtin out = function
  | I id -> ID.pp out id
  | B b -> Type.pp_builtin out b

(** {2 Inference rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  type decl

  val pp_decl : decl CCFormat.printer

  type declare_result =
    | New of decl
    | AlreadyDeclared of decl

  val declare_ty :
    proof:Proof.t ->
    ty_id:ID.t ->
    ty_vars:Type.t HVar.t list ->
    var:Type.t HVar.t ->
    term list ->
    declare_result
  (** Declare that the domain of the type [ty_id] is restricted to
      given list of [cases], in the form [forall var. Or_{c in cases} var = c].
      The type of [var] must be [ty_id ty_vars].
      Will be ignored if the type already has a enum declaration, and the old
      declaration will be returned instead.
      @return either the new declaration, or the already existing one for
        this type if any
      @raise Error if some of the preconditions is not filled *)

  val instantiate_vars : Env.multi_simpl_rule
  (** Instantiate variables whose type is a known enumerated type,
      with all cases of this type. *)

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let _enable = ref false
let _instantiate_shielded = ref false
let _accept_unary_types = ref true
let _instantiate_projector_axiom = ref false

let is_projector_ id ~of_ = match Ind_ty.as_projector id with
  | Some p -> ID.equal (Ind_ty.projector_id p) of_
  | None -> false

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module PS = Env.ProofState
  module Ctx = Env.Ctx

  (* one particular enum type. The return type has the shape [id(vars)],
     such as [list(a)] or [map(a,b)] *)
  type decl = {
    decl_ty_id : id_or_ty_builtin;
    decl_ty_vars : Type.t HVar.t list;
    decl_ty : Type.t;  (* id applied to ty_vars (shortcut) *)
    decl_var : Type.t HVar.t; (* x = ... *)
    decl_cases : term list; (* ... t1 | t2 | ... | tn *)
    decl_proof :
      [ `Data of Proof.t * Type.t Statement.data
      | `Clause of Proof.t
      ]; (* justification for the enumeration axiom *)
    mutable decl_symbols : ID.Set.t; (* set of declared symbols for t1,...,tn *)
  }

  let pp_decl out d =
    Format.fprintf out "@[<1>{enum_ty=@[%a@],@ cases=@[%a@]}@]"
      Type.pp d.decl_ty (Util.pp_list T.pp) d.decl_cases

  (* set of enumerated types (indexed by [decl_ty_id]) *)
  let decls_by_id = ID.Tbl.create 16
  let decls_builtin = ref []

  let find_decl_ = function
    | I i -> ID.Tbl.find decls_by_id i
    | B b -> List.assoc b !decls_builtin

  let add_decl_ id decl = match id with
    | I id -> ID.Tbl.add decls_by_id id decl
    | B b -> decls_builtin := (b,decl) :: !decls_builtin

  (* triggered whenever a new EnumType is added *)
  let on_new_decl = Signal.create ()

  (* check that [var] is the only free (term) variable in all cases *)
  let check_uniq_var_is_ ~var cases =
    cases
    |> Sequence.of_list
    |> Sequence.flat_map T.Seq.vars
    |> Sequence.filter (fun v -> not (Type.equal Type.tType (HVar.ty v)))
    |> Sequence.for_all (HVar.equal Type.equal var)

  (* check that all vars in [l] are pairwise distinct *)
  let rec check_all_distinct_ acc l = match l with
    | [] -> true
    | v :: l' ->
      not (CCList.mem ~eq:(HVar.equal Type.equal) v acc)
      && check_all_distinct_ (v :: acc) l'

  type declare_result =
    | New of decl
    | AlreadyDeclared of decl

  let declare_ ~proof ~ty_id:id ~ty_vars ~var cases =
    if not (check_all_distinct_ [] ty_vars)
    then errorf_ "invalid declaration %a: duplicate type variable" pp_id_or_builtin id;
    if not (check_uniq_var_is_ ~var cases)
    then errorf_ "invalid declaration %a: %a is not the only variable in @[%a@]"
        pp_id_or_builtin id (Util.pp_list T.pp) cases HVar.pp var;
    try
      let decl = find_decl_ id in
      Util.debugf ~section 3 "@[an enum is already declared for type %a@]"
        (fun k->k pp_id_or_builtin id);
      AlreadyDeclared decl
    with Not_found ->
      let ty = match id with
        | I id -> Type.app id (List.map Type.var ty_vars)
        | B b -> assert (ty_vars=[]); Type.builtin b
      in
      Util.debugf ~section 1
        "@[<2>declare new enum type `@[%a@]`@ (@[cases %a ∈ {@[<hv>%a@]}@])@]"
        (fun k->k Type.pp ty HVar.pp var (Util.pp_list ~sep:", " T.pp) cases);
      Util.incr_stat stat_declare;
      (* set of already declared symbols *)
      let decl_symbols =
        List.fold_left
          (fun set t -> match T.head t with
             | None -> errorf_ "non-symbolic case @[%a@]" T.pp t
             | Some s -> ID.Set.add s set)
          ID.Set.empty cases
      in
      let decl = {
        decl_ty_id=id;
        decl_ty_vars=ty_vars;
        decl_ty=ty;
        decl_var=var;
        decl_cases=cases;
        decl_symbols;
        decl_proof=proof;
      } in
      add_decl_ id decl;
      Signal.send on_new_decl decl;
      New decl

  (* declare an enumerated type *)
  let declare_ty ~proof ~ty_id ~ty_vars ~var cases =
    declare_ ~proof:(`Clause proof) ~var ~ty_id:(I ty_id) ~ty_vars cases

  let as_simple_ty ty = match Type.view ty with
    | Type.App (id, []) -> Some (I id)
    | Type.Builtin b -> Some (B b)
    | _ -> None

  let is_simple_ty ty = as_simple_ty ty <> None

  (* detect whether the clause [c] is a declaration of a simply-typed EnumType
     with only constants as cases (in other words, a monomorphic finite type) *)
  let detect_decl_ c =
    let eq_var_ ~var t = match T.view t with
      | T.Var v' -> HVar.equal Type.equal var v'
      | _ -> false
    and get_var_ t = match T.view t with
      | T.Var v -> v
      | _ -> assert false
    and is_const t = match T.view t with
      | T.Const _ -> true
      | _ -> false
    in
    (* loop over literals checking whether they are all of the form
       [var = t] for some constant [t] *)
    let rec _check_all_vars ~ty ~var acc lits = match lits with
      | [] ->
        (* now also check that no case has free variables other than [var],
            and that there are at least 2 cases *)
        if check_uniq_var_is_ ~var acc
        && (!_accept_unary_types || List.length acc >= 2)
        then Some (var, acc)
        else None
      | Lit.Equation (l, r, true) :: lits' when eq_var_ ~var l && is_const r ->
        _check_all_vars ~ty ~var (r::acc) lits'
      | Lit.Equation (l, r, true) :: lits' when eq_var_ ~var r && is_const l ->
        _check_all_vars ~ty ~var (l::acc) lits'
      | _ -> None
    in
    let lits = C.lits c in
    if CCArray.exists (fun l -> not (Lit.is_eq l)) lits then None
    else match Array.to_list lits with
      | Lit.Equation (l,r,true) :: lits when T.is_var l && is_simple_ty (T.ty l) && is_const r ->
        let var = get_var_ l in
        _check_all_vars ~ty:(T.ty l) ~var [r] lits
      | Lit.Equation (l,r,true) :: lits when T.is_var r && is_simple_ty (T.ty r) && is_const l ->
        let var = get_var_ r in
        _check_all_vars ~ty:(T.ty r) ~var [l] lits
      | _ -> None

  let detect_declaration c = Util.with_prof prof_detect detect_decl_ c

  (* retrieve variables that are directly under a positive equation *)
  let vars_under_eq_ lits =
    Sequence.of_array lits
    |> Sequence.filter Lit.is_eq
    |> Sequence.flat_map Lit.Seq.terms
    |> Sequence.filter T.is_var

  (* variables occurring under some function symbol (at non-0 depth) *)
  let _shielded_vars lits =
    Sequence.of_array lits
    |> Sequence.flat_map Lit.Seq.terms
    |> Sequence.flat_map T.Seq.subterms_depth
    |> Sequence.filter_map
      (fun (v,depth) -> if depth>0 && T.is_var v then Some v else None)
    |> T.Seq.add_set T.Set.empty

  let naked_vars_ lits =
    let v =
      vars_under_eq_ lits
      |> T.Seq.add_set T.Set.empty
    in
    T.Set.diff v (_shielded_vars lits)
    |> T.Set.elements

  (* assuming [length decl.decl_ty_vars = length args], bind them pairwise
     in a substitution *)
  let bind_vars_ (d,sc_decl) (args,sc_args) =
    List.fold_left2
      (fun subst v arg ->
         let v = (v : Type.t HVar.t :> InnerTerm.t HVar.t) in
         Subst.Ty.bind subst (v,sc_decl) (arg,sc_args))
      Subst.empty d.decl_ty_vars args

  (* given a type [ty], find whether it's an enum type, and if it is the
     case return [Some (decl, subst)] *)
  let find_ty_ sc_decl ty sc_ty =
    let find_aux i l =
      try
        let d = find_decl_ i in
        if List.length l = List.length d.decl_ty_vars
        then
          let subst = bind_vars_ (d,sc_decl) (l,sc_ty) in
          Some (d, subst)
        else None
      with Not_found -> None
    in
    match Type.view ty with
      | Type.Builtin b -> find_aux (B b) []
      | Type.App (id, l) -> find_aux (I id) l
      | _ -> None

  (* TODO: maybe relax the restriction that is must not be naked, but only
     up to a given depth (if CLI arg?) *)

  (* TODO: only instantiate naked variables that are in positive equations;
     those in negative equations may come from purification and must be
     found be E-unification *)

  (* instantiate variables that belong to an enum case *)
  let instantiate_vars_ c =
    (* which variables are candidate? depends on a CLI flag *)
    let vars =
      if !_instantiate_shielded
      then vars_under_eq_ (C.lits c) |> Sequence.to_rev_list
      else naked_vars_ (C.lits c)
    in
    let s_c = 0 and s_decl = 1 in
    CCList.find_map
      (fun v ->
         match find_ty_ s_decl (T.ty v) s_c with
           | None -> None
           | Some (decl, subst) ->
             (* we found an enum type declaration for [v], replace it
                with each case for the enum type *)
             Util.incr_stat stat_simplify;
             let subst = Unif_subst.of_subst subst in
             let l =
               List.map
                 (fun case ->
                    (* replace [v] with [case] now *)
                    let subst = Unif.FO.unify_full ~subst (v,s_c) (case,s_decl) in
                    let renaming = Subst.Renaming.create () in
                    let c_guard = Literals.of_unif_subst renaming subst
                    and subst = Unif_subst.subst subst in
                    let lits' = Lits.apply_subst renaming subst (C.lits c,s_c) in
                    let proof =
                      Proof.Step.inference [Proof.Parent.from @@ C.proof c]
                        ~rule:(Proof.Rule.mk"enum_type_case_switch")
                    in
                    let trail = C.trail c and penalty = C.penalty c in
                    let c' =
                      C.create_a ~trail ~penalty
                        (CCArray.append c_guard lits') proof
                    in
                    Util.debugf ~section 3
                      "@[<2>deduce @[%a@]@ from @[%a@]@ @[(enum_type switch on %a)@]@]"
                      (fun k->k C.pp c' C.pp c Type.pp decl.decl_ty);
                    c')
                 decl.decl_cases
             in
             Some l)
      vars

  let instantiate_vars c = Util.with_prof prof_instantiate instantiate_vars_ c

  let instantiate_axiom_ ~ty_s s poly_args decl =
    if ID.Set.mem s decl.decl_symbols
    then None (* already declared *)
    else (
      let ty_args, _ = Type.open_fun ty_s in
      (* need to add an axiom instance for this symbol and declaration *)
      decl.decl_symbols <- ID.Set.add s decl.decl_symbols;
      (* create the axiom.
         - build [subst = decl.x->s(u1,...,u_m)]
           where the the [u_i] are variables of the types required by [ty_s]
         - evaluate [decl.x = decl.t1 | decl.t2 .... | decl.t_m] in subst
      *)
      let vars = List.mapi (fun i ty -> HVar.make ~ty i |> T.var) ty_args in
      let t = T.app (T.const ~ty:ty_s s) vars in
      Util.debugf ~section 5 "@[<2>instantiate enum type `%a`@ on `@[%a@]`@]"
        (fun k->k pp_id_or_builtin decl.decl_ty_id T.pp t);
      let us = bind_vars_ (decl,0) (poly_args,1) |> Unif_subst.of_subst in
      let us = Unif.FO.unify_full ~subst:us (T.var decl.decl_var,0) (t,1) in
      let renaming = Subst.Renaming.create () in
      let subst = Unif_subst.subst us
      and c_guard = Literal.of_unif_subst renaming us in
      let lits =
        List.map
          (fun case ->
             Lit.mk_eq
               (S.FO.apply renaming subst (t,1))
               (S.FO.apply renaming subst (case,0)))
          decl.decl_cases
      in
      let proof =
        let parent = match decl.decl_proof with
          | `Data (src,_) -> src
          | `Clause src -> src
        in
        Proof.Step.inference
          ~rule:(Proof.Rule.mk "axiom_enum_types")
          [Proof.Parent.from parent]
      in
      let trail = Trail.empty in
      (* start with initial penalty *)
      let penalty = 4 in
      let c' = C.create ~trail ~penalty (c_guard@lits) proof in
      Util.debugf ~section 3 "@[<2>instantiate axiom of enum type `%a` \
                              on @[%a@]:@ clause @[%a@]@]"
        (fun k->k pp_id_or_builtin decl.decl_ty_id ID.pp s C.pp c');
      Util.incr_stat stat_instantiate;
      Some c'
    )

  (* assume [s : ty_s] where [ty_s = _ -> ... -> decl.decl_ty_id poly_args]
     This builds the axiom
     [forall x1...xn.
        let t = s x1...xn in
        exists y11...y1m. t = c1 y11...y1mn
        or exists ..... t = c2 ....
        or ....]
      where [c1, ..., ck] are the constructors of [decl].
      It uses the projectors instead of just skolemizing the "exists" *)
  let instantiate_axiom ~ty_s s poly_args decl =
    if !_instantiate_projector_axiom
    then instantiate_axiom_ ~ty_s s poly_args decl
    else None

  (* [check_decl_ id ~ty decl] checks whether [ty] is compatible
     with [decl.decl_ty]. If it is the case, let [c a1...an = ty], we
     add the axiom
     [forall x1:a1...xn:an, id(x1...xn) = t_1[x := id(x1..xn)] or ... or t_m[...]]
     where the [t_i] are the cases of [decl] *)
  let check_decl_ ~ty s decl =
    let _, ty_ret = Type.open_fun ty in
    match Type.view ty_ret, decl.decl_ty_id with
      | Type.Builtin b, B b' when b=b' ->
        instantiate_axiom ~ty_s:ty s [] decl
      | Type.App (c, args), I i
        when ID.equal c i
          && not (is_projector_ s ~of_:i)
          && List.length args = List.length decl.decl_ty_vars->
        instantiate_axiom ~ty_s:ty s args decl
      | _ -> None

  (* add axioms for new symbol [s] with type [ty], if needed *)
  let _on_new_symbol s ~ty =
    let aux i =
      try
        let decl = find_decl_ i in
        check_decl_ ~ty s decl |> CCOpt.to_list
      with Not_found -> []
    in
    let clauses =
      let _, ty_ret = Type.open_fun ty in
      match Type.view ty_ret with
        | Type.Builtin b -> aux (B b)
        | Type.App (id, _) -> aux (I id)
        | _ -> []
    in
    (* set of support *)
    PS.ActiveSet.add (Sequence.of_list clauses)

  let _on_new_decl decl =
    let clauses =
      Signature.fold (Ctx.signature ()) []
        (fun acc s ty ->
           match check_decl_ s ~ty decl with
             | None -> acc
             | Some c -> c::acc)
    in
    PS.PassiveSet.add (Sequence.of_list clauses)

  let is_trivial c =
    C.get_flag flag_enumeration_clause c

  (* detect whether the clause is a declaration of enum type, and if it
      is, declare the type! *)
  let _detect_and_declare c =
    Util.debugf ~section 5 "@[<2>examine clause@ `@[%a@]`@]" (fun k->k C.pp c);
    match detect_declaration c with
      | None -> ()
      | Some (var,cases) ->
        let ty_id = CCOpt.get_exn (as_simple_ty (HVar.ty var)) in
        let is_new = declare_ ~ty_id ~ty_vars:[] ~var ~proof:(`Clause (C.proof c)) cases in
        (* clause becomes redundant if it's a new declaration *)
        match is_new with
          | New _ -> C.set_flag flag_enumeration_clause c true
          | AlreadyDeclared _ -> ()

  (* introduce projectors for each constructor's argument, in order to
     declare the inductive type as an EnumType. *)
  let _declare_inductive ~proof d =
    Util.debugf ~section 5 "@[<2>examine data `%a`@]" (fun k->k ID.pp d.Stmt.data_id);
    (* make HVars *)
    let ty_vars = List.mapi (fun i _ -> HVar.make ~ty:Type.tType i) d.Stmt.data_args in
    let ty_vars_t = List.map T.var ty_vars in
    let ty_of_var =
      Type.app d.Stmt.data_id (List.map Type.var ty_vars) in
    let v = HVar.make ~ty:ty_of_var (List.length d.Stmt.data_args+1) in
    let v_t = T.var v in
    let cases =
      List.map
        (fun (c_id, c_ty, c_args) ->
           (* declare projector functions for this constructor *)
           let num_ty_vars, _, _ty_ret = Type.open_poly_fun c_ty in
           assert (num_ty_vars = List.length ty_vars);
           let projs_of_v =
             List.map
               (fun (_ty_arg,(proj, ty_proj)) ->
                  T.app (T.const ~ty:ty_proj proj) (ty_vars_t @ [v_t]))
               c_args
           in
           (* [c (proj1 v) (proj2 v) ... (proj_n v)] *)
           T.app (T.const ~ty:c_ty c_id) (ty_vars_t @ projs_of_v)
        )
        d.Stmt.data_cstors
    in
    let _ =
      declare_ ~proof:(`Data (proof,d))
        ~var:v ~ty_id:(I d.Stmt.data_id) ~ty_vars cases
    in
    ()

  (* detect whether the input statement contains some EnumType declaration *)
  let _detect_stmt stmt =
    match Stmt.view stmt with
      | Stmt.Assert c ->
        let proof = Stmt.proof_step stmt in
        let c = C.of_forms ~trail:Trail.empty c proof in
        _detect_and_declare c
      | Stmt.Data l ->
        let proof = Stmt.as_proof_c stmt in
        List.iter (_declare_inductive ~proof) l
      | Stmt.TyDecl _
      | Stmt.Def _
      | Stmt.Rewrite _
      | Stmt.Lemma _
      | Stmt.NegatedGoal _
      | Stmt.Goal _ -> ()

  let setup () =
    if !_enable then (
      Util.debug ~section  1 "register handling of enumerated types";
      Env.add_multi_simpl_rule instantiate_vars;
      Env.add_is_trivial is_trivial;
      (* look in input statements  for inductive types *)
      Signal.on_every Env.on_input_statement _detect_stmt;
      (* signals: instantiate axioms upon new symbols, or when new
          declarations are added *)
      Signal.on_every Ctx.on_new_symbol
        (fun (s, ty) -> _on_new_symbol s ~ty);
      Signal.on_every on_new_decl
        (fun decl ->
           _on_new_decl decl;
           (* need to simplify (instantiate) active clauses that have naked
              variables of the given type *)
           Env.simplify_active_with instantiate_vars);
      Signature.iter (Ctx.signature ()) (fun s ty -> _on_new_symbol s ~ty);
    )
end

(** {2 As Extension} *)

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with Extensions.
                         name = "enum_types";
                         env_actions=[register];
  }

let () =
  Extensions.register extension;
  Params.add_opts
    [ "--enum-types"
    , Options.switch_set true _enable
    , " enable inferences for enumerated/inductive types"
    ; "--no-enum-types"
    , Options.switch_set false _enable
    , " disable inferences for enumerated/inductive types"
    ; "--projector-axioms"
    , Options.switch_set true _instantiate_projector_axiom
    , " enable exhaustiveness axioms for inductive types (with projectors)"
    ; "--no-projector-axioms"
    , Options.switch_set false _instantiate_projector_axiom
    , " disable exhaustiveness axioms for inductive types (with projectors)"
    ; "--enum-shielded"
    , Options.switch_set true _instantiate_shielded
    , " enable/disable instantiation of shielded variables of enum type"
    ; "--no-enum-shielded"
    , Options.switch_set false _instantiate_shielded
    , " enable/disable instantiation of shielded variables of enum type"
    ; "--enum-unary"
    , Options.switch_set true _accept_unary_types
    , " enable support for unary enum types (one case)"
    ; "--no-enum-unary"
    , Options.switch_set false _accept_unary_types
    , " disable support for unary enum types (one case)"
    ]
