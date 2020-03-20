open Logtk
open Libzipperposition

module L = Literal
module Ls = Literals
module T = Term

let enabled = ref false

let k_lazy_cnf_kind = Flex_state.create_key ()
let k_renaming_threshold = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C

  module Idx = Fingerprint.Make(struct 
    type t = T.t * ((C.t * bool) list ref)
    let (<?>) = (CCOrd.Infix.(<?>))
    let compare (a1,c1) (a2,c2) = (T.compare a1 a2)
   end)

  let sign_present sign = function
    | [(c,sign1)] -> sign1 = sign
    | [(c1, sign1);(c2, sign2)] ->
      if sign1 = sign2 then invalid_arg "signs must be different!";
      true
    | _ -> invalid_arg "only one or two element lists"


  let _form_counter = Term.Tbl.create 256 
  let _skolem_idx = ref @@ Idx.empty ()
  let _renaming_idx = ref @@ Idx.empty ()

  let update_counter ~action c =
    Ls.fold_eqn 
      ~both:false ~ord:(E.Ctx.ord ()) ~eligible:(C.Eligible.always) 
      (C.lits c)
    |> Iter.iter (fun (lhs,rhs,_,_) -> 
      let terms = 
        if T.equal T.true_ rhs && T.is_appbuiltin lhs  then [lhs]
        else if Type.is_prop (T.ty lhs) && not (Term.is_var lhs) then
          [T.Form.equiv lhs rhs]
        else [] in
      List.iter (fun t -> 
        match action with
          | `Increase ->
            Term.Tbl.incr _form_counter t
          | `Decrease -> 
            assert(Term.Tbl.mem _form_counter t);
            Term.Tbl.decr _form_counter t
      ) terms)

  let fold_lits c = 
    Ls.fold_eqn 
      ~both:false ~ord:(E.Ctx.ord ()) ~eligible:(C.Eligible.res c) 
      (C.lits c)

  let proof ~name ~parents c =
    Proof.Step.inference 
      ~rule:(Proof.Rule.mk name) (List.map C.proof_parent parents)

  let mk_renaming_clause parent ~renamer ~form sign =
    let proof = Proof.Step.define_internal 
      (T.head_exn renamer) [C.proof_parent parent] in

    let res = 
      if sign then (
        C.create ~penalty:0 ~trail:Trail.empty 
          [L.mk_false renamer; L.mk_true form] proof
      ) else (C.create ~penalty:0 ~trail:Trail.empty 
          [L.mk_true renamer; L.mk_false form] proof
      ) in
    C.set_flag SClause.flag_is_lazy_def res true;
    res

  let rename ~c form sign =
    assert(Type.is_prop (T.ty form));
    if C.get_flag SClause.flag_is_lazy_def c then None
    else (
      let gen = Iter.head @@ 
        Idx.retrieve_generalizations (!_renaming_idx, 0) (form, 1) in
      match gen with 
      | Some (orig, (renamer, defined_as), subst) ->
        let renamer_sub = 
          Subst.FO.apply Subst.Renaming.none subst (renamer,0) in

        let renamer_sub, new_defs, parents = 
          if sign_present sign !defined_as then (
            (renamer_sub, [], !defined_as)
          ) else (
            let def = mk_renaming_clause c ~renamer ~form:orig sign in
            defined_as := (def,sign) :: !defined_as;
            (renamer_sub, [def], !defined_as)) in
        Some(renamer_sub, new_defs,
             CCList.filter_map (fun (c, sign') ->
              if sign != sign' then None else Some c) parents)
      | None ->
        (* maybe we need to define it if it appears too many times *)
        let num_occurences = Term.Tbl.get_or _form_counter form ~default:0 in
        if num_occurences >= Env.flex_get k_renaming_threshold then (
          Term.Tbl.remove _form_counter form;

          let free_vars = T.vars form |> T.VarSet.to_list in
          let (id, ty), renamer =
            T.mk_fresh_skolem ~prefix:"form" free_vars Type.prop in
          E.Ctx.declare id ty;
          let def = mk_renaming_clause c ~renamer ~form sign in
          _renaming_idx := Idx.add !_renaming_idx form (renamer, ref [(def,sign)]);
          Some(renamer, [def], [def])
        ) else None
    )

  let rename_eq ~c lhs rhs sign =
    assert(Type.equal (T.ty lhs) (T.ty rhs));
    assert(Type.is_prop (T.ty lhs));
    rename ~c (T.Form.equiv lhs rhs) sign

  let mk_and ~rule_name and_args c ?(parents=[c]) lit_idx =
    let lits = CCArray.except_idx (C.lits c) lit_idx in
    let proof = proof ~parents ~name:rule_name c  in
    List.map (fun t -> 
      C.create ~penalty:(C.penalty c) ~trail:(C.trail c) 
        (L.mk_true t :: lits) proof) and_args
  
  let mk_or ~rule_name or_args c ?(parents=[c]) lit_idx =
    let lits = 
      (List.map L.mk_true or_args) @
      (CCArray.except_idx (C.lits c) lit_idx) in
    let proof = proof ~parents ~name:rule_name c in
    [C.create ~penalty:(C.penalty c) ~trail:(C.trail c) lits proof]

  let lazy_clausify_driver c =
    fold_lits c
    |> Iter.flat_map_l ( fun (lhs, rhs, sign, pos) -> 
      let i,_ = Ls.Pos.cut pos in
      if T.equal rhs T.true_ then (
        match rename ~c lhs sign with
        | Some (renamer, new_defs, parents) ->
          let rule_name = "renaming" in
          let renamer = (if sign then CCFun.id else T.Form.not_) renamer in
          (mk_or ~rule_name [renamer] c ~parents:(c :: parents) i)
          @ new_defs
        | None -> 
          begin match T.view lhs with 
          | T.AppBuiltin(And, l) when List.length l >= 2 ->
            let rule_name = "lazy_cnf_and" in
            if sign then mk_and l c i ~rule_name
            else mk_or (List.map T.Form.not_ l) c i ~rule_name
          | T.AppBuiltin(Or, l) when List.length l >= 2 ->
            let rule_name = "lazy_cnf_or" in
            if sign then mk_or l c i ~rule_name
            else mk_and (List.map T.Form.not_ l) c i ~rule_name
          | T.AppBuiltin(Imply, [a;b]) ->
            let rule_name = "lazy_cnf_imply" in
            if sign then mk_or [T.Form.not_ a; b] c i ~rule_name
            else mk_and [a; T.Form.not_ b] c i ~rule_name
          | T.AppBuiltin((Equiv|Xor) as hd, [a;b]) ->
            let hd = if sign then hd else (if hd = Equiv then Xor else Equiv) in
            let rule_name = CCFormat.sprintf "lazy_cnf_%a" Builtin.pp hd  in
            if hd = Equiv then (
              mk_or ~rule_name [T.Form.not_ a; b] c i 
              @ mk_or ~rule_name [a; T.Form.not_ b] c i 
            ) else (
              mk_or ~rule_name [T.Form.not_ a; T.Form.not_ b] c i 
              @ mk_or ~rule_name [a; b] c i 
            )
          | T.AppBuiltin((ForallConst|ExistsConst) as hd, [f]) ->
            let free_vars = C.Seq.vars c in
            let var_id = T.Seq.max_var free_vars + 1 in
            let f = Lambda.eta_expand f in
            let var_tys, body =  T.open_fun f in
            assert(List.length var_tys = 1);
            let var_ty = List.hd var_tys in
            let hd, f =
              if sign then hd,f
              else ((if hd=ForallConst then ExistsConst else ForallConst),
                    T.fun_ var_ty (T.Form.not_ body)) in
            let rule_name = 
              CCFormat.sprintf "lazy_cnf_%s" 
                (if hd = ForallConst then "forall" else "exists") in
            let subst_term =
              if hd = ForallConst then (
                T.var @@ HVar.make ~ty:var_ty var_id
              ) else (
                let free_vars_l = 
                    T.VarSet.to_list (T.VarSet.of_seq free_vars) in
                let (id,ty), t = T.mk_fresh_skolem ~prefix:"sk" free_vars_l var_ty in
                E.Ctx.declare id ty;
                t) in
            let res = Lambda.snf @@ T.app f [subst_term] in
            assert(Type.is_prop (T.ty res));
            mk_or ~rule_name [res] c i
          | T.AppBuiltin(Not, _) -> assert false
          | _ -> [] end
      ) else if Type.is_prop (T.ty lhs) && not (T.is_var lhs) then (
          let rule_name = "lazy_cnf_equiv" in
          
          match rename_eq ~c lhs rhs sign with
          | Some (renamer, new_defs, parents) ->
            let rule_name = "renaming" in
            let renamer = (if sign then CCFun.id else T.Form.not_) renamer in
            (mk_or ~rule_name [renamer] c ~parents:(c :: parents) i)
            @ new_defs
          | None ->
            if sign then (
              let not_a_or_b = T.Form.or_ (T.Form.not_ lhs) rhs  in
              let a_or_not_b = T.Form.or_ lhs (T.Form.not_ rhs)  in
              mk_and [not_a_or_b; a_or_not_b] c i ~rule_name
            ) else (
              let a_or_b = T.Form.or_ lhs rhs  in
              let not_a_or_not_b = T.Form.or_ (T.Form.not_ lhs) (T.Form.not_ rhs) in
              mk_and [a_or_b; not_a_or_not_b] c i ~rule_name
      )) else [])

  let lazy_clausify_simpl c =
    let res = Iter.to_list @@ lazy_clausify_driver c  in
    if CCList.is_empty res then None
    else ( 
      update_counter ~action:`Decrease c;
      CCList.iter (update_counter ~action:`Increase) res;
      Some res )

  let lazy_clausify_inf c = 
    let res = Iter.to_list (lazy_clausify_driver c) in
    CCList.iter (update_counter ~action:`Increase) res;
    res
  
  let initialize () =
    Iter.append (E.get_active ()) (E.get_passive ())
    |> Iter.iter (update_counter ~action:`Increase)

  let setup () =
    if !enabled then (
      initialize ();
      match Env.flex_get k_lazy_cnf_kind with 
      | `Inf -> Env.add_unary_inf "lazy_cnf" lazy_clausify_inf;
      | `Simp -> Env.add_multi_simpl_rule lazy_clausify_simpl;
    )
end

let _lazy_cnf_kind = ref `Inf
let _renaming_threshold = ref 8

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_lazy_cnf_kind !_lazy_cnf_kind;
    E.flex_add k_renaming_threshold !_renaming_threshold;

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "lazy_cnf";
    env_actions=[register];
  }

let () =
  Options.add_opts [
    "--lazy-cnf", Arg.Bool ((:=) enabled), " turn on lazy clausification";
    "--lazy-cnf-renaming-threshold", Arg.Int ((:=) _renaming_threshold), 
      " set the subformula renaming threshold";
    "--lazy-cnf-kind", Arg.Symbol (["inf"; "simp"], (fun str -> 
      match str with 
      | "inf" -> _lazy_cnf_kind := `Inf
      | "simp" -> _lazy_cnf_kind := `Simp
      | _ -> assert false)), " use lazy cnf as either simplification or inference"];
  Params.add_to_modes ["ho-complete-basic";
                       "ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      enabled := false;
  );
  Extensions.register extension