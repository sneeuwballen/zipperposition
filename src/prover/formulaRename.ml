open Logtk
module L = Literal
module T = Term

let section = Util.Section.make ~parent:Const.section "renaming"

module type S = sig
  module Ctx : module type of Ctx
  module C : Clause_intf.S with module Ctx = Ctx

  val on_pred_skolem_introduction : (C.t * Term.t) Signal.t
  val is_renaming_clause : flex_ref:Flex_state.t ref -> C.t -> bool

  val rename_form :
    ?should_rename:(T.t -> bool) ->
    ?polarity_aware:bool ->
    flex_ref:Flex_state.t ref ->
    c:C.t ->
    T.t ->
    bool ->
    (T.t * C.t list * C.t list) option

  val get_skolem :
    flex_ref:Flex_state.t ref ->
    parent:C.t ->
    mode:[< `Choice | `SkolemRecycle | `SkolemAlwaysFresh ] ->
    T.t ->
    T.t
end

module Make (C : Clause_intf.S) = struct
  module Ctx = C.Ctx
  module C = C

  module Idx = Fingerprint.Make (struct
    type t = T.t * (C.t * bool option) list ref

    let ( <?> ) = CCOrd.Infix.( <?> )
    let compare (a1, c1) (a2, c2) = T.compare a1 a2
  end)

  let on_pred_skolem_introduction : (C.t * Logtk.Term.t) Logtk.Signal.t =
    Signal.create ()

  type rename_state = {
    mutable skolem_idx: Idx.t;
    mutable renaming_idx: Idx.t;
    mutable renamer_symbols: Name.Set.t;
  }

  let k_state : rename_state Flex_state.key = Flex_state.create_key ()

  let mk_rename_state () =
    {
      skolem_idx = Idx.empty ();
      renaming_idx = Idx.empty ();
      renamer_symbols = Name.Set.empty;
    }

  let get_state (flex_ref : Flex_state.t ref) : rename_state =
    match Flex_state.get k_state !flex_ref with
    | Some st -> st
    | None ->
      let st = mk_rename_state () in
      flex_ref := Flex_state.add k_state st !flex_ref;
      st

  (* Two-literal clause of which one is a renaming literal
     and the other one is a formula *)
  let is_renaming_clause ~flex_ref c =
    let st = get_state flex_ref in
    let is_renaming_lit = function
      | L.Equation (lhs, _, _) as lit when L.is_predicate_lit lit ->
        let hd = T.head_term lhs in
        (match T.head hd with
        | Some id -> Name.Set.mem id st.renamer_symbols
        | None -> false)
      | _ -> false
    in
    let is_formula_lit lit = not (is_renaming_lit lit) in
    let ans =
      match C.lits c with
      | [| a; b |] ->
        CCArray.length (CCArray.filter is_renaming_lit (C.lits c)) = 1
        && CCArray.length (CCArray.filter is_formula_lit (C.lits c)) = 1
      | _ -> false
    in
    Util.debugf ~section 1 "@[%a@] is %srenaming" (fun k ->
        k C.pp c
          (if ans then
             ""
           else
             "not "));
    ans

  let mk_renaming_clause parent ~polarity_aware ~renamer ~form sign =
    let proof =
      Proof.Step.define_internal (T.head_exn renamer) [ C.proof_parent parent ]
    in

    let res =
      if polarity_aware then
        if sign then
          C.create ~ctx:(C.ctx_of parent) ~penalty:1 ~trail:Trail.empty
            [ L.mk_false renamer; L.mk_true form ]
            proof
        else
          C.create ~ctx:(C.ctx_of parent) ~penalty:1 ~trail:Trail.empty
            [ L.mk_true renamer; L.mk_false form ]
            proof
      else
        C.create ~ctx:(C.ctx_of parent) ~penalty:1 ~trail:Trail.empty
          [ L.mk_eq renamer form ]
          proof
    in

    res

  let rename_form ?(should_rename = fun _ -> true) ?(polarity_aware = true)
      ~flex_ref ~c form sign =
    let st = get_state flex_ref in
    assert (Type.is_prop (T.ty form));
    let mk_sign sign =
      if polarity_aware then
        Some sign
      else
        None
    in
    let defined_with_sign sign = function
      | [ (_, Some sign') ] -> sign = sign'
      | [ (_, Some s); (_, Some s') ] ->
        assert (s != s');
        false
      | [ (_, None) ] -> false
      | _ -> invalid_arg "wrong definition state"
    in

    if is_renaming_clause ~flex_ref c || T.is_true_or_false form then
      None
    else (
      let gen =
        Iter.head @@ Idx.retrieve_generalizations (st.renaming_idx, 0) (form, 1)
      in
      match gen with
      | Some (orig, (renamer, defined_as), subst) ->
        let renamer_sub =
          Subst.FO.apply Subst.Renaming.none subst (renamer, 0)
        in

        (* it might be that at one moment we tried to do the renaming in
           polarity aware mode and then at the other in non-polarity aware mode *)
        let renamer_sub, new_defs, parents =
          (* if we are now trying to define in non-PA mode and there is already
             a polarity aware definition -- simply insert the other polarity *)
          if
            (not polarity_aware)
            && (defined_with_sign sign !defined_as
               || defined_with_sign (not sign) !defined_as)
          then (
            let sign =
              if defined_with_sign sign !defined_as then
                not sign
              else
                sign
            in
            let def =
              mk_renaming_clause c ~polarity_aware:true ~renamer ~form:orig sign
            in
            defined_as := (def, Some sign) :: !defined_as;
            renamer_sub, [ def ], !defined_as
          ) else if polarity_aware then
            if
              (* if we are doing PA and the sign is present in one form or the
              other, OK *)
              defined_with_sign sign !defined_as
              ||
              match !defined_as with
              | [ (_, Some _); (_, Some _) ] -> true
              | [ (_, None) ] -> true
              | _ -> false
            then
              renamer_sub, [], !defined_as
            else (
              (* if the sign is missing add it*)
              let def =
                mk_renaming_clause c ~polarity_aware ~renamer ~form:orig sign
              in
              defined_as := (def, Some sign) :: !defined_as;
              renamer_sub, [ def ], !defined_as
            )
          else (
            assert (
              match !defined_as with
              | [ (_, None) ] -> true
              | [ (_, Some _); (_, Some _) ] -> true
              | _ -> false);
            renamer_sub, [], !defined_as
          )
        in
        Some (renamer_sub, new_defs, CCList.map fst parents)
      | None ->
        (* maybe we need to define it if it appears too many times *)
        if should_rename form then (
          let free_vars = T.vars form |> T.VarSet.to_list in
          let (id, ty), renamer =
            T.mk_fresh_skolem ~prefix:"form" free_vars Type.prop
          in
          Ctx.declare (C.ctx_of c) id ty;
          let def = mk_renaming_clause c ~renamer ~polarity_aware ~form sign in
          st.renaming_idx <-
            Idx.add st.renaming_idx form (renamer, ref [ def, mk_sign sign ]);
          st.renamer_symbols <- Name.Set.add id st.renamer_symbols;
          Some (renamer, [ def ], [ def ])
        ) else
          None
    )

  let get_skolem ~flex_ref ~parent ~mode f =
    let st = get_state flex_ref in
    let free_vars = T.Seq.vars f in
    let var_tys, _ = Type.open_fun (T.ty f) in
    assert (List.length var_tys = 1);
    let ret_ty = List.hd var_tys in

    let mk_skolem () =
      let free_vars_l = T.VarSet.to_list (T.VarSet.of_iter free_vars) in
      let (id, ty), t = T.mk_fresh_skolem ~prefix:"sk" free_vars_l ret_ty in
      Ctx.declare (C.ctx_of parent) id ty;
      Signal.send on_pred_skolem_introduction (parent, t);
      t
    in

    match mode with
    | `SkolemRecycle ->
      let gen =
        Iter.head @@ Idx.retrieve_generalizations (st.skolem_idx, 0) (f, 1)
      in
      (match gen with
      | Some (orig, (skolem, _), subst) ->
        Subst.FO.apply Subst.Renaming.none subst (skolem, 0)
      | None ->
        let t = mk_skolem () in
        st.skolem_idx <- Idx.add st.skolem_idx f (t, ref []);
        t)
    | `SkolemAlwaysFresh -> mk_skolem ()
    | `Choice -> T.Form.choice f
end
