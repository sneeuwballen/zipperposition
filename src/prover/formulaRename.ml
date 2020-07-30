open Logtk

module L = Literal
module T = Term

let section = Util.Section.make ~parent:Const.section "renaming"

module type S = sig
  module Ctx : Ctx.S
  module C : Clause.S with module Ctx = Ctx

  val on_pred_skolem_introduction : (C.t * Term.t) Signal.t

  val is_renaming_clause : C.t -> bool

  val rename_form : 
    ?should_rename:(T.t -> bool) -> c:C.t ->
    T.t -> bool -> (T.t * C.t list * C.t list) option
  val get_skolem : parent:C.t -> mode:[< `Choice | `Skolem ] -> T.t -> T.t
end

module Make(C : Clause.S) = struct
  module Ctx = C.Ctx
  module C = C

  module Idx = Fingerprint.Make(struct 
    type t = T.t * ((C.t * bool) list ref)
    let (<?>) = (CCOrd.Infix.(<?>))
    let compare (a1,c1) (a2,c2) = (T.compare a1 a2)
  end)

  let on_pred_skolem_introduction : (C.t * Logtk.Term.t) Logtk.Signal.t = Signal.create ()

  let sign_present sign = function
    | [(c,sign1)] -> sign1 = sign
    | [(c1, sign1);(c2, sign2)] ->
      if sign1 = sign2 then invalid_arg "signs must be different!";
      true
    | _ -> invalid_arg "only one or two element lists"

  let _skolem_idx = ref @@ Idx.empty ()
  let _renaming_idx = ref @@ Idx.empty ()
  let _renamer_symbols = ref @@ ID.Set.empty

  (* Two-literal clause of which one is a renaming literal
     and the other one is a formula *)
  let is_renaming_clause c =
    let is_renaming_lit = function
      | L.Equation (lhs, _, _) as lit when L.is_predicate_lit lit  ->
        let hd = T.head_term lhs in
        begin match T.head hd with
        | Some id  -> ID.Set.mem id !_renamer_symbols
        | None -> false end
      | _ -> false in
    let is_formula_lit lit = not (is_renaming_lit lit) in
    let ans = 
      match C.lits c with
      | [| a; b |] ->
        CCArray.length (CCArray.filter is_renaming_lit (C.lits c)) = 1 &&
        CCArray.length (CCArray.filter is_formula_lit (C.lits c)) = 1
      | _ -> false
    in
    Util.debugf ~section 1 "@[%a@] is %srenaming" 
      (fun k -> k C.pp c (if ans then "" else "not "));
    ans

  let mk_renaming_clause parent ~renamer ~form sign =
    let proof = Proof.Step.define_internal 
      (T.head_exn renamer) [C.proof_parent parent] in

    let res = 
      if sign then (
        C.create ~penalty:1 ~trail:Trail.empty 
          [L.mk_false renamer; L.mk_true form] proof
      ) else (C.create ~penalty:1 ~trail:Trail.empty 
          [L.mk_true renamer; L.mk_false form] proof
      ) in
    res

  let rename_form ?(should_rename=(fun _ -> true)) ~c form sign =
    assert(Type.is_prop (T.ty form));
    if is_renaming_clause c || T.is_true_or_false form then None
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
        if should_rename form  then (
          let free_vars = T.vars form |> T.VarSet.to_list in
          let (id, ty), renamer =
            T.mk_fresh_skolem ~prefix:"form" free_vars Type.prop in
          Ctx.declare id ty;
          let def = mk_renaming_clause c ~renamer ~form sign in
          _renaming_idx := Idx.add !_renaming_idx form (renamer, ref [(def,sign)]);
          _renamer_symbols := ID.Set.add id !_renamer_symbols;
          Some(renamer, [def], [def])
        ) else None
    )
  
  let get_skolem ~parent ~mode f =
    let free_vars = T.Seq.vars f in
    let var_tys, _ = Type.open_fun (T.ty f) in
    assert(List.length var_tys = 1);
    let ret_ty = List.hd var_tys in
    match mode with 
    | `Skolem ->
      let gen = Iter.head @@ 
      Idx.retrieve_generalizations (!_skolem_idx, 0) (f, 1) in
      begin match gen with 
      | Some (orig, (skolem, _), subst) ->
        Subst.FO.apply Subst.Renaming.none subst (skolem,0) 
      | None ->
        let free_vars_l = 
            T.VarSet.to_list (T.VarSet.of_seq free_vars) in
        let (id,ty), t = T.mk_fresh_skolem ~prefix:"sk" free_vars_l ret_ty in
        Ctx.declare id ty;
        Signal.send on_pred_skolem_introduction (parent, t);
        _skolem_idx := Idx.add !_skolem_idx f (t, ref[]);
        t end
    | `Choice -> T.Form.choice f

end