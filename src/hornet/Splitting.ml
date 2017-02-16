
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Avatar + Inst-Gen-Eq} *)

open Libzipperposition
open Hornet_types

module T = FOTerm
module C = Clause

let section = Util.Section.make "splitting"

module Make(Ctx : State.CONTEXT) = struct
  module Ctx = Ctx

  let name = "splitting"

  (** {2 Avatar Splitting} *)

  (** Clauses that contain several "components" are split immediately.
      A clause [C] is splittable if it contains subsets of literals
      [C_1 \lor … \lor C_n]  where each [C_i]
      is a component.
      A component [C_i] is a non-empty subset of the literals of the clause,
      that shares no variables with the other [C_j | j≠i] *)

  module Avatar : sig
    val split : C.t -> C.t list option
  end = struct
    let stat_avatar_split = Util.mk_stat "hornet.avatar_split"

    (* for a component [¬p], add [[¬p] => ¬[p]] to SAT *)
    let add_neg_lit_complement (b:bool_lit): unit =
      begin match Bool_lit.view b with
        | A_box_clause r ->
          let c = r.bool_box_clause in
          if C.is_unit_ground c &&
             Lit.is_neg (IArray.get (C.lits c) 0) &&
             Constraint.is_trivial (C.constr c)
          then (
            let lit' = Lit.neg (IArray.get (C.lits c) 0) in
            let c' =
              C.make_l [lit'] Proof.trivial
                ~constr:(C.constr c) ~depth:(C.depth c) ~trail:(C.trail c)
            in
            let b' = Bool_lit.box_clause Ctx.bool_state c' in
            let b_c = [Bool_lit.neg b; Bool_lit.neg b'] in
            Ctx.add_clause Proof.bool_tauto b_c;
          )
        | _ -> ()
      end

    (* union-find that maps vars to list of literals, used for splitting *)
    module UF = UnionFind.Make(struct
        type key = T.var
        type value = Lit.t list
        let equal = HVar.equal Type.equal
        let hash = HVar.hash
        let zero = []
        let merge = List.rev_append
      end)

    (* main Avatar splitting function *)
    let try_split_ lits c : C.t list option =
      assert (IArray.length lits >= 2);
      (* ground literals (each one is its own component) *)
      let cluster_ground = ref [] in
      (* maps each variable to a list of literals. Sets can be merged whenever
         two variables occur in the same literal.  *)
      let uf_vars =
        IArray.to_seq lits
        |> Sequence.flat_map Lit.vars_seq
        |> T.VarSet.of_seq
        |> T.VarSet.to_list
        |> UF.create
      in
      (* literals belong to either their own ground component, or to every
          sets in [uf_vars] associated to their variables *)
      IArray.iter
        (fun lit ->
           let v_opt = Lit.vars_seq lit |> Sequence.head in
           begin match v_opt with
             | None -> (* ground, lit has its own component *)
               cluster_ground := lit :: !cluster_ground
             | Some v ->
               (* merge other variables of the literal with [v] *)
               Lit.vars_seq lit
               |> Sequence.iter
                 (fun v' ->
                    UF.add uf_vars v' [lit];  (* lit is in the equiv class of [v'] *)
                    UF.union uf_vars v v')
           end)
        lits;

      (* now gather all the components as a literal list list *)
      let components = ref [] in
      List.iter (fun lit -> components := [lit] :: !components) !cluster_ground;
      UF.iter uf_vars (fun _ comp -> components := comp :: !components);

      begin match !components with
        | [] -> assert (IArray.length lits=0); None
        | [_] -> None
        | _::_ ->
          (* do a simplification by splitting [c]! *)
          Util.incr_stat stat_avatar_split;
          let proof = Proof.avatar_split c in
          let bool_lits, clauses =
            !components
            |> List.map
              (fun lits ->
                 let rec sub_clause = lazy (
                   C.make_l lits proof
                     ~constr:(C.constr c) ~depth:(C.depth c) ~trail:[b_lit]
                 )
                 and b_lit =
                   lazy (Bool_lit.box_clause Ctx.bool_state (Lazy.force sub_clause))
                 in
                 Lazy.force b_lit, Lazy.force sub_clause)
            |> List.split
          in
          Util.debugf ~section 2 "@[<2>@{<yellow>avatar.split@}@ %a@]" (fun k->k C.pp c);
          Util.debugf ~section 4
            "@[<hv2>avatar_split@ :clause @[%a@]@ :yields (@[<hv>%a@])@ @[:trail %a@]@]"
            (fun k->k C.pp c (Util.pp_list C.pp) clauses Bool_lit.pp_trail (C.trail c));
          (* add boolean constraint: trail(c) => bigor_{name in clauses} name *)
          (* guard for the boolean clause *)
          let guard =
            C.trail c
            |> List.map (fun (lazy blit) -> Bool_lit.neg blit)
          in
          (* also add boolean constraint to pick ≥ 1 components *)
          let bool_clause = guard @ bool_lits in
          Ctx.add_clause proof bool_clause;
          (* shortcut: for [¬p], add clause [[¬p] ⇒ ¬[¬p]] *)
          List.iter add_neg_lit_complement bool_lits;
          Util.debugf ~section 4 "@[<2>constraint clause is@ @[%a@]@]"
            (fun k->k Bool_lit.pp_clause bool_clause);
          (* return the clauses *)
          Some clauses
      end

    let split c : C.t list option = match C.proof c with
      | _ when IArray.length (C.lits c) <= 1 -> None
      | P_avatar_split _ | P_split _ -> None (* by construction, impossible *)
      | _ -> try_split_ (C.lits c) c
  end

  (** Depth limit for inst-gen-eq *)
  module Depth_limit : sig
    val set : int -> unit
    val get : unit -> int
  end = struct
    let d_ = ref 1
    let set = (:=) d_
    let get () = !d_
  end

  (** {2 Inst-Gen-Eq} *)

  module Inst_gen_eq : sig
    val grounding : C.t -> unit
    (** Instantiate the given non-horn clause with grounding subst,
        so as to select its literals depending on the current ground
        interpretation *)

    val assert_ground : bool_ground -> unit
    (** Called when the given boolean ground literal is true.
        This will select some FO literals in clauses whose instance contain
        the bool ground literal *)
  end = struct
    let stat_grounding = Util.mk_stat "hornet.grounding"

    (* ground the literals of [c] *)
    let ground_lits (c:clause): bool_lit IArray.t =
      let subst =
        C.vars_l c
        |> List.map
          (fun v ->
             let t = T.grounding (HVar.ty v) in
             ((v:>InnerTerm.t HVar.t),0), ((t:>InnerTerm.t),0))
        |> Subst.of_list
      in
      let renaming = Ctx.renaming_cleared () in
      let lits =
        C.lits c
        |> IArray.mapi
          (fun i lit ->
             (* ground [lit] using the substitution *)
             let lit' = Lit.apply_subst ~renaming subst (lit,0) in
             assert (Lit.is_ground lit');
             let b_lit = Bool_lit.ground Ctx.bool_state lit' in
             (* register [c] in each ground literal *)
             begin match Bool_lit.view b_lit with
               | A_ground r ->
                 r.bool_ground_instance_of <- (c,i) :: r.bool_ground_instance_of;
               | _ -> assert false
             end;
             b_lit)
      in
      lits

    let grounding (c:C.t): unit =
      begin match C.grounding c with
        | None ->
          Util.incr_stat stat_grounding;
          let b_clause = ground_lits c in
          C.set_grounding c b_clause;
          let b_clause = IArray.to_list b_clause in
          Util.debugf ~section 3
            "@[<2>@{<yellow>inst_gen_eq.grounding@}@ %a@ :grounding %a@]"
            (fun k->k C.pp c Bool_lit.pp_clause b_clause);
          Ctx.add_clause (Proof.bool_grounding c) b_clause;
          ()
        | Some _ -> ()
      end

    let try_select (c:clause)(i:clause_idx)(_:bool_ground): unit =
      begin match C.select c with
        | Some _ -> ()
        | None ->
          (* select the literal of [c] whose instance is [r.bool_ground_lit] *)
          let sel = {
            select_lit=IArray.get (C.lits c) i;
            select_idx=i;
            select_depends=[];
          } in
          C.set_select c sel;
          (* be sure to remove the selection afterwards *)
          Ctx.on_backtrack
            (fun () ->
               C.clear_select c;
               Util.debugf ~section 3
                 "@[<2>@{<yellow>unselect_lit@} `%a`@ :clause %a@]"
                 (fun k->k Lit.pp sel.select_lit C.pp c);
               Ctx.send_event (E_unselect_lit (c,sel)));
          (* select lit *)
          Util.debugf ~section 3 "@[<2>@{<yellow>select_lit@} `%a`@ :clause %a@]"
            (fun k->k Lit.pp sel.select_lit C.pp c);
          Ctx.send_event (E_select_lit (c,sel,C.constr c));
      end

    (* when a boolean literal is asserted, try to select it
       in every clause whose instance it belongs to *)
    let assert_ground (r:bool_ground): unit =
      Util.debugf ~section 5 "(@[assert_ground@ %a@])" (fun k->k Lit.pp r.bool_ground_lit);
      List.iter
        (fun (c,i) -> try_select c i r)
        r.bool_ground_instance_of
  end

  (** {2 Non-Horn Clauses} *)

  let initial_clauses : C.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.to_rev_list

  (* split a clause into Avatar components, then normally *)
  let rec split_clause (c:C.t): unit =
    begin match C.classify c with
      | C.Horn _ -> ()
      | C.General ->
        (* first, try Avatar splitting *)
        begin match Avatar.split c with
          | Some l -> List.iter split_clause l (* recurse *)
          | None when C.is_ground c ->
            assert (C.is_unit_ground c); (* otherwise, avatar would have split *)
            ()
          | None -> Inst_gen_eq.grounding c
        end
    end

  let split_initial_clauses () =
    List.iter split_clause initial_clauses

  let on_assumption (lit:Bool_lit.t): unit =
    begin match Bool_lit.view lit, Bool_lit.sign lit with
      | A_box_clause r, true ->
        Ctx.on_backtrack
          (fun () -> Ctx.send_event (E_remove_component r));
        Ctx.send_event (E_add_component r)
      | A_box_clause _, false -> ()
      | A_ground r, true ->
        (* maybe select some FO literals in reaction *)
        Ctx.on_backtrack
          (fun () -> Ctx.send_event (E_remove_ground_lit r));
        Inst_gen_eq.assert_ground r;
        Ctx.send_event (E_add_ground_lit r);
      | A_ground _, false -> ()
      | A_fresh _, _
        -> ()
    end

  let set_depth_limit d = Depth_limit.set d

  (* TODO: use depth limit in instantiation (put new instances that
     are too deep in a heap?) *)

  (* what to do if a conflict is detected *)
  let on_conflict (trail:Trail.t) (label:Label.t) (proof:Proof.t): unit =
    let non_trivial_instantiations =
      Label.to_seq label
      |> Sequence.filter (fun lc -> not (Labelled_clause.is_empty lc))
      |> Sequence.to_rev_list
    in
    if CCList.is_empty non_trivial_instantiations then (
      (* all instances are trivial (same clause), we can trigger
           conflict in SAT using a conflict clause that combines
           the trail and the label *)
      let neg_trail =
        Trail.bool_lits trail
        |> Bool_lit.Tbl.of_seq_count
        |> Bool_lit.Tbl.keys
        |> Sequence.map Bool_lit.neg
        |> Sequence.to_rev_list
      and neg_ground_lits =
        Label.to_list label
        |> List.map
          (fun lc ->
             assert (Labelled_clause.is_empty lc);
             let sel = lc.lc_sel in
             let idx = sel.select_idx in
             let b_lit = IArray.get (C.grounding_exn lc.lc_clause) idx in
             Bool_lit.neg b_lit)
      in
      let conflict = List.rev_append neg_trail neg_ground_lits in
      Util.debugf ~section 2
        "(@[<2>conflict@ :trail %a@ :label %a@ :conflict %a@])"
        (fun k->k Trail.pp trail Label.pp label Bool_lit.pp_clause conflict);
      Ctx.add_clause proof conflict
    ) else (
      (* at least one labelled clause needs instantiation, so we don't
         have a conflict here. The real conflict with come from the instance. *)
      Util.debugf ~section 2
        "(@[<2>conflict->instantiate@ :trail %a@ :label %a@ :instances (@[%a@])@])"
        (fun k->k Trail.pp trail Label.pp label
            (Util.pp_list Labelled_clause.pp) non_trivial_instantiations);
      let clauses_to_instantiate, new_instances =
        Sequence.of_list non_trivial_instantiations
        |> Sequence.map
          (fun lc ->
             let c = lc.lc_clause in
             let subst = Labelled_clause.to_subst lc in
             assert (not (Subst.is_renaming subst));
             (* apply substitution to create a new instance *)
             let renaming = Ctx.renaming_cleared () in
             let lits' =
               C.lits c
               |> IArray.map (fun lit -> Lit.apply_subst ~renaming subst (lit,0))
             and constr' =
               Constraint.apply_subst ~renaming subst (C.constr c,0)
             in
             let c' =
               C.make lits' (Proof.instance c subst)
                 ~constr:constr' ~trail:Trail.empty ~depth:(C.depth c+1)
             in
             assert (not (C.is_trivial c'));
             (* block this instance from [c] *)
             let new_constr = Labelled_clause.to_dismatch lc in
             assert (not (Dismatching_constr.is_trivial new_constr));
             C.add_dismatch_constr c new_constr;
             Util.debugf ~section 2
               "(@[<2>@{<yellow>inst_gen_eq.instantiate@}@ :clause %a@ :subst %a@ :new_dismatch %a@])"
               (fun k->k C.pp c Subst.pp subst Dismatching_constr.pp new_constr);
             (* be ready to remove and re-add c, and to add c' *)
             (c, lc.lc_sel), c')
        |> Sequence.to_rev_list
        |> List.split
      in
      let clauses_to_instantiate =
        CCList.sort_uniq clauses_to_instantiate
          ~cmp:(CCOrd.pair C.compare
              (CCFun.compose_binop (fun sel->sel.select_idx) CCOrd.int))
      in
      (* these clauses have new constraints, remove then re-add them *)
      List.iter
        (fun (c,sel) -> Ctx.send_event (E_unselect_lit (c, sel)))
        clauses_to_instantiate;
      List.iter
        (fun (c,sel) -> Ctx.send_event (E_select_lit (c, sel, C.constr c)))
        clauses_to_instantiate;
      (* now add the new clauses *)
      List.iter split_clause new_instances;
    )

  let on_event (e:event) =
    begin match e with
      | E_add_component _ | E_remove_component _
      | E_add_ground_lit _ | E_remove_ground_lit _
      | E_select_lit _ | E_unselect_lit _ -> () (* come from here *)
      | E_conflict (trail,label,proof) -> on_conflict trail label proof
      | E_if_sat
      | E_found_unsat _ -> ()
      | E_stage s ->
        begin match s with
          | Stage_start -> split_initial_clauses ()
          | _ -> ()
        end
    end
end

let theory = (module Make : State.THEORY_FUN)

