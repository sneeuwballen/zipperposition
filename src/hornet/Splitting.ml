
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
             C.constr c = []
          then (
            let lit' = Lit.neg (IArray.get (C.lits c) 0) in
            let c' = C.make_l ~trail:(C.trail c) [lit'] Proof.trivial in
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
                   C.make_l ~trail:[b_lit] lits proof
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
    val instantiate : C.t -> unit
    (** Instantiate the given non-horn clause, so as to select
        its literals depending on the current ground interpretation *)

    val assert_ground : bool_ground -> unit
    (** Called when the given boolean ground literal is true.
        This will select some FO literals in clauses whose instance contain
        the bool ground literal *)
  end = struct
    let stat_instantiate = Util.mk_stat "hornet.instantiate"

    (* TODO:
       - rule to ground non-ground clauses (with a pointer to the grounding)
       - rule to select a horn subset in non-Horn non-ground clause components
    *)


    (* TODO:
       - ground the clause, register it to each ground literal
       - add the grounding to SAT
       - remember to select a literal in [on_assumption]
    *)
    let instantiate _ =
      Util.incr_stat stat_instantiate;
      assert false

    (* TODO: when a conflict between selected lits is found, add
       instantiations *)

    let try_select (c:clause)(i:clause_idx)(r:bool_ground): unit =
      begin match C.select c with
        | Some _ -> ()
        | None ->
          (* select the literal of [c] whose instance is [r.bool_ground_lit] *)
          let sel = {
            select_lit=IArray.get (C.lits c) i;
            select_idx=i; (* TODO *)
            select_depends=[];
          } in
          C.set_select c sel;
          Ctx.send_event (E_select_lit (c,sel,C.dismatch_constr c));
          (* remove the selection afterwards *)
          Ctx.on_backtrack
            (fun () ->
               C.clear_select c;
               Ctx.send_event (E_unselect_lit (c,sel)));
      end

    (* when a boolean literal is asserted, try to select it
       in every clause whose instance it belongs to *)
    let assert_ground (r:bool_ground): unit =
      List.iter
        (fun (c,i) -> try_select c i r)
        r.bool_ground_instance_of
  end

  (** {2 Non-Horn Clauses} *)

  let initial_clauses : C.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.to_rev_list

  (* TODO: on assumption [Lit some_ground_lit], select the corresponding
     literal in every FO clause whose grounding contains [some_ground_lit]. *)

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
          | None -> Inst_gen_eq.instantiate c
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
        Ctx.on_backtrack
          (fun () -> Ctx.send_event (E_remove_ground_lit r));
        (* maybe select some FO literals in reaction *)
        Inst_gen_eq.assert_ground r;
        Ctx.send_event (E_add_ground_lit r)
      | A_ground _, false -> ()
      | A_fresh _, _
        -> ()
    end

  let set_depth_limit d = Depth_limit.set d

  let on_event (e:event) =
    begin match e with
      | E_add_component _ | E_remove_component _
      | E_add_ground_lit _ | E_remove_ground_lit _
      | E_select_lit _ | E_unselect_lit _ -> () (* come from here *)
      | E_conflict (c,proof) ->
        (* trigger conflict in SAT *)
        Ctx.add_clause proof c
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

