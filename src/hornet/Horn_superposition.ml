
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(* {1 Superposition on Horn Clauses} *)

open Libzipperposition

module T = FOTerm
module C = Clause
module HC = Horn_clause
module P = Position
module Fmt = CCFormat

open Hornet_types

let section = Util.Section.make "horn_sup"

module Make : State.THEORY_FUN = functor(Ctx : State_intf.CONTEXT) -> struct
  module Ctx = Ctx

  (* index term->clause *)
  module CP_idx = NPDtree.MakeTerm(HC.With_pos)

  module FV_idx = FV_tree.Make(HC)

  let name = "horn_superposition"

  (* a simplification rule *)
  type 'a rule_simp = 'a -> 'a option

  (* a simplification rule yielding multiple clauses *)
  type 'a rule_simp_n = 'a -> 'a list option

  (* simplification that simplifies a list of clauses from the active set,
     removes them, and returns their new version *)
  type 'a rule_back_simp = 'a -> 'a list

  (* an inference rule *)
  type 'a rule_infer = 'a -> 'a list

  (* real depth limit for saturation is [n * depth_limit] *)
  let depth_limit_coeff : int = 2

  module Depth_limit : sig
    val set : int -> unit (** Set the limit on derivations *)
    val get : unit -> int (** Set the limit on derivations *)
  end = struct
    let limit_ : int ref = ref 1
    let set i = assert (i>0); limit_ := depth_limit_coeff * i
    let get () = !limit_
  end

  (** {2 Clause Sets} *)

  type idx_elt = term * HC.With_pos.t

  type relevant_pos =
    | Head of idx_elt list * idx_elt list (* active, passive *)
    | Body0 of idx_elt list (* passive res, passive sup *)

  let positions_body c : _ Sequence.t =
    assert (HC.body_len c > 0);
    Lit.passive_terms
      ~pos:P.(body @@ arg 0 @@ stop)
      Ctx.ord (HC.body0_exn c)

  let relevant_pos (c:HC.t): relevant_pos =
    if HC.body_len c = 0
    then (
      (* unit clause: both active and passive *)
      let head = HC.head c in
      let active =
        Lit.active_terms ~pos:P.(head stop) Ctx.ord head
        |> Sequence.map (fun (t,pos) -> t, (c,pos))
        |> Sequence.to_rev_list
      and passive =
        Lit.passive_terms ~pos:P.(head stop) Ctx.ord head
        |> Sequence.map (fun (t,pos) -> t, (c,pos))
        |> Sequence.to_rev_list
      in
      Head (active, passive)
    ) else (
      (* horn: only passive *)
      let passive =
        positions_body c
        |> Sequence.map (fun (t,pos) -> t, (c,pos))
        |> Sequence.to_rev_list
      in
      Body0 passive
    )

  (* positive unit clauses *)
  module Active_set : sig
    val add : HC.t -> unit

    val mem : HC.t -> bool

    val is_subsumed : HC.t -> bool
    (** Return [true] if the clause is subsumed by the set *)

    val find_subsuming : HC.t -> HC.Set.t
    (** Find all clauses subsumed by this given clause *)

    val remove : HC.t -> unit

    val size: unit -> int

    (* index on the head equation sides of positive unit clauses
       (active res/paramodulation) *)
    val idx_heads : unit -> CP_idx.t

    (* index on subterms of the first body lit of non-unit clauses,
       and on the head of unit clauses
       (passive res/paramodulation) *)
    val idx_sup_into : unit -> CP_idx.t
  end = struct
    let tbl : unit HC.Tbl.t = HC.Tbl.create 512
    let size () = HC.Tbl.length tbl

    let idx_heads_ : CP_idx.t ref = ref (CP_idx.empty ())
    let idx_sup_into_ : CP_idx.t ref = ref (CP_idx.empty ())
    let idx_fv_ : FV_idx.t ref = ref (FV_idx.empty ())

    let idx_heads () = !idx_heads_
    let idx_sup_into () = !idx_sup_into_

    let add c =
      if not (HC.Tbl.mem tbl c) then (
        HC.Tbl.add tbl c ();
        idx_fv_ := FV_idx.add !idx_fv_ c;
        begin match relevant_pos c with
          | Head (active, subs) ->
            idx_heads_ := CP_idx.add_list !idx_heads_ active;
            idx_sup_into_ := CP_idx.add_list !idx_sup_into_ subs
          | Body0 subs ->
            idx_sup_into_ := CP_idx.add_list !idx_sup_into_ subs
        end
      )

    let mem c = HC.Tbl.mem tbl c

    let remove c =
      if HC.Tbl.mem tbl c then (
        HC.Tbl.remove tbl c;
        idx_fv_ := FV_idx.remove !idx_fv_ c;
        begin match relevant_pos c with
          | Head (active, subs) ->
            idx_heads_ := CP_idx.remove_list !idx_heads_ active;
            idx_sup_into_ := CP_idx.remove_list !idx_sup_into_ subs
          | Body0 subs ->
            idx_sup_into_ := CP_idx.remove_list !idx_sup_into_ subs
        end
      )

    let is_subsumed (c:HC.t): bool =
      FV_idx.retrieve_subsuming_c !idx_fv_ c
      |> Sequence.exists
        (fun c' -> HC.subsumes_pred c' c)

    let find_subsuming (c:HC.t): HC.Set.t =
      let set =
        FV_idx.retrieve_subsumed_c !idx_fv_ c
        |> Sequence.filter
          (fun c' -> HC.subsumes_pred c c')
        |> HC.Set.of_seq
      in
      set
  end

  let kill_clauses (l:HC.t list): unit =
    List.iter
      (fun c ->
         if HC.is_alive c then (
           HC.kill c;
           assert (HC.is_dead c);
           Active_set.remove c; (* if it's active, not any more *)
         ))
      l

  let remove_all_box (r:bool_box_clause): unit =
    let l = r.bool_box_depends in
    r.bool_box_depends <- [];
    kill_clauses l

  let remove_all_select (r:select_lit): unit =
    let l = r.select_depends in
    r.select_depends <- [];
    kill_clauses l

  module Passive_set : sig
    val add : HC.t -> unit
    val add_seq : HC.t Sequence.t -> unit
    val update_depth_limit: unit -> unit
    val has_too_deep_clauses: unit -> bool (** are there clauses frozen b.c. of their depth? *)
    val next : unit -> HC.t option
  end = struct
    (* heuristic weights for selecting clauses *)
    module W = struct
      type t = HC.t -> int

      (* weighted combination *)
      let combine (ws:(t * int) list): t =
        assert (ws <> []);
        assert (List.for_all (fun (_,c) -> c > 0) ws);
        fun c ->
          List.fold_left
            (fun sum (w,coeff) -> sum + coeff * w c)
            0 ws

      let all_terms c: term Sequence.t =
        (Sequence.append (Lit.seq_terms (HC.head c))
           (HC.body c |> IArray.to_seq |> Sequence.flat_map Lit.seq_terms))

      let weight_lits (c:HC.t): int =
        all_terms c
        |> Sequence.map T.weight
        |> Sequence.fold (+) 0

      let depth_lits (c:HC.t): int =
        all_terms c
        |> Sequence.map T.depth
        |> Sequence.max
        |> CCOpt.get_or ~default:0

      let depth_ty (c:HC.t): int =
        all_terms c
        |> Sequence.flat_map T.Seq.subterms
        |> Sequence.map FOTerm.ty
        |> Sequence.map Type.depth
        |> Sequence.max ?lt:None
        |> CCOpt.get_or ~default:0

      let num_vars_lits (c:HC.t): int =
        all_terms c
        |> Sequence.flat_map T.Seq.vars
        |> Sequence.length

      let unordered_depth = HC.unordered_depth

      let favor_ground: t = fun c -> if HC.is_ground c then 0 else 10

      let favor_pos_unit: t = fun c -> if HC.is_unit_pos c then 0 else 10
    end

    (* "weight" of a clause.
       favor small clauses with shallow, ground terms *)
    let weight : W.t =
      W.combine
        [ W.depth_ty, 5;
          W.weight_lits, 2;
          W.depth_lits, 2;
          W.num_vars_lits, 3;
          W.favor_ground, 1;
          W.unordered_depth, 1;
          W.favor_pos_unit, 5;
        ]

    (* priority queue *)
    module H = CCHeap.Make(struct
        type t = (int * HC.t)
        let leq (i1, c1) (i2, c2): bool =
          i1 < i2 || (i1 = i2 && HC.compare c1 c2 <= 0)
      end)

    let prof_passive = Util.mk_profiler "hornet.passive_set"
    let stat_passive_add = Util.mk_stat "hornet.passive_add"

    (* queue of clauses to process (passive) *)
    let q_ : H.t  ref = ref H.empty

    (* queue for clauses that are deeper than the current limit *)
    let too_deep_ : H.t ref = ref H.empty

    let has_too_deep_clauses () = not (H.is_empty !too_deep_)

    let add_ c =
      if HC.is_dead c then () (* useless *)
      else if Active_set.mem c then () (* already active *)
      else (
        let depth = HC.unordered_depth c in
        if depth < Depth_limit.get () then (
          Util.debugf ~section 3 "@[<2>add `%a`@ to passive set@]"
            (fun k->k HC.pp c);
          Util.incr_stat stat_passive_add;
          q_ := H.add !q_ (weight c,c)
        ) else (
          too_deep_ := H.add !too_deep_ (depth,c);
        )
      )

    let add c = Util.with_prof prof_passive add_ c

    let add_seq = Sequence.iter add

    (* new depth limit -> some clauses become active *)
    let update_depth_limit () =
      let d = Depth_limit.get() in
      let rec aux q = match H.take q with
        | Some (new_q, (d_c,c)) when d_c < d ->
          (* [c] becomes passive *)
          add c;
          aux new_q
        | Some _ | None -> q
      in
      too_deep_ := aux !too_deep_

    (* find the next alive clause *)
    let rec next () = match H.take !q_ with
      | None -> None
      | Some (new_q, (_,c)) ->
        q_ := new_q;
        if HC.is_dead c
        then next() (* discard dead clause *)
        else Some c
  end

  (** {2 Superposition} *)
  module Sup : sig
    val rule_infer_active : HC.t rule_infer
    val rule_infer_passive : HC.t rule_infer
    val rule_eq_resolution : HC.t rule_infer
    val rule_destr_eq_resolution : HC.t rule_simp
    val rule_demod : HC.t rule_simp
    val rule_back_demod : HC.t rule_back_simp
  end = struct
    let stat_infer = Util.mk_stat "hornet.steps_sup_infer"
    let stat_eq_res = Util.mk_stat "hornet.steps_eq_res"
    let stat_destr_eq_res = Util.mk_stat "hornet.steps_destr_res"
    let stat_demod_call = Util.mk_stat "hornet.calls_demod"
    let stat_demod_step = Util.mk_stat "hornet.steps_demod"
    let stat_back_demod_step = Util.mk_stat "hornet.steps_back_demod"
    let prof_infer_active = Util.mk_profiler "hornet.sup_active"
    let prof_infer_passive = Util.mk_profiler "hornet.sup_passive"
    let prof_eq_res = Util.mk_profiler "hornet.eq_res"
    let prof_demod = Util.mk_profiler "hornet.demod"
    let prof_back_demod = Util.mk_profiler "hornet.back_demod"

    (* do the inference, if it is needed *)
    let do_sup_inference (sup:hc_superposition_step): HC.t option =
      let c, sc_active = sup.hc_sup_active in
      assert (HC.body_len c=0);
      assert (not (T.is_var sup.hc_sup_rewritten));
      let c', sc_passive = sup.hc_sup_passive in
      assert (HC.is_alive c);
      assert (HC.is_alive c');
      let subst = sup.hc_sup_subst in
      let renaming = Ctx.renaming_cleared () in
      let s = sup.hc_sup_s in
      let s' = Subst.FO.apply ~renaming subst (s,sc_active) in
      let t' = Subst.FO.apply ~renaming subst (sup.hc_sup_t,sc_active) in
      (* passive lit and equation *)
      let passive_lit, passive_lit_pos = match sup.hc_sup_passive_pos with
        | P.Body (P.Arg (0, p)) -> HC.body0_exn c', p
        | P.Head p -> HC.head c', p
        | _ -> assert false
      in
      let u', v' = match Lit.get_eqn passive_lit passive_lit_pos with
        | Some (u, v, sign) ->
          assert sign;
          Subst.FO.apply ~renaming subst (u,sc_passive),
          Subst.FO.apply ~renaming subst (v,sc_passive)
        | _ -> assert false
      in
      (* check ordering on [s>t] and [u>v], with possibility of non-decreasing
         inference at the cost of a depth increase *)
      let cmp_s_t = Ordering.compare Ctx.ord s' t' in
      let cmp_u_v = Ordering.compare Ctx.ord u' v' in
      let ord_ok = match cmp_s_t, cmp_u_v with
        | Comparison.Gt, (Comparison.Gt | Comparison.Eq) -> true
        | Comparison.Lt, _
        | _, Comparison.Lt -> false (* ill-ordered *)
        | Comparison.Eq, _ -> false (* trivial *)
        | Comparison.Incomparable, (Comparison.Gt | Comparison.Eq)
        | Comparison.Gt, Comparison.Incomparable -> true
        | Comparison.Incomparable, Comparison.Incomparable -> true (* ouch. *)
      in
      let unordered_depth () =
        (* malus applied to this particular inference. an inference is
           safe if:
           - [s'] is ground, [t'] too, [s'>t'], passive clause is unit or ground
        *)
        let step =
          if (T.is_ground s' &&
              cmp_s_t = Comparison.Gt &&
              (assert (T.is_ground t'); true) &&
              (HC.is_ground c' || HC.is_unit_pos c'))
          then 0
          else 1
        in
        HC.unordered_depth c + HC.unordered_depth c' + step
      in
      (* check for some trivial inference: using [s=t] to rewrite [s=t] *)
      let will_be_trivial () =
        HC.body_len c' = 0 &&
        Lit.equal (HC.head c) (HC.head c') &&
        T.equal s' u' && T.equal t' v'
      in
      (* if all conditions met, do the inference *)
      if ord_ok && not (will_be_trivial ())
      then (
        (* inference is a go *)
        let new_head = Lit.apply_subst ~renaming subst (HC.head c',sc_passive) in
        let new_body =
          HC.body c'
          |> IArray.map_arr
            (fun lit -> Lit.apply_subst ~renaming subst (lit,sc_passive))
        in
        let new_head, new_body = match sup.hc_sup_passive_pos with
          | P.Head pos' ->
            Lit.Pos.replace new_head ~at:pos' ~by:t',
            IArray.of_array_unsafe new_body
          | P.Body (P.Arg (0,pos')) ->
            new_body.(0) <- Lit.Pos.replace new_body.(0) ~at:pos' ~by:t';
            new_head, IArray.of_array_unsafe new_body
          | _ -> assert false
        in
        let constr =
          Constraint.combine
            (Constraint.apply_subst ~renaming subst (HC.constr c,sc_active))
            (Constraint.apply_subst ~renaming subst (HC.constr c',sc_passive))
        and trail =
          Trail.merge (HC.trail c) (HC.trail c')
        and label =
          Label.merge
            (Label.apply_subst ~renaming subst (HC.label c,sc_active))
            (Label.apply_subst ~renaming subst (HC.label c',sc_passive))
        in
        let unordered_depth = unordered_depth() in
        let new_c =
          HC.make ~unordered_depth ~constr ~trail ~label
            new_head new_body (Proof.hc_sup sup)
        in
        Util.incr_stat stat_infer;
        Util.debugf ~section 4
          "(@[<hv2>superposition_step@ :yields %a@ :params %a@ :depth %d@])"
          (fun k->k HC.pp new_c Hornet_types_util.pp_hc_sup sup unordered_depth);
        Some new_c
      ) else None

    (* perform active superposition rewriting [s] into [t] *)
    let active_sup (c:HC.t) (pos_s:P.t) (s:T.t) (t:T.t): HC.t list =
      let idx = Active_set.idx_sup_into () in
      let sc_active = 0 in
      let sc_passive = 1 in
      CP_idx.retrieve_unifiables (idx,sc_passive) (s,sc_active)
      |> Sequence.filter_map
        (fun (u_p,c'_with_pos,subst) ->
           let c', pos' = c'_with_pos in
           let sup = {
             hc_sup_active=(c,sc_active);
             hc_sup_passive=(c',sc_passive);
             hc_sup_s=s;
             hc_sup_t=t;
             hc_sup_subst=subst;
             hc_sup_rewritten=u_p;
             hc_sup_active_pos=pos_s;
             hc_sup_passive_pos=pos';
           } in
           do_sup_inference sup)
      |> Sequence.to_rev_list

    (* try to use this clause to rewrite other clauses *)
    let rule_infer_active_ (c:HC.t) : _ list =
      if HC.body_len c = 0 then (
        begin match HC.head c with
          | Lit.Atom (t, true) ->
            active_sup c P.(head @@ left @@ stop) t T.true_
          | Lit.Eq (s, t, true) ->
            begin match Ordering.compare Ctx.ord s t with
              | Comparison.Gt -> active_sup c P.(head @@ left @@ stop) s t
              | Comparison.Lt -> active_sup c P.(head @@ right @@ stop) t s
              | Comparison.Eq -> []
              | Comparison.Incomparable ->
                List.rev_append
                  (active_sup c P.(head @@ left @@ stop) s t)
                  (active_sup c P.(head @@ right @@ stop) t s)
            end
          | Lit.Atom (_,false) | Lit.Eq (_,_,false) -> assert false
          | Lit.Bool _ -> []
        end
      ) else []

    let passive_sup (c:HC.t): _ list =
      let idx = Active_set.idx_heads () in
      let sc_active = 0 in
      let sc_passive = 1 in
      (* all position that can be rewritten *)
      let pos_seq =match relevant_pos c with
        | Head (_,subs) | Body0 subs -> subs
      in
      Sequence.of_list pos_seq
      |> Sequence.flat_map
        (fun (rewritten,(_,pos_rewritten)) ->
           (* try to rewrite this sub-term *)
           CP_idx.retrieve_unifiables (idx,sc_active) (rewritten,sc_passive)
           |> Sequence.filter_map
             (fun (s,c'_with_pos,subst) ->
                let c', pos' = c'_with_pos in
                let pos_lit' = match pos' with P.Head p'->p' | _ -> assert false in
                let t = match Lit.get_eqn (HC.head c') pos_lit' with
                  | Some (s_, t, true) ->
                    (*Format.eprintf "hd %a, s=`@[%a@]`, s_=`@[%a@]` pos %a@."
                      Lit.pp (HC.head c') T.pp s T.pp s_ P.pp pos_lit';*)
                    assert (T.equal s s_); t
                  | _ ->
                    Format.eprintf "hd %a, pos %a@." Lit.pp (HC.head c') P.pp pos_lit';
                    assert false
                in
                let sup = {
                  hc_sup_active=(c',sc_active);
                  hc_sup_passive=(c,sc_passive);
                  hc_sup_s=s;
                  hc_sup_t=t;
                  hc_sup_subst=subst;
                  hc_sup_rewritten=rewritten;
                  hc_sup_active_pos=pos';
                  hc_sup_passive_pos=pos_rewritten;
                } in
                do_sup_inference sup
             ))
      |> Sequence.to_rev_list

    (* equality resolution *)
    let eq_res (c:HC.t): _ list = match HC.body0 c with
      | None -> []
      | Some (Lit.Eq (a,b,true)) ->
        let sc = 0 in
        begin
          try
            let subst = Unif.FO.unification (a,sc) (b,sc) in
            (* do inference, by removing [a=b] from body *)
            let renaming = Ctx.renaming_cleared () in
            let new_head =
              Lit.apply_subst ~renaming subst (HC.head c,sc)
            and new_body =
              HC.body_tail c
              |> IArray.map_arr
                (fun lit -> Lit.apply_subst ~renaming subst (lit,sc))
              |> IArray.of_array_unsafe
            and proof = Proof.hc_eq_res c subst
            and constr = Constraint.apply_subst ~renaming subst (HC.constr c,sc)
            and label =
              Label.apply_subst ~renaming subst (HC.label c,sc)
            in
            let c' =
              HC.make new_head new_body proof
                ~unordered_depth:(HC.unordered_depth c)
                ~trail:(HC.trail c) ~constr ~label
            in
            Util.debugf ~section 4
              "(@[<hv2>eq_res@ :on %a@ :subst %a@ :yield %a@])"
              (fun k->k HC.pp c Subst.pp subst HC.pp c');
            Util.incr_stat stat_eq_res;
            [c']
          with Unif.Fail -> []
        end
      | Some (Lit.Atom (_,true))  (* TODO: E-unif for true? *)
      | Some (Lit.Bool _) -> []
      | Some (Lit.Eq (_,_,false) | Lit.Atom (_,false)) -> assert false

    (* destructive equality resolution.
       Works on every literal of the body, not only the first one. *)
    let rule_destr_eq_resolution (c:HC.t): _ option =
      let lits = HC.body c in
      let lit_replace =
        IArray.to_seqi lits
        |> Sequence.find
          (fun (i,lit) -> match lit with
             | Lit.Eq (a,b,true) ->
               begin match T.view a, T.view b with
                 | T.Var x, _ when not (T.var_occurs ~var:x b) -> Some (i,x,b)
                 | _, T.Var x when not (T.var_occurs ~var:x a) -> Some (i,x,a)
                 | _ -> None
               end
             | _ -> None)
      in
      begin match lit_replace with
        | None -> None
        | Some (i, x, t) ->
          (* replace [x] by [t] in the clause, and remove the literal *)
          let sc = 0 in
          let subst =
            Subst.bind Subst.empty ((x:>InnerTerm.t HVar.t),0) ((t:>InnerTerm.t),0)
          in
          let new_body =
            IArray.init (IArray.length lits-1)
              (fun j-> if j<i then IArray.get lits j else IArray.get lits (j+1))
          in
          let renaming = Ctx.renaming_cleared () in
          let new_head = Lit.apply_subst ~renaming subst (HC.head c,sc)
          and new_body = Lit.apply_subst_arr ~renaming subst (new_body,sc)
          and proof = Proof.hc_eq_res c subst
          and constr = Constraint.apply_subst ~renaming subst (HC.constr c,sc)
          and label =
            Label.apply_subst ~renaming subst (HC.label c,sc)
          in
            let c' =
              HC.make new_head new_body proof
                ~unordered_depth:(HC.unordered_depth c)
                ~trail:(HC.trail c) ~constr ~label
            in
            Util.debugf ~section 4
              "(@[<hv2>destr_eq_res@ :on %a@ :subst %a@ :yield %a@])"
              (fun k->k HC.pp c Subst.pp subst HC.pp c');
            Util.incr_stat stat_destr_eq_res;
            Some c'
      end

    (* Compute normal form of term w.r.t active set. Clauses used to
       restrict is an option for restricting demodulation
       in positive unit clauses.
       add rewriting rules to [rules] *)
    let demod_nf ~restrict (passive:HC.t)(rules:HC.Set.t ref) (t:term): term =
      let ord = Ctx.ord in
      let idx = Active_set.idx_heads () in
      let sc_active = 1 in
      let sc_passive = 0 in
      (* compute normal form of subterm. If restrict is true, substitutions that
         are variable renamings are forbidden (since we are at root of a max term) *)
      let rec reduce_at_root ~restrict t =
        (* find an equation l=r that match subterm *)
        let matching_rule =
          CP_idx.retrieve_generalizations (idx, sc_active) (t, sc_passive)
          |> Sequence.find
            (fun (l, c_with_pos, subst) ->
               let active, pos = c_with_pos in
               assert (HC.is_unit_pos active);
               let lit_pos = match pos with P.Head p -> p | _ -> assert false in
               let l', r, sign = Lit.get_eqn_exn (HC.head active) lit_pos in
               assert (sign && T.equal l l');
               (* check ordering conditions and restriction.
                  if [restrict], we cannot rewrite [t] with itself,
                  only with terms that are strictly more general
                  (avoid self-demodulation of the rewrite rule) *)
               let ok =
                 (not restrict || not (Unif.FO.matches ~pattern:t l))
                 && Trail.subsumes (HC.trail active) (HC.trail passive)
                 && Ordering.compare ord
                       (Subst.FO.apply_no_renaming subst (l,sc_active))
                       (Subst.FO.apply_no_renaming subst (r,sc_active)) = Comparison.Gt
                 && Label.subsumes_pred ~subst
                   (HC.label active,sc_active)(HC.label passive,sc_passive)
                 && Constraint.subsumes
                   (HC.constr active,sc_active)(HC.constr passive,sc_passive)
               in
               if ok then (
                 rules := HC.Set.add active !rules;
                 Util.incr_stat stat_demod_step;
                 Some (r, subst)
               ) else
                 None)
        in
        begin match matching_rule with
          | None -> t
          | Some (t', subst) ->
            Util.debugf ~section 5 "(@[<2>demod@ :old `@[%a@]`@ :new `@[%a@]`@])"
              (fun k->k T.pp t T.pp t');
            normal_form ~restrict subst t' 1 (* done one rewriting step, continue *)
        end
      (* rewrite innermost-leftmost of [subst(t,scope)]. The initial scope is
         0, but then we normal_form terms in which variables are really the variables
         of the RHS of a previously applied rule (in context 1); all those
         variables are bound to terms in context 0 *)
      and normal_form ~restrict subst t scope =
        begin match T.view t with
          | T.App (f, l) ->
            begin match T.view f with
              | T.DB _
              | T.Var _ -> Subst.FO.apply_no_renaming subst (t, scope)
              | T.Const _ ->
                (* rewrite subterms in call by value *)
                let l' =
                  List.map (fun t' -> normal_form ~restrict:false subst t' scope) l
                and f' =
                  Subst.FO.apply_no_renaming subst (f, scope)
                in
                (* avoid rebuilding term if nothing changed *)
                let t' =
                  if T.equal f f' && T.same_l l l'
                  then t
                  else T.app f' l'
                in
                (* rewrite term at root *)
                reduce_at_root ~restrict t'
              | T.App _
              | T.AppBuiltin _ -> assert false
            end
          | _ -> Subst.FO.apply_no_renaming subst (t,scope)
        end
      in
      normal_form ~restrict Subst.empty t 0

    let demod_lit ~head passive (rules:HC.Set.t ref)(lit:lit): lit =
      begin match lit with
        | Atom (p,sign) ->
          let p = demod_nf ~restrict:head passive rules p in
          Lit.atom ~sign p
        | Eq (a,b,sign) when head ->
          (* head literal: restrict the bigger side(s) *)
          let c = Ordering.compare Ctx.ord a b in
          let restrict_a, restrict_b = match c with
            | Comparison.Gt -> true, false
            | Comparison.Lt -> false, true
            | Comparison.Incomparable -> true, true
            | Comparison.Eq -> false, false
          in
          (* demod with given restriction *)
          Lit.eq ~sign
            (demod_nf ~restrict:restrict_a passive rules a)
            (demod_nf ~restrict:restrict_b passive rules b)
        | Eq (a,b,sign) ->
          (* demod without restriction *)
          Lit.eq ~sign
            (demod_nf ~restrict:false passive rules a)
            (demod_nf ~restrict:false passive rules b)
        | Bool _ -> lit
      end

    let demod_ (c:HC.t): HC.t option =
      Util.incr_stat stat_demod_call;
      let rules = ref HC.Set.empty in
      let new_head = demod_lit ~head:true c rules (HC.head c)
      and new_body =
        (* only demodulate first body literal *)
        IArray.mapi
          (fun i lit ->
             if i=0 then demod_lit ~head:false c rules lit else lit)
          (HC.body c)
      in
      (* rewriting happened iff there is at least one rule *)
      if not (HC.Set.is_empty !rules) then (
        let rules = HC.Set.to_list !rules in
        let new_c : HC.t =
          HC.make new_head new_body
            (Proof.hc_demod c rules)
            ~constr:(HC.constr c) ~trail:(HC.trail c)
            ~unordered_depth:(HC.unordered_depth c) ~label:(HC.label c)
        in
        Util.debugf ~section 3
          "(@[<2>demod@ :clause %a@ :into %a@ :rules {@[<hv>%a@]}@])"
          (fun k->k HC.pp c HC.pp new_c (Util.pp_list HC.pp) rules);
        Some new_c
      ) else None

    (* find clauses that can be demodulated by the given positive unit eqn.
       returns the list of demodulated clauses, removed from active set *)
    let back_demod_ (c:HC.t): HC.t list =
      let idx = Active_set.idx_sup_into () in
      let renaming = Ctx.renaming_cleared () in
      (* find clauses that might be rewritten by l -> r *)
      let find_candidates ~oriented set l r =
        CP_idx.retrieve_specializations (idx,1) (l,0)
        |> Sequence.filter_map
          (fun (_,c'_with_pos,subst) ->
             let c', _ = c'_with_pos in
             (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
             if (oriented ||
                  Ordering.compare Ctx.ord
                    (Subst.FO.apply ~renaming subst (l,0))
                    (Subst.FO.apply ~renaming subst (r,0)) = Comparison.Gt) &&
                Trail.subsumes (HC.trail c) (HC.trail c')
             then Some c' (* add the clause to the set, it may be rewritten by l -> r *)
             else None)
        |> HC.Set.add_seq set
      in
      let set = HC.Set.empty in
      (* gather all candidates *)
      let candidates =
        if HC.is_unit_pos c
        then match HC.head c with
          | Lit.Eq (l,r,true) ->
            begin match Ordering.compare Ctx.ord l r with
              | Comparison.Gt -> find_candidates ~oriented:true set l r
              | Comparison.Lt -> find_candidates ~oriented:true set r l
              | _ ->
                (* both sides can rewrite, but we need to check ordering *)
                let set = find_candidates ~oriented:false set l r in
                find_candidates ~oriented:false set r l
            end
          | _ -> HC.Set.empty
        else HC.Set.empty
      in
      (* try to simplify candidates by demod now *)
      let final_set =
        HC.Set.to_seq candidates
        |> Sequence.filter_map
          (fun c' -> match demod_ c' with
             | None -> None
             | Some new_c ->
               (* clause is simplified, remove it and return its new version *)
               Util.incr_stat stat_back_demod_step;
               Active_set.remove c';
               Some new_c)
        |> Sequence.to_rev_list
      in
      final_set

    let rule_infer_active c =
      Util.with_prof prof_infer_active rule_infer_active_ c

    let rule_infer_passive (c:HC.t): _ list =
      Util.with_prof prof_infer_passive passive_sup c

    let rule_demod c =
      Util.with_prof prof_demod demod_ c

    let rule_back_demod c =
      Util.with_prof prof_back_demod back_demod_ c

    let rule_eq_resolution c : _ list =
      Util.with_prof prof_eq_res eq_res c
  end

  (** {2 Avatar} *)

  (** Part of the Avatar reasoning. Here we do simplifications related
      to boolean literals that have been {b proved} by the SAT solver,
      that is, that are propagated at level 0 *)

  module Avatar : sig
    val has_trivial_trail : HC.t -> bool
    val simplify_trail : HC.t rule_simp
  end = struct
    let stat_trail_trivial = Util.mk_stat "hornet.avatar_trivial_trail"
    let stat_trail_simplify = Util.mk_stat "hornet.steps_avatar_simplify_trail"

    (* check whether the trail is false and will remain so *)
    let trail_is_trivial_ (trail:Trail.t): bool =
      let res =
        Trail.exists
          (fun lit -> match Ctx.valuation_at_level0 lit with
             | Some false -> true (* false at level 0: proven false *)
             | _ -> false)
          trail
      in
      if res then (
        Util.incr_stat stat_trail_trivial;
        Util.debugf ~section 3 "(@[<2>trail @[%a@]@ is trivial@])"
          (fun k->k Trail.pp trail);
      );
      res

    let has_trivial_trail (c:HC.t) =
      trail_is_trivial_ (HC.trail c)

    (* simplify the trail of [c] using boolean literals that have been proven *)
    let simplify_trail (c:HC.t): HC.t option =
      let trail = HC.trail c in
      let n_simpl = ref 0 in
      (* remove bool literals made trivial by SAT solver *)
      let trail, trivial_trail =
        trail
        |> List.partition
          (fun (lazy lit) -> match Ctx.valuation_at_level0 lit with
             | Some true ->
               (* [lit] is proven true, it is therefore not necessary
                    to depend on it *)
               incr n_simpl;
               false
             | _ -> true)
      in
      if !n_simpl > 0 then (
        Util.incr_stat stat_trail_simplify;
        (* use SAT resolution proofs for tracking why the trail
           has been simplified, so that the other branches that have been
           closed can appear in the proof *)
        let proof_removed =
          List.map (fun (lazy l) -> l, Ctx.proof_of_lit l) trivial_trail
        in
        let proof = Proof.avatar_cut c proof_removed in
        let new_c : HC.t =
          HC.make (HC.head c) (HC.body c) proof
            ~trail ~constr:(HC.constr c) ~label:(HC.label c)
            ~unordered_depth:(HC.unordered_depth c)
        in
        Util.incr_stat stat_trail_simplify;
        Util.debugf ~section 3
          "(@[<hv2>avatar_cut@ :clause %a@ :into @[%a@]@ :lits %a@])"
          (fun k->k HC.pp c HC.pp new_c Trail.pp trivial_trail);
        Some new_c
      ) else None
  end

  (** {2 Simplifications} *)

  module Simplifications : sig
    val rules_simp_fast : HC.t rule_simp list
    val rules_simp_full : HC.t rule_simp list
    val rules_simp_n : HC.t rule_simp_n list
    val rules_back_simp : HC.t rule_back_simp list
  end = struct
    let stat_simp_body = Util.mk_stat "hornet.simp_body"

    (* simplification of first body literal *)
    let simp_body0 c: HC.t option = match HC.body0 c with
      | None -> None
      | Some (Lit.Bool true) ->
        (* trivial body literal, remove *)
        let c' =
          HC.make
            ~constr:(HC.constr c) ~trail:(HC.trail c)
            ~unordered_depth:(HC.unordered_depth c) ~label:(HC.label c)
            (HC.head c) (HC.body_tail c) (Proof.hc_simplify c)
        in
        Util.incr_stat stat_simp_body;
        Some c'
      | Some (Lit.Eq (t, u, true)) when T.equal t u ->
        (* [a=a] -> true *)
        let c' =
          HC.make
            ~constr:(HC.constr c) ~trail:(HC.trail c)
            ~unordered_depth:(HC.unordered_depth c) ~label:(HC.label c)
            (HC.head c) (HC.body_tail c) (Proof.hc_simplify c)
        in
        Util.incr_stat stat_simp_body;
        Some c'
      | Some lit when not (Lit.sign lit) -> assert false
      | Some _ -> None

    let rules_simp_fast = [ simp_body0; Avatar.simplify_trail ]

    (* TODO: rewriting for deduction modulo *)

    let rules_simp_full =
      rules_simp_fast @ [ Sup.rule_destr_eq_resolution; Sup.rule_demod; ]

    let rules_simp_n = [ ]
    let rules_back_simp = [ Sup.rule_back_demod ]
  end

  (** {2 Saturation} *)

  (** Keeps a set of clauses that are saturated up to some limit.
      The limit is on derivations: a clause that has been derived using
      "too many" non-decreasing steps is thrown away.
      This is sufficient for saturation to always terminate.

      The state is a set of Horn clauses, and is backtrackable. *)

  let rules_infer : HC.t rule_infer list =
    [ Sup.rule_infer_active;
      Sup.rule_infer_passive;
      Sup.rule_eq_resolution;
    ]

  module Saturate : sig
    type res =
      | Sat
      | Unknown (* reached depth limit *)
      | Unsat of HC.t (* empty clause *)

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    val pp_stats : stats CCFormat.printer

    val stats : unit -> stats

    val add_clauses : C.t list -> res
    (** Add a list of clauses.
        For each [c], adds the clause [c] to the set, and applies
        Avatar splitting and Inst_gen_eq to it. *)

    val add_horn : HC.t -> res
    (** [add_horn c] adds the clause [c]  to the set, and saturates it
        again. If, during saturation, the empty clause is derived,
        [Unsat l] is returned (where [l] is a non-empty list of empty clauses).
        Otherwise, [Sat] is returned. *)

    val saturate : ?full:bool -> unit -> res
    (** Saturate again. Should be called after every increase of depth,
        since it might unlock some clauses that were too deep till now.
        @param full if true, ignore the saturation limit (default false) *)
  end = struct
    type res =
      | Sat
      | Unknown (* reached depth limit *)
      | Unsat of HC.t

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    let pp_stats out s: unit =
      Fmt.fprintf out "{@[num_clauses: %d@]}" s.num_clauses

    (** {6 Local State} *)

    let stats (): stats = {
      num_clauses=Active_set.size();
    }

    (** {6 Saturation} *)

    (* simplify using given rules *)
    let simplify rules0 c =
      let rec aux rules c = match rules with
        | [] -> c
        | r :: rules_tail ->
          begin match r c with
            | None -> aux rules_tail c
            | Some c' -> aux rules0 c' (* from start *)
          end
      in
      let new_c = aux rules0 c in
      if not (HC.equal c new_c) then (
        Util.debugf ~section 5 "(@[<2>simplify@ `%a`@ :into `%a`@])"
          (fun k->k HC.pp c HC.pp new_c);
      );
      new_c

    let simplify_fast = simplify Simplifications.rules_simp_fast
    let simplify_full = simplify Simplifications.rules_simp_full

    (* apply the "splitting" rules *)
    let simplify_n rules0 c =
      let res = ref [] in
      let rec aux rules c = match rules with
        | [] ->
          (* re-simplify c *)
          let c = simplify_fast c in
          CCList.Ref.push res c
        | r :: rules_tail ->
          begin match r c with
            | None -> aux rules_tail c
            | Some l -> List.iter (aux rules0) l (* from start for each new clause *)
          end
      in
      aux rules0 c;
      !res

    (* apply backward simplification rules (simplifies active set using [c]) *)
    let back_simplify c =
      CCList.flat_map
        (fun r -> r c)
        Simplifications.rules_back_simp

    let stat_loop_count = Util.mk_stat "hornet.saturation_iter_count"

    (* the main saturation loop *)
    let rec saturation_loop n =
      if n=0
      then
        if Passive_set.has_too_deep_clauses ()
        then Unknown
        else Sat
      else saturate_next n
    and saturate_next n = match Passive_set.next () with
      | None ->
        if Passive_set.has_too_deep_clauses ()
        then Unknown
        else Sat
      | Some c ->
        Util.incr_stat stat_loop_count;
        Util.debugf ~section 2
          "@[<2>@{<Blue>## saturate@}: given clause@ %a@]"
          (fun k->k HC.pp c);
        let c = simplify_full c in
        if HC.is_trivial c || Avatar.has_trivial_trail c then (
          saturation_loop n
        ) else if HC.is_absurd c then (
          Util.debugf ~section 2 "@[<2>@{<Green>found empty clause@}@ %a@]"
            (fun k->k HC.pp c);
          Unsat c
        ) else if Active_set.mem c then (
          Util.debugf ~section 4 "clause %a already in active set, continue"
            (fun k->k HC.pp c);
          saturation_loop n
        ) else if Active_set.is_subsumed c then (
          Util.debugf ~section 4 "clause %a subsumed by active set, continue"
            (fun k->k HC.pp c);
          saturation_loop n
        ) else (
          (* remove clauses subsumed by [c] *)
          let set = Active_set.find_subsuming c in
          HC.Set.iter
            (fun c' ->
               Util.debugf ~section 4 "active clause %a@ subsumed by %a,@ remove it"
                 (fun k->k HC.pp c' HC.pp c);
               Active_set.remove c')
            set;
          (* add to [c] *)
          Active_set.add c;
          (* backward simplifications *)
          let back_simplified =
            back_simplify c |> Sequence.of_list
          in
          (* infer new clauses *)
          let inferred =
            Sequence.of_list rules_infer
            |> Sequence.flat_map_l (fun rule -> rule c)
          in
          (* simplify all new clauses, send to passive set *)
          let new_c : HC.t Sequence.t =
            (Sequence.append back_simplified inferred)
            |> Sequence.map simplify_fast
            |> Sequence.flat_map_l (simplify_n Simplifications.rules_simp_n)
            |> Sequence.filter (fun c -> not (HC.is_trivial c))
          in
          Passive_set.add_seq new_c;
          saturation_loop (n-1) (* did one step *)
        )

    let saturate ?(full=false) () =
      let n_steps = if full then max_int else Ctx.saturation_steps in
      saturation_loop n_steps

    let add_horn c : res =
      Util.debugf ~section 2 "@[<2>@{<yellow>saturate.add_horn@}@ %a@]"
        (fun k->k HC.pp c);
      let c = simplify_fast c in
      (* this clause might be currently dead, but give it another chance *)
      Passive_set.add c;
      saturate ()

    let add_clause c =
      Util.debugf ~section 3 "@[<2>saturate.add_clause@ %a@]"
        (fun k->k C.pp c);
      begin match C.classify c with
        | C.Horn hc -> add_horn hc
        | C.General -> Sat (* wait until it is split *)
      end

    let rec add_clauses (l:C.t list) = match l with
      | [] -> Sat
      | c :: tail ->
        begin match add_clause c with
          | Sat | Unknown -> add_clauses tail
          | Unsat p -> Unsat p
        end
  end

  (** {2 Interface to literal-selection} *)
  module Select : sig
    val add_select : clause -> select_lit -> c_constraint -> Saturate.res
  end = struct
    let add_select (c:clause) (sel:select_lit) (constr:c_constraint): Saturate.res =
      let lit = sel.select_lit in
      let head, body =
        if Lit.is_pos lit
        then lit, IArray.empty
        else Lit.false_, IArray.make 1 (Lit.neg lit)
      in
      let hc =
        HC.make head body
          ~constr ~label:[Labelled_clause.make_empty c sel]
          ~trail:Trail.empty ~unordered_depth:0
          (Proof.split c sel constr)
      in
      Saturate.add_horn hc
  end

  (** {2 Main} *)

  let initial_clauses : C.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.to_rev_list

  (* check result of saturation and trigger appropriate events *)
  let check_res (res:Saturate.res): unit =
    begin match res with
      | Saturate.Unknown
      | Saturate.Sat -> ()
      | Saturate.Unsat c ->
        assert (HC.is_absurd c);
        Ctx.send_event (E_conflict (HC.trail c, HC.label c, HC.proof c));
    end

  let set_depth_limit d =
    Depth_limit.set d;
    Passive_set.update_depth_limit ();
    (* some clauses might have become active *)
    check_res (Saturate.saturate ());
    ()

  let presaturate () =
    (* add the set of initial clauses *)
    Util.debugf ~section 2 "@{<Yellow>## start presaturation ##@}"(fun k->k);
    let res = Saturate.add_clauses initial_clauses in
    check_res res

  (* no direct communication with SAT solver *)
  let on_assumption _ = ()

  let on_event e =
    begin match e with
      | E_add_component r ->
        (* assume this component *)
        begin match C.classify r.bool_box_clause with
          | C.Horn c ->
            HC.start_new_cycle();
            HC.make_alive_again c;
            check_res (Saturate.add_horn c)
          | C.General -> ()
        end
      | E_remove_component r -> remove_all_box r
      | E_select_lit (c,sel,constr) ->
        check_res (Select.add_select c sel constr)
      | E_unselect_lit (_,r) -> remove_all_select r
      | E_stage Stage_presaturate -> presaturate ()
      | E_stage Stage_exit ->
        Util.debugf ~section 1 "@[<2>saturate:@ %a@]"
          (fun k->
             let stats = Saturate.stats() in
             k Saturate.pp_stats stats);
        ()
      | E_stage (Stage_start | Stage_init) -> ()
      | E_remove_ground_lit _ -> ()
      | E_if_sat ->
        (* saturate again, fully *)
        check_res (Saturate.saturate ~full:true ())
      | E_add_ground_lit _
      | E_conflict _
      | E_found_unsat _ -> ()
    end
end

let theory : State.theory_fun = (module Make)

