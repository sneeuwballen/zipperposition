
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

open Libzipperposition

module Hash = CCHash
module BV = CCBV
module T = FOTerm
module S = Substs
module Lit = Literal
module Lits = Literals

let stat_fresh = Util.mk_stat "fresh_clause"
let stat_clause_create = Util.mk_stat "clause_create"
let prof_clause_create = Util.mk_profiler "clause_create"

module type S = Clause_intf.S

(** {2 Type def} *)
module Make(Ctx : Ctx.S) : S with module Ctx = Ctx = struct
  module Ctx = Ctx
  module BLit = Ctx.BoolBox.Lit
  module Trail = Trail.Make(BLit)

  let pp_trail out trail =
    if not (Trail.is_empty trail)
    then
      Format.fprintf out "@ @<2>← @[<hv>%a@]"
        (CCFormat.seq ~start:"" ~stop:"" ~sep:" ⊓ " Ctx.BoolBox.pp)
        (Trail.to_seq trail)

  type t = {
    lits : Literal.t array; (** the literals *)
    tag : int; (** unique ID of the clause *)
    mutable flags : int; (** boolean flags for the clause *)
    mutable selected : BV.t Lazy.t; (** bitvector for selected lits*)
    mutable proof : t ProofStep.t; (** Proof of the clause *)
    mutable trail : Trail.t; (** boolean trail *)
  }

  type clause = t

  (** {2 boolean flags} *)

  let new_flag =
    let flag_gen = Util.Flag.create () in
    fun () -> Util.Flag.get_new flag_gen

  let flag_lemma = new_flag ()
  let flag_persistent = new_flag ()

  let set_flag flag c truth =
    if truth
    then c.flags <- c.flags lor flag
    else c.flags <- c.flags land (lnot flag)

  let get_flag flag c = (c.flags land flag) != 0

  (** {2 Hashcons} *)

  let equal c1 c2 = c1.tag = c2.tag

  let compare c1 c2 = c1.tag - c2.tag

  let hash_fun c h = Lits.hash_fun c.lits h
  let hash c = Hash.apply hash_fun c

  let id c = c.tag

  let is_ground c = Literals.is_ground c.lits

  let weight c = Lits.weight c.lits

  let trail c = c.trail
  let has_trail c = not (Trail.is_empty c.trail)
  let trail_subsumes c1 c2 = Trail.subsumes c1.trail c2.trail
  let is_active c ~v = Trail.is_active c.trail ~v

  let trail_l = function
    | [] -> Trail.empty
    | [c] -> c.trail
    | [c1; c2] -> Trail.merge c1.trail c2.trail
    | l -> Trail.merge_l (List.map trail l)

  let lits c = c.lits

  module CHashtbl = CCHashtbl.Make(struct
      type t = clause
      let hash = hash
      let equal = equal
    end)

  (** {2 Utils} *)

  let is_goal c = ProofStep.is_goal c.proof

  let distance_to_goal c = ProofStep.distance_to_goal c.proof

  let id_count_ = ref 0

  let create_inner ~selected ~trail lits proof =
    Util.enter_prof prof_clause_create;
    (* create the structure *)
    let c = {
      lits = lits;
      flags = 0;
      tag = ! id_count_;
      selected;
      proof;
      trail;
    } in
    incr id_count_;
    (* return clause *)
    Util.incr_stat stat_clause_create;
    Util.exit_prof prof_clause_create;
    c

  let create_a ~trail lits proof =
    let selected = lazy (Ctx.select lits) in
    create_inner ~trail ~selected lits proof

  let create ~trail lits proof =
    create_a ~trail (Array.of_list lits) proof

  let of_forms ~trail forms proof =
    let lits = List.map Ctx.Lit.of_form forms |> Array.of_list in
    create_a ~trail lits proof

  let of_forms_axiom ~file ~name forms =
    let lits = List.map Ctx.Lit.of_form forms in
    let proof = ProofStep.mk_assert' ~file ~name () in
    create ~trail:Trail.empty lits proof

  let of_statement st =
    let of_lits ~is_goal lits =
      (* convert literals *)
      let lits = List.map Ctx.Lit.of_form lits in
      let src = Statement.src st in
      let proof = if is_goal then ProofStep.mk_goal src else ProofStep.mk_assert src in
      let c = create ~trail:Trail.empty lits proof in
      Some c
    in
    match Statement.view st with
    | Statement.Data _
    | Statement.TyDecl _ -> None
    | Statement.Def (id, ty, t) ->
        let lit = SLiteral.eq (T.const ~ty id) t in
        of_lits ~is_goal:false [lit]
    | Statement.Assert lits -> of_lits ~is_goal:false lits
    | Statement.Goal lits -> of_lits ~is_goal:true lits

  let update_trail f c =
    let trail = f c.trail in
    create_inner ~trail ~selected:c.selected c.lits c.proof

  let proof_step c = c.proof

  let proof c = ProofStep.mk_c c.proof c

  let update_proof c f =
    let new_proof = f c.proof in
    create_a ~trail:c.trail c.lits new_proof

  let is_empty c =
    Lits.is_absurd c.lits && Trail.is_empty c.trail

  let length c = Array.length c.lits

  (** Apply substitution to the clause. Note that using the same renaming for all
      literals is important. *)
  let apply_subst ~renaming subst (c,sc) =
    let lits = Literals.apply_subst ~renaming subst (c.lits,sc) in
    let new_c = create_a ~trail:c.trail lits c.proof in
    new_c

  let _apply_subst_no_simpl subst (lits,sc) =
    if Substs.is_empty subst
    then lits (* id *)
    else
      let renaming = S.Renaming.create () in
      Array.map
        (fun l -> Lit.apply_subst_no_simp ~renaming subst (l,sc))
        lits

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are maximal under [ord] *)
  let maxlits (c,sc) subst =
    let ord = Ctx.ord () in
    let lits' = _apply_subst_no_simpl subst (lits c,sc) in
    Lits.maxlits ~ord lits'

  (** Check whether the literal is maximal *)
  let is_maxlit (c,sc) subst ~idx =
    let ord = Ctx.ord () in
    let lits' = _apply_subst_no_simpl subst (lits c,sc) in
    Lits.is_max ~ord lits' idx

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. *)
  let eligible_res (c,sc) subst =
    let ord = Ctx.ord () in
    let lits' = _apply_subst_no_simpl subst (lits c,sc) in
    let selected = Lazy.force c.selected in
    if BV.is_empty selected
    then (
      (* maximal literals *)
      Lits.maxlits ~ord lits'
    ) else (
      let bv = BV.copy selected in
      let n = Array.length lits' in
      (* Only keep literals that are maximal among selected literals of the
          same sign. *)
      for i = 0 to n-1 do
        (* i-th lit is already known not to be max? *)
        if not (BV.get bv i) then () else
          let lit = lits'.(i) in
          for j = i+1 to n-1 do
            let lit' = lits'.(j) in
            (* check if both lits are still potentially eligible, and have the same
               sign if [check_sign] is true. *)
            if Lit.is_pos lit = Lit.is_pos lit' &&  BV.get bv j
            then match Lit.Comp.compare ~ord lit lit' with
              | Comparison.Incomparable
              | Comparison.Eq -> ()     (* no further information about i-th and j-th *)
              | Comparison.Gt -> BV.reset bv j  (* j-th cannot be max *)
              | Comparison.Lt -> BV.reset bv i  (* i-th cannot be max *)
          done;
      done;
      bv
    )

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. *)
  let eligible_param (c,sc) subst =
    let ord = Ctx.ord () in
    if BV.is_empty (Lazy.force c.selected) then (
      let lits' = _apply_subst_no_simpl subst (lits c,sc) in
      (* maximal ones *)
      let bv = Lits.maxlits ~ord lits' in
      (* only keep literals that are positive equations *)
      BV.filter bv (fun i -> Lit.is_eq lits'.(i));
      bv
    ) else
      BV.empty () (* no eligible literal when some are selected *)

  let is_eligible_param (c,sc) subst ~idx =
    Lit.is_pos c.lits.(idx)
    &&
    BV.is_empty (Lazy.force c.selected)
    &&
    is_maxlit (c,sc) subst ~idx

  (** are there selected literals in the clause? *)
  let has_selected_lits c = not (BV.is_empty (Lazy.force c.selected))

  (** Check whether the literal is selected *)
  let is_selected c i = BV.get (Lazy.force c.selected) i

  (** Indexed list of selected literals *)
  let selected_lits c = BV.selecti (Lazy.force c.selected) c.lits

  (** is the clause a unit clause? *)
  let is_unit_clause c = match c.lits with
    | [|_|] -> true
    | _ -> false

  let is_oriented_rule c =
    let ord = Ctx.ord () in
    match c.lits with
    | [| Lit.Equation (l, r, true) |] ->
        begin match Ordering.compare ord l r with
          | Comparison.Gt
          | Comparison.Lt -> true
          | Comparison.Eq
          | Comparison.Incomparable -> false
        end
    | [| Lit.Prop (_, true) |] -> true
    | _ -> false

  let symbols ?(init=ID.Set.empty) seq =
    Sequence.fold
      (fun set c -> Lits.symbols ~init:set c.lits)
      init seq

  let to_forms c = Lits.Conv.to_forms c.lits

  module Seq = struct
    let lits c = Sequence.of_array c.lits
    let terms c = lits c |> Sequence.flatMap Lit.Seq.terms
    let vars c = terms c |> Sequence.flatMap T.Seq.vars
  end

  (** {2 Filter literals} *)

  module Eligible = struct
    type t = int -> Lit.t -> bool

    let res c =
      let bv = eligible_res (Scoped.make c 0) S.empty in
      fun i _lit -> BV.get bv i

    let param c =
      let bv = eligible_param (Scoped.make c 0) S.empty in
      fun i _lit -> BV.get bv i

    let eq _ lit = match lit with
      | Lit.Equation (_, _, true) -> true
      | _ -> false

    let arith _ lit = Lit.is_arith lit

    let filter f _ lit = f lit

    let max c =
      let bv = lazy (Lits.maxlits ~ord:(Ctx.ord ()) c.lits) in
      fun i _ -> BV.get (Lazy.force bv) i

    let pos _ lit = Lit.is_pos lit

    let neg _ lit = Lit.is_neg lit

    let always _ _ = true

    let combine l = match l with
      | [] -> (fun _ _ -> true)
      | [x] -> x
      | [x; y] -> (fun i lit -> x i lit && y i lit)
      | [x; y; z] -> (fun i lit -> x i lit && y i lit && z i lit)
      | _ -> (fun i lit -> List.for_all (fun eligible -> eligible i lit) l)

    let ( ** ) f1 f2 i lit = f1 i lit && f2 i lit
    let ( ++ ) f1 f2 i lit = f1 i lit || f2 i lit
    let ( ~~ ) f i lit = not (f i lit)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet = CCSet.Make(struct
      type t = clause
      let compare hc1 hc2 = CCInt.compare hc1.tag hc2.tag
    end)

  (** {2 Positions in clauses} *)

  module Pos = struct
    let at c pos = Lits.Pos.at c.lits pos
  end

  module WithPos = struct
    type t = {
      clause : clause;
      pos : Position.t;
      term : T.t;
    }

    let compare t1 t2 =
      let c = t1.clause.tag - t2.clause.tag in
      if c <> 0 then c else
        let c = T.compare t1.term t2.term in
        if c <> 0 then c else
          Position.compare t1.pos t2.pos

    let pp out t =
      Format.fprintf out "@[clause @[%a@]@ at pos @[%a@]@]"
        Lits.pp t.clause.lits Position.pp t.pos
  end


  (** {2 IO} *)

  let pp out c =
    let pp_selected selected out i =
      if BV.get selected i then Format.fprintf out "@{<Black>+@}"
    and pp_maxlit maxlits out i =
      if BV.get maxlits i then Format.fprintf out "@{<Black>*@}"
    in
    (* print literals with a '*' for maximal, and '+' for selected *)
    let selected = Lazy.force c.selected in
    let max = maxlits (Scoped.make c 0) S.empty in
    let pp_lits out lits =
      if Array.length lits = 0
      then CCFormat.string out "⊥"
      else (
        let pp_lit out (i,lit) =
          Format.fprintf out "@[%a%a%a@]"
            Lit.pp lit (pp_selected selected) i (pp_maxlit max) i
        in
        Format.fprintf out "[@[%a@]]"
          (CCFormat.arrayi ~start:"" ~stop:"" ~sep:" ∨ " pp_lit)
          lits
      )
    in
    Format.fprintf out "@[<2>%a%a@]" pp_lits c.lits pp_trail c.trail;
    ()

  (* TODO print trail?! *)
  let pp_tstp out c =
    match c.lits with
    | [| |] -> CCFormat.string out "$false"
    | [| l |] -> Lit.pp_tstp out l
    | _ -> Format.fprintf out "(%a)" Lits.pp_tstp c.lits

  let pp_tstp_full out c =
    Format.fprintf out "@[<2>cnf(%d, plain,@ %a).@]" c.tag pp_tstp c

  let to_string = CCFormat.to_string pp

  let pp_set out set =
    Format.fprintf out "{@[<hv>%a@]}"
      (CCFormat.seq ~start:"" ~stop:"" ~sep:"," pp)
      (ClauseSet.to_seq set)

  let pp_set_tstp out set =
    Format.fprintf out "@[<v>%a@]"
      (CCFormat.seq ~start:"" ~stop:"" ~sep:"," pp_tstp)
      (ClauseSet.to_seq set)
end
