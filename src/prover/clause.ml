
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

open Libzipperposition

module BV = CCBV
module T = FOTerm
module S = Subst
module Lit = Literal
module Lits = Literals
module Stmt = Statement

let stat_clause_create = Util.mk_stat "clause.create"
let prof_clause_create = Util.mk_profiler "clause_create"

module type S = Clause_intf.S

type proof_step = Clause_intf.proof_step
type proof = Clause_intf.proof

(** {2 Type def} *)
module Make(Ctx : Ctx.S) : S with module Ctx = Ctx = struct
  module Ctx = Ctx

  type flag = SClause.flag

  (* re-export type, to access fields *)
  type sclause = SClause.t = private {
    id: int;
    lits: Literals.t;
    trail: Trail.t;
    mutable flags: flag;
  }

  type t = {
    sclause : sclause;
    mutable selected : BV.t Lazy.t; (** bitvector for selected lits*)
    mutable proof : proof_step; (** Proof of the clause *)
    mutable eligible_res: BV.t option; (* eligible for resolution? *)
  }

  type clause = t

  (** {2 boolean flags} *)

  let get_flag flag c = SClause.get_flag flag c.sclause
  let set_flag flag c b = SClause.set_flag flag c.sclause b

  let mark_redundant c = set_flag SClause.flag_redundant c true
  let is_redundant c = get_flag SClause.flag_redundant c
  let mark_backward_simplified c = set_flag SClause.flag_backward_simplified c true
  let is_backward_simplified c = get_flag SClause.flag_backward_simplified c

  (** {2 Hashcons} *)

  let equal c1 c2 = SClause.equal c1.sclause c2.sclause

  let compare c1 c2 = SClause.compare c1.sclause c2.sclause

  let hash c = SClause.hash c.sclause

  let id c = SClause.id c.sclause

  let is_ground c = Literals.is_ground c.sclause.lits

  let weight c = Lits.weight c.sclause.lits

  let trail c = c.sclause.trail
  let has_trail c = not (Trail.is_empty c.sclause.trail)
  let trail_subsumes c1 c2 = Trail.subsumes c1.sclause.trail c2.sclause.trail
  let is_active c ~v = Trail.is_active c.sclause.trail ~v

  let trail_l = function
    | [] -> Trail.empty
    | [c] -> c.sclause.trail
    | [c1; c2] -> Trail.merge c1.sclause.trail c2.sclause.trail
    | l -> Trail.merge_l (List.map trail l)

  let lits c = c.sclause.lits

  module CHashtbl = CCHashtbl.Make(struct
      type t = clause
      let hash = hash
      let equal = equal
    end)

  (** {2 Utils} *)

  let is_goal c = ProofStep.is_goal c.proof

  let distance_to_goal c = ProofStep.distance_to_goal c.proof

  (* private function for building clauses *)
  let create_inner ~selected sclause proof =
    Util.enter_prof prof_clause_create;
    (* create the structure *)
    let c = {
      sclause;
      selected;
      proof;
      eligible_res=None;
    } in
    (* return clause *)
    Util.incr_stat stat_clause_create;
    Util.exit_prof prof_clause_create;
    c

  let of_sclause c proof =
    let selected = lazy (Ctx.select c.lits) in
    create_inner ~selected c proof

  let create_a ~trail lits proof =
    let selected = lazy (Ctx.select lits) in
    create_inner ~selected (SClause.make ~trail lits) proof

  let create ~trail lits proof =
    create_a ~trail (Array.of_list lits) proof

  let of_forms ~trail forms proof =
    let lits = List.map Ctx.Lit.of_form forms |> Array.of_list in
    create_a ~trail lits proof

  let of_forms_axiom ~file ~name forms =
    let lits = List.map Ctx.Lit.of_form forms in
    let proof = ProofStep.mk_assert' ~file ~name () in
    create ~trail:Trail.empty lits proof

  let rule_neg_ = ProofStep.mk_rule ~comment:["negate goal to find a refutation"] "neg_goal"
  let rule_cnf_ = ProofStep.mk_rule "cnf"

  module Src_tbl = CCHashtbl.Make(struct
      type t = Stmt.source
      let equal = Stmt.Src.equal
      let hash = Stmt.Src.hash
    end)

  (* used to share the same SClause.t in the proof *)
  let input_proof_tbl_ : ProofStep.t Src_tbl.t = Src_tbl.create 32

  let rec proof_of_stmt src =
    try Src_tbl.find input_proof_tbl_ src
    with Not_found ->
      let p = match Stmt.Src.view src with
        | Stmt.Input (_, Stmt.R_goal) -> ProofStep.mk_goal src
        | Stmt.Input (_, _) -> ProofStep.mk_assert src
        | Stmt.From_file (_, Stmt.R_goal) -> ProofStep.mk_goal src
        | Stmt.From_file (_, _) -> ProofStep.mk_assert src
        | Stmt.Internal _ -> ProofStep.mk_trivial
        | Stmt.Neg srcd -> ProofStep.mk_esa ~rule:rule_neg_ [proof_of_sourced srcd]
        | Stmt.CNF srcd -> ProofStep.mk_esa ~rule:rule_cnf_ [proof_of_sourced srcd]
      in
      Src_tbl.add input_proof_tbl_ src p;
      p

  and proof_of_sourced (res, src) =
    let p = proof_of_stmt src in
    begin match res with
      | Stmt.Sourced_input f ->
        ProofStep.mk_f p f
      | Stmt.Sourced_clause c ->
        let lits = List.map Ctx.Lit.of_form c |> Array.of_list in
        let c = SClause.make ~trail:Trail.empty lits in
        ProofStep.mk_c p c
    end

  let of_statement st =
    let of_lits lits =
      (* convert literals *)
      let lits = List.map Ctx.Lit.of_form lits in
      let proof = proof_of_stmt (Stmt.src st) in
      let c = create ~trail:Trail.empty lits proof in
      c
    in
    match Stmt.view st with
      | Stmt.Data _
      | Stmt.TyDecl _ -> []
      | Stmt.Def _
      | Stmt.RewriteForm _
      | Stmt.RewriteTerm _ -> [] (* dealt with by rewriting *)
      | Stmt.Assert lits -> [of_lits lits]
      | Stmt.Goal lits -> [of_lits lits]
      | Stmt.Lemma l
      | Stmt.NegatedGoal (_,l) -> List.map of_lits l

  let update_trail f c =
    let sclause = SClause.update_trail f c.sclause in
    create_inner ~selected:c.selected sclause c.proof

  let proof_step c = c.proof

  let proof c = ProofStep.mk_c c.proof c.sclause

  let update_proof c f =
    let new_proof = f c.proof in
    create_a ~trail:c.sclause.trail c.sclause.lits new_proof

  let is_empty c =
    Lits.is_absurd c.sclause.lits && Trail.is_empty c.sclause.trail

  let length c = SClause.length c.sclause

  (** Apply substitution to the clause. Note that using the same renaming for all
      literals is important. *)
  let apply_subst ~renaming subst (c,sc) =
    let lits = Literals.apply_subst ~renaming subst (c.sclause.lits,sc) in
    let new_c = create_a ~trail:c.sclause.trail lits c.proof in
    new_c

  let _apply_subst_no_simpl subst (lits,sc) =
    if Subst.is_empty subst
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

  let eligible_res_no_subst c =
    begin match c.eligible_res with
      | Some r -> r
      | None ->
        let bv = eligible_res (c,0) Subst.empty in
        c.eligible_res <- Some bv;
        bv
    end

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
    Lit.is_pos c.sclause.lits.(idx)
    &&
    BV.is_empty (Lazy.force c.selected)
    &&
    is_maxlit (c,sc) subst ~idx

  (** are there selected literals in the clause? *)
  let has_selected_lits c = not (BV.is_empty (Lazy.force c.selected))

  (** Check whether the literal is selected *)
  let is_selected c i = BV.get (Lazy.force c.selected) i

  (** Indexed list of selected literals *)
  let selected_lits c = BV.selecti (Lazy.force c.selected) c.sclause.lits

  (** is the clause a unit clause? *)
  let is_unit_clause c = match c.sclause.lits with
    | [|_|] -> true
    | _ -> false

  let is_oriented_rule c =
    let ord = Ctx.ord () in
    match c.sclause.lits with
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
      (fun set c -> Lits.symbols ~init:set c.sclause.lits)
      init seq

  let to_forms c = Lits.Conv.to_forms c.sclause.lits
  let to_sclause c = c.sclause

  module Seq = struct
    let lits c = Sequence.of_array c.sclause.lits
    let terms c = lits c |> Sequence.flat_map Lit.Seq.terms
    let vars c = terms c |> Sequence.flat_map T.Seq.vars
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
      let bv = lazy (Lits.maxlits ~ord:(Ctx.ord ()) c.sclause.lits) in
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
      let compare c1 c2 = SClause.compare c1.sclause c2.sclause
    end)

  (** {2 Positions in clauses} *)

  module Pos = struct
    let at c pos = Lits.Pos.at c.sclause.lits pos
  end

  module WithPos = struct
    type t = {
      clause : clause;
      pos : Position.t;
      term : T.t;
    }

    let compare t1 t2 =
      let c = SClause.compare t1.clause.sclause t2.clause.sclause in
      if c <> 0 then c else
        let c = T.compare t1.term t2.term in
        if c <> 0 then c else
          Position.compare t1.pos t2.pos

    let pp out t =
      Format.fprintf out "@[clause @[%a@]@ at pos @[%a@]@]"
        Lits.pp t.clause.sclause.lits Position.pp t.pos
  end

  (** {2 IO} *)

  let pp_trail = SClause.pp_trail

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
          (Util.pp_seq ~sep:" ∨ " pp_lit)
          (Sequence.of_array_i lits)
      )
    in
    Format.fprintf out "@[%a@[<2>%a%a@]@]/%d"
      SClause.pp_vars c.sclause pp_lits c.sclause.lits
      SClause.pp_trail c.sclause.trail c.sclause.id;
    ()

  let pp_tstp out c = SClause.pp_tstp out c.sclause

  let pp_tstp_full out c = SClause.pp_tstp_full out c.sclause

  let to_string = CCFormat.to_string pp

  let pp_set out set =
    Format.fprintf out "{@[<hv>%a@]}"
      (Util.pp_seq ~sep:"," pp)
      (ClauseSet.to_seq set)

  let pp_set_tstp out set =
    Format.fprintf out "@[<v>%a@]"
      (Util.pp_seq ~sep:"," pp_tstp)
      (ClauseSet.to_seq set)
end
