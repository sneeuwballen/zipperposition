(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

open Logtk
module BV = CCBV
module T = Term
module S = Subst
module Lit = Literal
module Lits = Literals
module Stmt = Statement

type proof = Proof.t

let stat_clause_create = Util.mk_stat "clause.create"

type proof_step = Proof.Step.t

type flag = SClause.flag
(** {2 Type def} *)

(* re-export type, to access fields *)
type sclause = SClause.t = private {
  id: int;
  lits: Literals.t;
  trail: Trail.t;
  mutable flags: flag;
}

type t = {
  sclause: sclause;
  ctx: Ctx.t;  (** context used for creating this clause *)
  mutable penalty: int;  (** heuristic penalty *)
  selected: BV.t Lazy.t;  (** bitvector for selected lits*)
  bool_selected: (Term.t * Position.t) list Lazy.t;
  max_lits: int list Lazy.t;  (** bitvector for maximal lits *)
  mutable proof: proof_step;  (** Proof of the clause *)
  mutable eligible_res: BV.t option; (* eligible for resolution? *)
  mutable eligible_bool: SClause.TPSet.t option;
}

type clause = t

type 'a sets = {
  c_set: 'a CCVector.ro_vector;  (** main set of clauses *)
  c_sos: 'a CCVector.ro_vector;  (** set of support *)
}

module Ctx = Ctx
(** Bundle of clause sets *)

(** {2 boolean flags} *)

let get_flag flag c = SClause.get_flag flag c.sclause
let set_flag flag c b = SClause.set_flag flag c.sclause b
let mark_redundant c = set_flag SClause.flag_redundant c true
let is_redundant c = get_flag SClause.flag_redundant c

let mark_backward_simplified c =
  set_flag SClause.flag_backward_simplified c true

let is_backward_simplified c = get_flag SClause.flag_backward_simplified c

(** {2 Hashcons} *)

let equal c1 c2 = SClause.equal c1.sclause c2.sclause
let compare c1 c2 = SClause.compare c1.sclause c2.sclause
let hash c = SClause.hash c.sclause
let id c = SClause.id c.sclause
let is_ground c = Literals.is_ground c.sclause.lits
let weight c = Lits.weight c.sclause.lits

let ho_weight c =
  Lits.Seq.terms c.sclause.lits
  |> Iter.fold (fun acc t -> T.ho_weight t + acc) 0

let trail c = c.sclause.trail
let has_trail c = not (Trail.is_empty c.sclause.trail)
let trail_subsumes c1 c2 = Trail.subsumes c1.sclause.trail c2.sclause.trail
let is_active c ~v = Trail.is_active c.sclause.trail ~v
let penalty c = c.penalty
let inc_penalty c inc = c.penalty <- c.penalty + inc
let ctx_of c = c.ctx

let trail_l = function
  | [] -> Trail.empty
  | [ c ] -> c.sclause.trail
  | [ c1; c2 ] -> Trail.merge c1.sclause.trail c2.sclause.trail
  | l -> Trail.merge_l (List.map trail l)

let lits c = c.sclause.lits

module Tbl = CCHashtbl.Make (struct
  type t = clause

  let hash = hash
  let equal = equal
end)

(** {2 Utils} *)

let is_goal c = Proof.Step.is_goal c.proof
let distance_to_goal c = Proof.Step.distance_to_goal c.proof
let comes_from_goal c = CCOpt.is_some @@ distance_to_goal c

(* private function for building clauses *)
let create_inner ~ctx ~penalty ~selected ~bool_selected sclause proof =
  (* create the structure *)
  let ord = Ctx.ord ctx in
  let max_lits = lazy (BV.to_list @@ Lits.maxlits sclause.lits ~ord) in
  let c =
    {
      sclause;
      ctx;
      penalty;
      selected;
      bool_selected;
      proof;
      max_lits;
      eligible_res = None;
      eligible_bool = None;
    }
  in
  (* return clause *)
  Util.incr_stat stat_clause_create;
  c

let of_sclause ~ctx ?(penalty = 1) c proof =
  let selected = lazy (Ctx.select ctx c.lits) in
  let bool_selected = lazy (Ctx.bool_select ctx c.lits) in
  create_inner ~ctx ~penalty ~selected ~bool_selected c proof

let lit_is_false_ = function
  | Literal.False -> true
  | _ -> false

let create_a ~ctx ~penalty ~trail lits proof =
  (* remove spurious "false" literals automatically *)
  let lits =
    if CCArray.exists lit_is_false_ lits then
      CCArray.filter (fun lit -> not (lit_is_false_ lit)) lits
    else
      lits
  in
  let selected = lazy (Ctx.select ctx lits) in
  let bool_selected = lazy (Ctx.bool_select ctx lits) in
  create_inner ~ctx ~penalty ~selected ~bool_selected (SClause.make ~trail lits)
    proof

let create ~ctx ~penalty ~trail lits proof =
  create_a ~ctx ~penalty ~trail (Array.of_list lits) proof

let of_forms ~ctx ?(penalty = 1) ~trail forms proof =
  let lits = List.map (Ctx.lit_of_form ctx) forms |> Array.of_list in
  create_a ~ctx ~penalty ~trail lits proof

let of_forms_axiom ~ctx ?(penalty = 1) ~file ~name forms =
  let lits = List.map (Ctx.lit_of_form ctx) forms in
  let proof = Proof.Step.assert' ~file ~name () in
  create ~ctx ~penalty ~trail:Trail.empty lits proof

let of_statement ~ctx ?(convert_defs = false) st =
  let of_lits lits =
    (* convert literals *)
    let lits = List.map (Ctx.lit_of_form ctx) lits in
    let proof = Stmt.proof_step st in
    let c = create ~ctx ~trail:Trail.empty ~penalty:1 lits proof in
    c
  in
  match Stmt.view st with
  | Stmt.Data _ | Stmt.TyDecl _ -> []
  | Stmt.Def _ | Stmt.Rewrite _ ->
    if not convert_defs then
      []
    (*dealt with by rewriting *)
    (* dealt with  *)
    else
      List.map of_lits (Stmt.get_formulas_from_defs st)
  | Stmt.Assert lits -> [ of_lits lits ]
  | Stmt.Goal lits -> [ of_lits lits ]
  | Stmt.Lemma l | Stmt.NegatedGoal (_, l) -> List.map of_lits l

let update_trail f c =
  let sclause = SClause.update_trail f c.sclause in
  create_inner ~ctx:c.ctx ~penalty:c.penalty ~selected:c.selected
    ~bool_selected:c.bool_selected c.sclause c.proof

let proof_step c = c.proof
let proof c = Proof.S.mk c.proof (SClause.mk_proof_res c.sclause)
let proof_depth c = 0 (* not easily computable without modding Proof *)
let proof_parent c = Proof.Parent.from (proof c)

let proof_parent_subst renaming (c, sc) subst =
  Proof.Parent.from_subst renaming (proof c, sc) subst

let update_proof c f =
  let new_proof = f c.proof in
  let sclause = c.sclause in
  let selected = c.selected in
  let bool_selected = c.bool_selected in
  {
    sclause;
    ctx = c.ctx;
    penalty = c.penalty;
    selected;
    bool_selected;
    proof = new_proof;
    max_lits = c.max_lits;
    eligible_res = None;
    eligible_bool = None;
  }

let is_empty c = Lits.is_absurd c.sclause.lits && Trail.is_empty c.sclause.trail
let length c = SClause.length c.sclause

let _apply_subst_no_simpl subst (lits, sc) =
  if Subst.is_empty subst then
    lits
  (* id *)
  else (
    let renaming = S.Renaming.create () in
    Array.map (fun l -> Lit.apply_subst_no_simp renaming subst (l, sc)) lits
  )

let apply_subst ?renaming ?proof ?penalty_inc (c_scoped : t Scoped.t) subst =
  let c = Scoped.get c_scoped in
  let sc = Scoped.scope c_scoped in
  let renaming = CCOpt.get_or ~default:(S.Renaming.create ()) renaming in
  let lits' = _apply_subst_no_simpl subst (lits c, sc) in
  let penalty = c.penalty + CCOpt.get_or ~default:0 penalty_inc in
  let proof = CCOpt.get_or ~default:c.proof proof in
  create_a ~ctx:c.ctx ~penalty ~trail:c.sclause.trail lits' proof

(** Bitvector that indicates which of the literals of [subst(clause)] are
    maximal under [ord] *)
let maxlits (c, sc) subst =
  let ord = Ctx.ord (ctx_of c) in
  if not @@ Subst.is_empty subst then (
    let lits' = _apply_subst_no_simpl subst (lits c, sc) in
    Lits.maxlits ~ord lits'
  ) else
    BV.of_list @@ Lazy.force c.max_lits

(** Check whether the literal is maximal *)
let is_maxlit (c, sc) subst ~idx =
  if not @@ Subst.is_empty subst then (
    let ord = Ctx.ord (ctx_of c) in
    let lits' = _apply_subst_no_simpl subst (lits c, sc) in
    Lits.is_max ~ord lits' idx
  ) else
    BV.get (BV.of_list @@ Lazy.force c.max_lits) idx

(** Bitvector that indicates which of the literals of [subst(clause)] are
    eligible for resolution. *)
let eligible_res (c, sc) subst =
  let ord = Ctx.ord (ctx_of c) in
  let selected = Lazy.force c.selected in
  let bool_selected = Lazy.force c.bool_selected in
  if BV.is_empty selected && CCList.is_empty bool_selected then
    if
      (* maximal literals *)
      not @@ Subst.is_empty subst
    then (
      let lits' = _apply_subst_no_simpl subst (lits c, sc) in
      Lits.maxlits ~ord lits'
    ) else
      BV.of_list @@ Lazy.force c.max_lits
  else (
    let lits' = _apply_subst_no_simpl subst (lits c, sc) in
    let bv = BV.copy selected in
    let n = Array.length lits' in
    (* Only keep literals that are maximal among selected literals of the
        same sign. *)
    for i = 0 to n - 1 do
      (* i-th lit is already known not to be max? *)
      if not (BV.get bv i) then
        ()
      else (
        let lit = lits'.(i) in
        for j = i + 1 to n - 1 do
          let lit' = lits'.(j) in
          (* check if both lits are still potentially eligible, and have the same
             sign if [check_sign] is true. *)
          if Lit.is_positivoid lit = Lit.is_positivoid lit' && BV.get bv j then (
            match Lit.Comp.compare ~ord lit lit' with
            | Comparison.Incomparable | Eq ->
              () (* no further information about i-th and j-th *)
            | Gt | Geq -> BV.reset bv j (* j-th cannot be max *)
            | Lt | Leq -> BV.reset bv i (* i-th cannot be max *)
          )
        done
      )
    done;
    bv
  )

let eligible_res_no_subst c =
  match c.eligible_res with
  | Some r -> r
  | None ->
    let bv = eligible_res (c, 0) Subst.empty in
    c.eligible_res <- Some bv;
    bv

let eligible_subterms_of_bool_ c =
  let ctx = ctx_of c in
  let module PB = Position.Build in
  let starting_positions =
    Lazy.force c.bool_selected
    |> List.map (fun (_, pos) -> Position.Build.of_pos pos)
  in
  let ord = Ctx.ord ctx in
  let res =
    (* directly at position of selected booleans *)
    Lazy.force c.bool_selected
    @
    (* below selected selected booleans *)
    CCList.flat_map
      (fun pb ->
        let pos = Position.Build.to_pos pb in
        let t = Literals.Pos.at (lits c) pos in
        (* selects --subterms-- of given t that are eligible *)
        Bool_selection.all_eligible_subterms ~ord ~pos_builder:pb t
        |> Iter.to_list)
      starting_positions
  in
  SClause.TPSet.of_list res

let eligible_subterms_of_bool c =
  match c.eligible_bool with
  | Some s -> s
  | None ->
    let s = eligible_subterms_of_bool_ c in
    c.eligible_bool <- Some s;
    s

(** Bitvector that indicates which of the literals of [subst(clause)] are
    eligible for paramodulation. That means the literal is positive, no literal
    is selected, and the literal is maximal among literals of [subst(clause)].
*)
let positive_maxlits_ ?max_lits ~ord lits =
  let max_bv =
    match max_lits with
    | Some ml -> BV.of_list ml
    | None -> Lits.maxlits ~ord lits
  in
  let res = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if BV.get max_bv i && Literal.is_positivoid lits.(i) then BV.set res i
  done;
  res

let eligible_param (c, sc) subst =
  let ord = Ctx.ord (ctx_of c) in
  let selected = Lazy.force c.selected in
  if BV.is_empty selected then
    if not @@ Subst.is_empty subst then (
      let lits' = _apply_subst_no_simpl subst (lits c, sc) in
      positive_maxlits_ ~ord lits'
    ) else
      (* if no substitution, we can use the cached max_lits *)
      positive_maxlits_ ?max_lits:(Some (Lazy.force c.max_lits)) ~ord (lits c)
  else
    BV.empty ()

let is_eligible_param (c, sc) subst ~idx =
  let ord = Ctx.ord (ctx_of c) in
  let selected = Lazy.force c.selected in
  (* different from [eligible_param] because it doesn't use the
     lazy max_lits and uses a single lit; it's used for the
     negative side only (res lit is never selected) *)
  if BV.is_empty selected then
    if not @@ Subst.is_empty subst then (
      let lits' = _apply_subst_no_simpl subst (lits c, sc) in
      Lit.is_positivoid lits'.(idx) && Lits.is_max ~ord lits' idx
    ) else
      Lit.is_positivoid (lits c).(idx) && Lits.is_max ~ord (lits c) idx
  else
    false

let has_selected_lits c = not (BV.is_empty (Lazy.force c.selected))
let is_selected c i = BV.get (Lazy.force c.selected) i

let selected_lits c =
  let s = lits c in
  let bv = Lazy.force c.selected in
  List.fold_left (fun acc i -> (s.(i), i) :: acc) [] (BV.to_list bv)

let selected_lits_bv c = Lazy.force c.selected
let bool_selected c = Lazy.force c.bool_selected

(** {2 Properties} *)

let is_unit_clause c = Array.length c.sclause.lits = 1

let is_oriented_rule c =
  let ctx = ctx_of c in
  let lits = c.sclause.lits in
  if Array.length lits <> 2 then
    false
  else (
    match lits.(0), lits.(1) with
    | Literal.Equation (lhs1, rhs1, true), Literal.Equation (lhs2, rhs2, false)
      ->
      let ord = Ctx.ord ctx in
      Ordering.compare ord lhs1 rhs1 = Comparison.Gt
      && Ordering.compare ord lhs2 rhs2 = Comparison.Gt
    | _ -> false
  )

let is_inj_axiom _c = None
let ctx_of c = c.ctx

(** {2 Constructors} *)
let flag_orphan = SClause.new_flag ()

let is_orphaned c = SClause.get_flag flag_orphan c.sclause
let mark_orphaned c = SClause.set_flag flag_orphan c.sclause true

(* internal *)
let symbols ?(init = Name.Set.empty) ?(include_types = false) seq =
  let module S = Name.Set in
  seq
  |> Iter.fold
       (fun acc c ->
         let acc = Lits.symbols ~include_types ~init:acc c.sclause.lits in
         acc)
       init

module Eligible = struct
  type clause = t
  type t = int -> Literal.t -> bool

  let res c =
    let bv = eligible_res_no_subst c in
    fun idx lit -> CCBV.get bv idx

  let _eligible_param : (clause * int -> Subst.t -> CCBV.t) ref =
    ref (fun _ _ -> BV.empty ())

  let param c =
    let bv = !_eligible_param (c, 0) Subst.empty in
    fun idx lit -> CCBV.get bv idx

  let eq = fun _ lit -> Literal.is_eq lit
  let filter f = fun _ lit -> f lit
  let max _c = failwith "Eligible.max unimplemented"
  let pos = fun _ lit -> Literal.is_positivoid lit
  let pos_eq = fun _ lit -> Literal.is_positivoid lit && Literal.is_eq lit
  let neg = fun _ lit -> not (Literal.is_positivoid lit)
  let always _ _ = true
  let combine cs idx lit = List.for_all (fun c -> c idx lit) cs
  let ( ** ) a b idx lit = a idx lit && b idx lit
  let ( ++ ) a b idx lit = a idx lit || b idx lit
  let ( ~~ ) a idx lit = not (a idx lit)
end

let () = Eligible._eligible_param := eligible_param

module Pos = struct
  let at (c : t) p = Literals.Pos.at (lits c) p
end

(** {2 Other conversions} *)

(* Other conversion — of_sclause is publicly available as the constructor above *)
let to_sclause c = c.sclause

let to_forms c =
  let conv lit =
    let open SLiteral in
    match lit with
    | Logtk.Literal.True -> true_
    | Logtk.Literal.False -> false_
    | Logtk.Literal.Equation (l, r, _) ->
      let sign = Logtk.Literal.is_positivoid lit in
      if Type.is_prop (Term.ty l) then
        if Term.is_true_or_false r then
          atom l sign
        else (
          let hd =
            if sign then
              Builtin.Equiv
            else
              Builtin.Xor
          in
          atom (Term.app_builtin ~ty:Type.prop hd [ l; r ]) true
        )
      else if sign then
        eq l r
      else
        neq l r
  in
  List.map conv (Array.to_list (lits c))

let to_s_form c =
  let ctx = Term.Conv.create () in
  let forms = to_forms c in
  let forms =
    List.map (SLiteral.map ~f:(fun t -> Term.Conv.to_simple_term ctx t)) forms
  in
  TypedSTerm.Form.or_ (List.map SLiteral.to_form forms)

let ground_clause c =
  let lits' = Literals.ground_lits (lits c) in
  create_a ~ctx:c.ctx ~penalty:c.penalty ~trail:c.sclause.trail lits'
    (proof_step c)

let eta_reduce _c = None

(** {2 Sets} *)

module ClauseSet = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

(** {2 IO} *)

let pp out c = SClause.pp out c.sclause
let pp_trail out t = Trail.pp out t
let pp_tstp out c = SClause.pp_tstp out c.sclause

let pp_tstp_full out c =
  Format.fprintf out "@[<2>%a%a@]" SClause.pp_trail c.sclause.trail
    SClause.pp_tstp c.sclause

let pp_set out s =
  Format.fprintf out "{%a}"
    (Util.pp_iter ~sep:",@ " pp)
    (Iter.of_seq (ClauseSet.to_seq s))

let pp_set_tstp out s =
  Format.fprintf out "{%a}"
    (Util.pp_iter ~sep:",@ " pp_tstp)
    (Iter.of_seq (ClauseSet.to_seq s))

let pp_tstp_list out l =
  Format.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:"@\n" pp_tstp) l

let to_string c = CCFormat.to_string pp c
let to_string_tstp c = CCFormat.to_string pp_tstp c

(** {2 WithPos} *)

(** {2 Clauses with more data} *)

let _outer_lits = lits

module WithPos = struct
  type elt = t
  type t = elt * Position.t

  let compare (c1, p1) (c2, p2) =
    let open CCOrd in
    compare c1 c2 <?> (Position.compare, p1, p2)

  let equal (c1, p1) (c2, p2) = equal c1 c2 && Position.equal p1 p2
  let hash (c, p) = CCHash.combine2 (hash c) (Position.hash p)
  let pp out (c, p) = Format.fprintf out "(@[%a@],%a)" pp c Position.pp p
  let term (c, p) = Literals.Pos.at (_outer_lits c) p
  let clause (wp : t) = fst wp
  let pos (_, p) = p
  let lits (wp : t) = _outer_lits (fst wp)

  let literals (wp : t) =
    let lit, _ = Literals.Pos.lit_at (_outer_lits (fst wp)) (snd wp) in
    lit

  let make ~clause ~pos = clause, pos

  let is_pos (c, p) =
    let lit, _ = Literals.Pos.lit_at (_outer_lits c) p in
    Literal.is_positivoid lit
end

(** {2 Seq} *)

module Seq = struct
  let lits c = Iter.of_array c.sclause.lits

  let forms c =
    Iter.map
      (fun lit ->
        let open SLiteral in
        match lit with
        | Logtk.Literal.True -> true_
        | Logtk.Literal.False -> false_
        | Logtk.Literal.Equation (l, r, _) ->
          let sign = Logtk.Literal.is_positivoid lit in
          if Type.is_prop (Term.ty l) then
            if Term.is_true_or_false r then
              atom l sign
            else (
              let hd =
                if sign then
                  Builtin.Equiv
                else
                  Builtin.Xor
              in
              atom (Term.app_builtin ~ty:Type.prop hd [ l; r ]) true
            )
          else if sign then
            eq l r
          else
            neq l r)
      (Iter.of_array c.sclause.lits)

  let terms c = Lits.Seq.terms c.sclause.lits
  let vars c = Lits.Seq.vars c.sclause.lits
  let symbols c = Iter.empty (* TODO: implement properly *)
end

(** {2 Types checking} *)

let check_types _c = ()
