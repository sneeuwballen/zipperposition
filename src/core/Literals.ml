
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Array of literals} *)

module BV = CCBV
module T = Term
module S = Subst
module Lit = Literal

type term = Term.t

type t = Literal.t array

let prof_maxlits = ZProf.make "lits.maxlits"

let equal lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
      Lit.equal lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
  then false
  else check 0

let equal_com lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
      Lit.equal_com lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
  then false
  else check 0

let compare = CCArray.compare Lit.compare

let compare_multiset ~ord (l1:t) (l2:t) =
  let module M = Multiset.Make(Literal) in
  M.compare_partial_l (Literal.Comp.compare ~ord)
    (Array.to_list l1) (Array.to_list l2)

let hash lits = Hash.array Lit.hash lits

let variant ?(subst=S.empty) (a1,sc1) (a2,sc2) =
  Unif.unif_array_com ~size:`Same (subst,[]) (a1,sc1) (a2,sc2)
    ~op:(fun (subst,t1) x y k ->
        Lit.variant ~subst x y (fun (s,t2) -> k (s,t1@t2)))
  |> Iter.filter (fun (s,_) -> Subst.is_renaming s)

let are_variant a1 a2 =
  not (Iter.is_empty (variant (Scoped.make a1 0) (Scoped.make a2 1)))

let matching ?(subst=S.empty) ~pattern:(a1,sc1) (a2,sc2) =
  Unif.unif_array_com ~size:`Same (subst,[]) (a1,sc1) (a2,sc2)
    ~op:(fun (subst,t1) x y k ->
        Lit.matching ~subst ~pattern:x y
          (fun (s,t2) -> k (s,t1@t2)))

let matches a1 a2 =
  not (Iter.is_empty (matching ~pattern:(Scoped.make a1 0) (Scoped.make a2 1)))

let weight lits =
  Array.fold_left (fun w lit -> w + Lit.weight lit) 0 lits

let ho_weight lits = 
  Array.fold_left (fun w lit -> w + Lit.ho_weight lit) 0 lits

let depth lits =
  Array.fold_left (fun d lit -> max d (Lit.depth lit)) 0 lits

let vars lits =
  Iter.of_array lits
  |> Iter.flat_map Lit.Seq.vars
  |> T.VarSet.of_iter
  |> T.VarSet.to_list

let is_ground lits =
  CCArray.for_all Lit.is_ground lits

let to_form lits =
  let lits = Array.map Lit.Conv.to_form lits in
  Array.to_list lits

(** Apply the substitution to the array of literals, with scope *)
let apply_subst renaming subst (lits,sc) =
  Array.map
    (fun lit -> Lit.apply_subst renaming subst (lit,sc))
    lits

let of_unif_subst renaming s =
  Literal.of_unif_subst renaming s |> Array.of_list

let map f lits =
  Array.map (fun lit -> Lit.map f lit) lits

(** bitvector of literals that are positive *)
let pos lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_positivoid lits.(i) then BV.set bv i
  done;
  bv

(** bitvector of literals that are positive *)
let neg lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_negativoid lits.(i) then BV.set bv i
  done;
  bv

(** Multiset of literals, with their index *)
module MLI = Multiset.Make(struct
    type t = Lit.t * int
    let compare (l1,i1)(l2,i2) =
      if i1 = i2 then Lit.compare l1 l2 else Stdlib.compare i1 i2
  end)

let _compare_lit_with_idx ~ord (lit1,i1) (lit2,i2) =
  if i1 = i2
  then Comparison.Eq (* ignore collisions *)
  else (
    let c = Lit.Comp.compare ~ord lit1 lit2 in
    (* two occurrences of one lit should be incomparable (and therefore maximal) *)
    if c = Comparison.Eq then Incomparable
    else c
  )

let _to_multiset_with_idx lits =
  CCArray.foldi
    (fun acc i x -> MLI.add acc (x,i))
    MLI.empty lits

(* TODO: optimize! quite a bottleneck on pb47.p with NoSelection *)
let maxlits_l ~ord lits =
  let _span = ZProf.enter_prof prof_maxlits in
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
            |> Iter.map fst
            |> Iter.to_list
  in
  ZProf.exit_prof _span;
  max

let maxlits ~ord lits =
  let _span = ZProf.enter_prof prof_maxlits in
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
            |> Iter.map (fun (x,_) -> snd x)
            |> Iter.to_list
            |> BV.of_list
  in
  ZProf.exit_prof _span;
  max

let is_max ~ord lits =
  (*
  let max = maxlits_l ~ord lits in
  fun i -> List.exists (fun (_,j) -> i=j) max
  *)
  let m = _to_multiset_with_idx lits in
  fun i ->
    let lit = lits.(i) in
    MLI.is_max (_compare_lit_with_idx ~ord) (lit,i) m

let is_trivial lits =
  (* check if a pair of lits is trivial *)
  let rec check_multi lits i =
    if i = Array.length lits then false
    else
      let lit = lits.(i) in
      let triv = match lit with
        | Lit.Equation (l, r, true) when T.equal l r -> true
        | Lit.Equation (l, r, sign) ->
          if Lit.is_predicate_lit lit then (
            let sign = Lit.is_positivoid lit in
            CCArray.exists
              (function
                | Lit.Equation (l', _, _) as lit' when Lit.is_predicate_lit lit' ->
                  let sign' = Lit.is_positivoid lit' in
                  sign != sign' && T.equal l l'
                | _ -> false)
              lits
          )
          else (
            CCArray.exists
              (function
                | Lit.Equation (l', r', sign') when sign = not sign' ->
                  (T.equal l l' && T.equal r r') || (T.equal l r' && T.equal l' r)
                | _ -> false)
              lits)
        | lit -> Lit.is_trivial lit
      in
      triv || check_multi lits (i+1)
  in
  CCArray.exists Lit.is_trivial lits || check_multi lits 0

let is_absurd lits =
  CCArray.for_all Lit.is_absurd lits

let apply_subst renaming subst (lits,sc) =
  CCArray.map (fun l -> Lit.apply_subst renaming subst (l,sc)) lits

module Seq = struct
  let vars lits =
    Iter.of_array lits |> Iter.flat_map Lit.Seq.vars
  let terms a =
    Iter.of_array a |> Iter.flat_map Lit.Seq.terms
  let to_form a = Iter.of_array a |> Iter.map Lit.Conv.to_form
end

(** {3 High Order combinators} *)

module Pos = struct
  let _fail_lits lits pos =
    let msg =
      CCFormat.sprintf "@[invalid position @[%a@]@ in lits [@[%a@]]@]"
        Position.pp pos (CCFormat.array Lit.pp) lits
    in invalid_arg msg

  let _fail_pos pos =
    let msg =
      CCFormat.sprintf
        "@[<2>invalid literal-array position@ @[%a@]@]" Position.pp pos in
    invalid_arg msg

  let at lits pos = match pos with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      Lit.Pos.at lits.(idx) pos'
    | _ -> _fail_lits lits pos

  let lit_at lits pos = match pos with
    | Position.Arg (i, pos') when i >= 0 && i < Array.length lits ->
      lits.(i), pos'
    | _ -> _fail_lits lits pos

  let replace lits ~at ~by = match at with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      lits.(idx) <- Lit.Pos.replace lits.(idx) ~at:pos' ~by
    | _ -> _fail_lits lits at

  let idx = function
    | Position.Arg(i, _) -> i
    | p -> _fail_pos p

  let tail = function
    | Position.Arg (_, pos') -> pos'
    | p -> _fail_pos p

  let cut = function
    | Position.Arg (i, pos') -> i, pos'
    | p -> _fail_pos p

  (* Does this position target one side of an equation? *)
  let is_toplevel_pos = function
    | Position.Arg (_, ((Position.Left p) | (Position.Right p))) ->
      Position.equal p Position.stop
    | _ -> false
end

module Conv = struct
  let of_forms ?hooks forms =
    let forms = Array.of_list forms in
    Array.map (Lit.Conv.of_form ?hooks) forms

  let to_forms ?hooks lits =
    Array.to_list (Array.map (Lit.Conv.to_form ?hooks) lits)

  let to_tst lits = 
    let ctx = Type.Conv.create () in
    let var_seq = Seq.vars lits in
    Type.Conv.set_maxvar ctx (T.Seq.max_var var_seq);
    Array.map (fun t -> Lit.Conv.lit_to_tst ~ctx (Lit.Conv.to_form t)) lits 
    |> Array.to_list
    |> (fun or_args ->
        let ty = TypedSTerm.Ty.prop in
        let clause_vars = T.VarSet.of_iter (var_seq) in
        let vars = clause_vars
                   |> T.VarSet.to_list 
                   |> CCList.map (fun v -> T.Conv.to_simple_term ctx (T.var v))  in
        let disjuncts =
          match or_args with
          | [] -> assert false
          | [disj] -> disj
          | _ -> TypedSTerm.app_builtin ~ty Builtin.or_ or_args in
        TypedSTerm.close_with_vars vars disjuncts
      )

  let to_s_form ?allow_free_db ?(ctx) ?hooks lits =
    let ctx =
      if CCOpt.is_some ctx then CCOpt.get_exn ctx
      else (
        let res = Type.Conv.create () in
        Type.Conv.set_maxvar res (Seq.vars lits |> T.Seq.max_var);
        res) in
    Array.to_list lits
    |> List.map (Literal.Conv.to_s_form ?hooks ?allow_free_db ~ctx)
    |> TypedSTerm.Form.or_
end

module View = struct
  let get_eqn lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.get_eqn lits.(idx) pos'
    | _ -> None

  let _unwrap2 ~msg f x y = match f x y with
    | Some z -> z
    | None -> invalid_arg msg

  let get_eqn_exn =
    _unwrap2 ~msg:"get_eqn: improper position" get_eqn
end

let fold_lits ~eligible lits k =
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i)) then aux (i+1)
    else (
      k (lits.(i), i);
      aux (i+1)
    )
  in
  aux 0

let fold_eqn ?(both=true) ?sign ~ord ~eligible lits k =
  let sign_ok s = match sign with
    | None -> true
    | Some sign -> sign = s
  in
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i)) then aux (i+1)
    else (
      let sign = Lit.is_positivoid lits.(i) in
      begin match lits.(i) with
        | Lit.Equation (l,r,_) when sign_ok sign ->
          begin match Ordering.compare ord l r with
            | Comparison.Gt | Geq ->
              k (l, r, sign, Position.(arg i @@ left @@ stop))
            | Lt | Leq ->
              k (r, l, sign, Position.(arg i @@ right @@ stop))
            | Eq
            | Incomparable ->
              if both
              then (
                (* visit both sides of the equation *)
                k (r, l, sign, Position.(arg i @@ right @@ stop));
                k (l, r, sign, Position.(arg i @@ left @@ stop))
              ) else
                (* only one side *)
                k (l, r, sign, Position.(arg i @@ left @@ stop))
          end
        | Lit.Equation _
        | Lit.True
        | Lit.False -> ()
      end;
      aux (i+1)
    )
  in
  aux 0

let fold_eqn_simple ?sign lits k =
  let sign_ok s = match sign with
    | None -> true
    | Some sign -> sign = s
  in
  let rec aux i =
    if i = Array.length lits then ()
    else (
      (* IMPORTANT: Returning the computed sign (positivoid vs negativoid)
         rather than the sign stored in the equation *)
      let sign = Lit.is_positivoid lits.(i) in
      begin match lits.(i) with
        | Lit.Equation (l,r,_) when sign_ok sign ->
          k (l, r, sign, Position.(arg i @@ left @@ stop))
        | Lit.Equation _
        | Lit.True
        | Lit.False -> ()
      end;
      aux (i+1)
    )
  in
  aux 0

let fold_terms ?(vars=false) ?(var_args=true) ?(fun_bodies=true) ?ty_args ~(which : [< `All|`Max])
    ~ord ~subterms ~eligible lits k =
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i))
    then aux (i+1) (* ignore lit *)
    else (
      Util.debugf 50 "folding literal %a, %B" (fun k -> k Lit.pp lits.(i) fun_bodies);
      Lit.fold_terms
        ~position:Position.(arg i stop) ?ty_args ~vars ~var_args ~fun_bodies ~which ~ord ~subterms
        lits.(i) k;
      aux (i+1)
    )
  in
  aux 0

let symbols ?(init=ID.Set.empty) ?(include_types=false) lits =
  Iter.of_array lits
  |> Iter.flat_map (Lit.Seq.symbols ~include_types)
  |> ID.Set.add_iter init

(** {3 IO} *)

let pp_gen ~false_ ~l ~r ~sep ~pp_lit out lits = match lits with
  | [||] -> CCFormat.string out false_
  | [| l |] -> pp_lit out l
  | _ ->
    Format.fprintf out "%s@[<hv>%a@]%s" l CCFormat.(array ~sep pp_lit) lits r

let pp out lits =
  let pp_lit = CCFormat.hovbox Lit.pp in
  pp_gen ~l:"[" ~r:"]" ~false_:"⊥" ~sep:(CCFormat.return "@ ∨ ") ~pp_lit out lits

let pp_vars_gen ~pp_var ~pp_lits out lits =
  let pp_vars out = function
    | [] -> ()
    | l -> Format.fprintf out "forall @[%a@].@ " (Util.pp_list ~sep:" " pp_var) l
  in
  let vars_ = Seq.vars lits |> T.VarSet.of_iter |> T.VarSet.to_list in
  Format.fprintf out "@[<2>%a%a@]" pp_vars vars_ pp_lits lits

let pp_vars out lits = pp_vars_gen ~pp_var:Type.pp_typed_var ~pp_lits:pp out lits

let pp_tstp out lits =
  let pp_lit = CCFormat.hovbox Lit.pp_tstp in
  pp_gen ~l:"(" ~r:")" ~false_:"$false" ~sep:(CCFormat.return "@ | ") ~pp_lit out lits

(* print quantified literals *)
let pp_tstp_closed out lits =
  let pp_vars out = function
    | [] -> ()
    | l ->
      Format.fprintf out "![@[%a@]]:@ "
        (Util.pp_list ~sep:", " Type.TPTP.pp_typed_var) l
  in
  Format.fprintf out "@[<2>%a%a@]" pp_vars (vars lits) pp_tstp lits

let pp_zf out lits =
  let pp_lit = CCFormat.hovbox Lit.pp_zf in
  pp_gen ~l:"(" ~r:")" ~false_:"false" ~sep:(CCFormat.return "@ || ") ~pp_lit out lits

let pp_zf_closed out lits =
  pp_vars_gen ~pp_var:Type.ZF.pp_typed_var ~pp_lits:pp_zf out lits

let to_string a = CCFormat.to_string pp a

(** {2 Special kinds of array} *)

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause lits =
  let bv = pos lits in
  match BV.to_list bv with
  | [i] ->
    (* single positive lit, check variables restrictions, ie all vars
        occur in the head *)
    let hd_vars = Lit.vars lits.(i) in
    List.length hd_vars = List.length (vars lits)
  | _ -> false

(** Is clause Horn and has a unique maximal literal  *)
let is_unique_max_horn_clause ~ord lits =
  BV.cardinal (pos lits) = 1 &&
  BV.cardinal (maxlits ~ord lits) = 1

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn lits =
  let bv = pos lits in
  BV.cardinal bv <= 1


(** {2 Shielded Variables} *)

let is_shielded var (lits:t) : bool =
  let var_eq = HVar.equal Type.equal in
  let rec shielded_by_term ~root t = match T.view t with
    | T.Var v' when var_eq v' var -> not root
    | _ when Type.Seq.vars (T.ty t) |> Iter.exists (var_eq var) ->
      true (* shielded by type *)
    | T.Var _
    | T.DB _
    | T.Const _ -> false
    | T.AppBuiltin (_, l) ->
      List.exists (shielded_by_term ~root:false) l
    | T.App (f, l) ->
      shielded_by_term ~root f ||
      List.exists (shielded_by_term ~root:false) l
    | T.Fun (_, bod) -> shielded_by_term ~root:false bod
  in
  (* is there a term, directly under a literal, that shields the variable? *)
  begin
    lits
    |> Seq.terms
    |> Iter.exists (shielded_by_term ~root:true)
  end

let unshielded_vars ?(filter=fun _->true) lits: _ list =
  vars lits
  |> List.filter
    (fun var ->
       filter var &&
       not (is_shielded var lits))

let vars_distinct lits = 
  let dif_vars = vars lits in
  let dif_ids  = 
    List.sort_uniq CCOrd.int @@ List.map HVar.id dif_vars in
  List.length dif_ids = List.length dif_vars

let ground_lits lits = 
  let counter = ref 0 in
  let all_vars = T.VarSet.of_iter @@ Seq.vars lits in
  let gr_subst = T.VarSet.fold (fun v subst -> 
      let ty = HVar.ty v in
      Subst.FO.bind subst ((v :> InnerTerm.t HVar.t),0) (T.mk_tmp_cst ~counter ~ty,0)
    ) all_vars Subst.empty in
  let res = apply_subst Subst.Renaming.none gr_subst (lits,0)  in
  assert (Iter.for_all T.is_ground @@ Seq.terms res);
  res

let num_predicate lits = 
  let cnt = ref 0 in
  CCArray.iter (fun l -> if Lit.is_predicate_lit l then incr cnt) lits;
  !cnt

let num_equational lits = 
  CCArray.length lits - num_predicate lits

let is_polymorphism_safe idx lits =
  let all = Iter.filter (fun var -> Type.is_tType (HVar.ty var)) (Seq.vars lits) in
  let at_idx =
    Iter.filter (fun var -> Type.is_tType (HVar.ty var)) (Literal.Seq.vars lits.(idx)) in
  Iter.for_all (fun ty_var -> Iter.exists (HVar.equal Type.equal ty_var) at_idx) all
