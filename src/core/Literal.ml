(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Equational literals} *)

module T = Term
module S = Subst
module PB = Position.Build
module P = Position
module US = Unif_subst
module VS = T.VarSet

type term = Term.t
type t = True | False | Equation of term * term * bool
type lit = t

let equal l1 l2 =
   match (l1, l2) with
      | Equation (l1, r1, sign1), Equation (l2, r2, sign2) -> sign1 = sign2 && l1 == l2 && r1 == r2
      | True, True | False, False -> true
      | Equation _, _ | True, _ | False, _ -> false

let equal_com l1 l2 =
   match (l1, l2) with
      | Equation (l1, r1, sign1), Equation (l2, r2, sign2) ->
         sign1 = sign2 && ((T.equal l1 l2 && T.equal r1 r2) || (T.equal l1 r2 && T.equal r1 l2))
      | True, True | False, False -> true
      | _ -> equal l1 l2 (* regular comparison *)

let no_prop_invariant = function
   | Equation (lhs, rhs, sign) ->
      if T.is_true_or_false rhs && not sign then (
        CCFormat.printf "literal wrongly encoded: @[%a@] @[%a@] @[%b@]@." T.pp lhs T.pp rhs sign;
        false)
      else true
   | _ -> true

let compare l1 l2 =
   (* assert(List.for_all no_prop_invariant [l1;l2]); *)
   let __to_int = function False -> 0 | True -> 1 | Equation _ -> 2 in
      match (l1, l2) with
         | Equation (l1, r1, sign1), Equation (l2, r2, sign2) ->
            let c = T.compare l1 l2 in
               if c <> 0 then c
               else
                 let c = T.compare r1 r2 in
                    if c <> 0 then c else CCShims_.Stdlib.compare sign1 sign2
         | True, True | False, False -> 0
         | _, _ -> __to_int l1 - __to_int l2

let fold f acc lit = match lit with Equation (l, r, _) -> f (f acc l) r | True | False -> acc
let for_all f lit = fold (fun b t -> b && f t) true lit

let hash lit =
   match lit with
      | Equation (l, r, sign) -> Hash.combine4 30 (Hash.bool sign) (T.hash l) (T.hash r)
      | True -> 40
      | False -> 50

let[@inline] is_predicate_lit = function Equation (_, rhs, true) -> T.is_true_or_false rhs | _ -> false
let weight lit = fold (fun acc t -> acc + T.size t) 0 lit
let ho_weight = fold (fun acc t -> acc + T.ho_weight t) 0

let heuristic_weight weight = function
   | Equation (l, _, _) as lit when is_predicate_lit lit -> weight l
   | Equation (l, r, _) -> weight l + weight r
   | True | False -> 0

let depth lit = fold (fun acc t -> max acc (T.depth t)) 0 lit

module Set = CCSet.Make (struct
  type t = lit

  let compare = compare
end)

let is_positivoid = function
   | Equation (l, r, sign) -> sign && (not @@ T.equal r T.false_)
   | False -> false
   | _ -> true

let eqn_sign = function Equation (_, _, sign) -> sign | False -> false | _ -> true

(* specific: for the term comparison *)
let polarity = function Equation (_, _, s) -> s | l -> is_positivoid l
let is_negativoid lit = not (is_positivoid lit)
let is_eqn = function Equation _ -> true | _ -> false
let is_eq lit = is_eqn lit && is_positivoid lit
let is_neq lit = is_eqn lit && is_negativoid lit
let is_app_var_eq = function Equation (l, r, _) -> T.is_app_var l && T.is_app_var r | _ -> false
let is_prop = function True | False -> true | _ -> false

let is_type_pred = function
   | Equation (lhs, _, _) as l when is_predicate_lit l -> (
      match Term.view lhs with App (f, [ x ]) -> T.is_var x && T.is_const f | _ -> false)
   | _ -> false

let is_typex_pred = function
   | Equation (lhs, rhs, _) as l when is_predicate_lit l -> (
      match Term.view lhs with
         | App (f, xs) when not (CCList.is_empty xs) -> T.is_const f && List.for_all T.is_var xs
         | _ -> false)
   | _ -> false

let ty_error_ a b =
   let msg =
      CCFormat.sprintf
        "@[<2>Literal: incompatible types in equational lit@ for `@[%a : %a@]`@ and `@[%a : %a@]`@]" T.TPTP.pp a
        Type.pp (T.ty a) T.TPTP.pp b Type.pp (T.ty b)
   in
      raise (Type.ApplyError msg)

(* primary constructor for equations and predicates *)
let rec mk_lit a b sign =
   if not (Type.equal (T.ty a) (T.ty b)) then ty_error_ a b;
   (* Maybe the sign will flip, so we have to beta reduce. *)
   match (T.view a, T.view b) with
      | T.AppBuiltin (Builtin.True, []), T.AppBuiltin (Builtin.False, []) -> if sign then False else True
      | T.AppBuiltin (Builtin.False, []), T.AppBuiltin (Builtin.True, []) -> if sign then False else True
      | T.AppBuiltin (Builtin.True, []), T.AppBuiltin (Builtin.True, []) -> if sign then True else False
      | T.AppBuiltin (Builtin.False, []), T.AppBuiltin (Builtin.False, []) -> if sign then True else False
      | T.AppBuiltin (Builtin.True, []), _ ->
         let lhs, rhs = (b, if sign then T.true_ else T.false_) in
            Equation (lhs, rhs, true)
      | _, T.AppBuiltin (Builtin.True, []) ->
         let lhs, rhs = (a, if sign then T.true_ else T.false_) in
            Equation (lhs, rhs, true)
      | T.AppBuiltin (Builtin.False, []), _ ->
         let lhs, rhs = (b, if sign then T.false_ else T.true_) in
            Equation (lhs, rhs, true)
      | _, T.AppBuiltin (Builtin.False, []) ->
         let lhs, rhs = (a, if sign then T.false_ else T.true_) in
            Equation (lhs, rhs, true)
      | _ -> Equation (a, b, sign)

and mk_prop p sign =
   match T.view p with
      | T.AppBuiltin (Builtin.True, []) -> if sign then True else False
      | T.AppBuiltin (Builtin.False, []) -> if sign then False else True
      | T.AppBuiltin (Builtin.Not, [ p' ]) -> mk_prop p' (not sign)
      | T.AppBuiltin (Builtin.Eq, [ a; b ]) -> mk_lit a b sign
      | T.AppBuiltin (Builtin.Neq, [ a; b ]) -> mk_lit a b (not sign)
      | _ ->
         if not (Type.equal (T.ty p) Type.prop) then ty_error_ p T.true_;
         mk_lit p T.true_ sign

let mk_eq a b = mk_lit a b true
let mk_neq a b = mk_lit a b false
let mk_true p = mk_prop p true
let mk_false p = mk_prop p false
let mk_tauto = True
let mk_absurd = False
let mk_constraint l r = mk_neq l r

module Seq = struct
  let terms lit k =
     match lit with
        | Equation (l, r, _) ->
           k l;
           k r
        | True | False -> ()

  let vars lit = Iter.flat_map T.Seq.vars (terms lit)
  let symbols ?(include_types = false) lit = Iter.flat_map (T.Seq.symbols ~include_types) (terms lit)
  let typed_symbols ?(include_types = false) lit = Iter.flat_map (T.Seq.typed_symbols ~include_types) (terms lit)
end

let symbols ?(include_types = false) lit = Seq.symbols ~include_types lit |> ID.Set.of_iter

(** Unification-like operation on components of a literal. *)
module UnifOp = struct
  type 'subst op = { term : subst:'subst -> term Scoped.t -> term Scoped.t -> 'subst Iter.t }
end

(* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
let unif4 op ~subst x1 y1 sc1 x2 y2 sc2 k =
   op ~subst (Scoped.make x1 sc1) (Scoped.make x2 sc2) (fun subst ->
       op ~subst (Scoped.make y1 sc1) (Scoped.make y2 sc2) k);
   op ~subst (Scoped.make y1 sc1) (Scoped.make x2 sc2) (fun subst ->
       op ~subst (Scoped.make x1 sc1) (Scoped.make y2 sc2) k);
   ()

(* generic unification structure *)
let unif_lits op ~subst (lit1, sc1) (lit2, sc2) k =
   let open UnifOp in
   match (lit1, lit2) with
      | True, True | False, False -> k (subst, [])
      | Equation (l1, r1, sign1), Equation (l2, r2, sign2) when sign1 = sign2 ->
         unif4 op.term ~subst l1 r1 sc1 l2 r2 sc2 (fun s -> k (s, []))
      | _, _ -> ()

let variant ?(subst = S.empty) lit1 lit2 k =
   let op =
      UnifOp.{ term = (fun ~subst t1 t2 k -> try k (Unif.FO.variant ~subst t1 t2) with Unif.Fail -> ()) }
   in
      unif_lits op ~subst lit1 lit2 (fun (subst, tags) -> if Subst.is_renaming subst then k (subst, tags))

let are_variant lit1 lit2 = not (Iter.is_empty (variant (Scoped.make lit1 0) (Scoped.make lit2 1)))

let matching ?(subst = Subst.empty) ~pattern:lit1 lit2 k =
   let op =
      UnifOp.
        {
          term =
            (fun ~subst t1 t2 k ->
               try k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1 t2) with Unif.Fail -> ());
        }
   in
      unif_lits op ~subst lit1 lit2 k

(* find substitutions such that subst(l1=r1) implies l2=r2 *)
let _eq_subsumes ~subst l1 r1 sc1 l2 r2 sc2 k =
   (* make l2 and r2 equal using l1 = r2 (possibly several times) *)
   let rec equate_terms ~subst l2 r2 k =
      (* try to make the terms themselves equal *)
      equate_root ~subst l2 r2 k;
      (* decompose *)
      match (T.view l2, T.view r2) with
         | _ when T.equal l2 r2 -> k subst
         | T.App (f, ss), T.App (g, ts) when List.length ss = List.length ts ->
            (* Don't rewrite heads because it can cause incompletness, e.g. by
               subsuming ho_complete_eq inferences. *)
            if T.equal f g then equate_lists ~subst ss ts k else ()
         | _ -> ()
   and equate_lists ~subst l2s r2s k =
      match (l2s, r2s) with
         | [], [] -> k subst
         | [], _ | _, [] -> ()
         | l2 :: l2s', r2 :: r2s' -> equate_terms ~subst l2 r2 (fun subst -> equate_lists ~subst l2s' r2s' k)
   (* make l2=r2 by a direct application of l1=r1, if possible. This can
       enrich [subst] *)
   and equate_root ~subst l2 r2 k =
      (try
         let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:(Scoped.make l1 sc1) (Scoped.make l2 sc2) in
         let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:(Scoped.make r1 sc1) (Scoped.make r2 sc2) in

         (* CCFormat.printf "%a = %a;\n%a = %a;\n%a.\n" T.pp l1 T.pp l2 T.pp r1 T.pp r2 Subst.pp subst; *)
         k subst
       with Unif.Fail ->
         (* CCFormat.printf "FAILED: %a = %a;\n%a = %a;\n%a.\n" T.pp l1 T.pp l2 T.pp r1 T.pp r2 Subst.pp subst; *)
         ());
      (try
         let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:(Scoped.make l1 sc1) (Scoped.make r2 sc2) in
         let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:(Scoped.make r1 sc1) (Scoped.make l2 sc2) in
            k subst
       with Unif.Fail ->
         (* CCFormat.printf "FAILED: %a = %a;\n%a = %a;\n%a.\n" T.pp l1 T.pp l2 T.pp r1 T.pp r2 Subst.pp subst; *)
         ());
      ()
   in
      equate_terms ~subst l2 r2 k

let subsumes ?(subst = Subst.empty) (lit1, sc1) (lit2, sc2) k =
   match (lit1, lit2) with
      | Equation (l1, r1, true), Equation (l2, r2, true) ->
         _eq_subsumes ~subst l1 r1 sc1 l2 r2 sc2 (fun s -> k (s, []))
      | _ -> matching ~subst ~pattern:(lit1, sc1) (lit2, sc2) k

let unify ?(subst = US.empty) lit1 lit2 k =
   let op =
      UnifOp.{ term = (fun ~subst t1 t2 k -> try k (Unif.FO.unify_full ~subst t1 t2) with Unif.Fail -> ()) }
   in
      unif_lits op ~subst lit1 lit2 k

let map_ f = function
   | Equation (left, right, sign) ->
      let new_left = f left and new_right = f right in
         mk_lit new_left new_right sign
   | True -> True
   | False -> False

let map f lit = map_ f lit

let apply_subst_ ~f_term subst (lit, sc) =
   match lit with
      | Equation (l, r, sign) ->
         let new_l = f_term subst (l, sc) and new_r = f_term subst (r, sc) in
            mk_lit new_l new_r sign
      | True | False -> lit

let apply_subst renaming subst (lit, sc) = apply_subst_ subst (lit, sc) ~f_term:(S.FO.apply renaming)

let apply_subst_no_simp renaming subst (lit, sc) =
   match lit with
      | Equation (l, r, sign) ->
         Equation (S.FO.apply renaming subst (l, sc), S.FO.apply renaming subst (r, sc), sign)
      | True | False -> lit

let apply_subst_list renaming subst (lits, sc) = List.map (fun lit -> apply_subst renaming subst (lit, sc)) lits

exception Lit_is_constraint

let is_ho_constraint = function
   | Equation (l, r, _) as lit when is_negativoid lit -> T.is_ho_at_root l || T.is_ho_at_root r
   | _ -> false

let is_constraint = function
   | Equation (t, u, _) as lit when is_negativoid lit -> T.is_var t || T.is_var u
   | _ -> false

let negate lit =
   assert (no_prop_invariant lit);
   match lit with Equation (l, r, sign) -> mk_lit l r (not sign) | True -> False | False -> True

let vars lit = Seq.vars lit |> T.VarSet.of_iter |> T.VarSet.to_list

let var_occurs v lit =
   match lit with Equation (l, r, _) -> T.var_occurs ~var:v l || T.var_occurs ~var:v r | True | False -> false

let is_ground lit = match lit with Equation (l, r, _) -> T.is_ground l && T.is_ground r | True | False -> true
let root_terms l = Seq.terms l |> Iter.to_rev_list

let to_multiset lit =
   match lit with
      | Equation (l, r, _) when is_predicate_lit lit -> Multisets.MT.singleton l
      | Equation (l, r, _) -> Multisets.MT.doubleton l r
      | True | False -> Multisets.MT.singleton T.true_

let is_trivial lit =
   assert (no_prop_invariant lit);
   match lit with
      | True -> true
      | False -> false
      | Equation (l, r, true) -> T.equal l r
      | Equation (_, _, false) -> false

(* is it impossible for these terms to be equal? check if a cstor-only
     path leads to distinct constructors/constants *)
let rec cannot_be_eq (t1 : term) (t2 : term) : Builtin.Tag.t list option =
   let module TC = T.Classic in
   match (TC.view t1, TC.view t2) with
      | TC.AppBuiltin (Builtin.Int z1, []), TC.AppBuiltin (Builtin.Int z2, []) ->
         if Z.equal z1 z2 then None else Some [ Builtin.Tag.T_lia; Builtin.Tag.T_cannot_orphan ]
      | TC.AppBuiltin (Builtin.Rat n1, []), TC.AppBuiltin (Builtin.Rat n2, []) ->
         if Q.equal n1 n2 then None else Some [ Builtin.Tag.T_lra; Builtin.Tag.T_cannot_orphan ]
      | TC.App (c1, l1), TC.App (c2, l2) when Ind_ty.is_constructor c1 && Ind_ty.is_constructor c2 ->
         (* two constructor applications cannot be equal if they
            don't have the same constructor *)
         if ID.equal c1 c2 && List.length l1 = List.length l2 then
           List.combine l1 l2 |> Iter.of_list |> Iter.find_map (fun (a, b) -> cannot_be_eq a b)
         else Some [ Builtin.Tag.T_data ]
      | _ -> None

let is_absurd lit =
   assert (no_prop_invariant lit);
   match lit with
      | Equation (l, r, false) when T.equal l r -> true
      | Equation (l, r, true) -> CCOpt.is_some (cannot_be_eq l r)
      | False -> true
      | Equation _ | True -> false

let is_absurd_tags lit =
   assert (no_prop_invariant lit);
   match lit with
      | Equation (l, r, true) -> cannot_be_eq l r |> CCOpt.get_or ~default:[]
      | Equation _ | False -> []
      | True -> assert false

let fold_terms ?(position = Position.stop) ?(vars = false) ?(var_args = true) ?(fun_bodies = true) ?ty_args
    ~which ?(ord = Ordering.none) ~subterms lit k =
   assert (no_prop_invariant lit);

   (* function to call at terms *)
   let filter_formula_subterms hd args =
      let open Builtin in
      match which with
         | `Max -> (
            match (hd, args) with
               | (Eq | Neq | Xor | Equiv), ([ _; a; b ] | [ a; b ]) -> (
                  match Ordering.compare ord a b with
                     | Comparison.Lt | Leq -> Some [ List.length args - 1 ]
                     | Gt | Geq -> Some [ List.length args - 2 ]
                     | _ -> None)
               | (ForallConst | ExistsConst), [ _; _ ] -> Some []
               | _ -> None)
         | `All -> None
   in

   let at_term ~pos t =
      if subterms then T.all_positions ~filter_formula_subterms ?ty_args ~vars ~var_args ~fun_bodies ~pos t k
      else if T.is_var t && not vars then () (* ignore *)
      else k (t, pos)
   in
      match lit with
         | Equation (l, r, _) -> (
            match which with
               | `All ->
                  at_term ~pos:P.(append position (left stop)) l;
                  at_term ~pos:P.(append position (right stop)) r
               | `Max -> (
                  match Ordering.compare ord l r with
                     | Comparison.Gt | Geq -> at_term ~pos:P.(append position (left stop)) l
                     | Lt | Leq -> at_term ~pos:P.(append position (right stop)) r
                     | Eq | Incomparable ->
                        (* visit both sides, they are both (potentially) maximal *)
                        at_term ~pos:P.(append position (left stop)) l;
                        at_term ~pos:P.(append position (right stop)) r))
         | True | False -> ()

(* try to convert a literal into a term *)
let to_ho_term (lit : t) : T.t =
   match lit with
      | True -> T.true_
      | False -> T.false_
      | Equation (t, u, _) when is_predicate_lit lit -> (if is_negativoid lit then T.Form.not_ else CCFun.id) t
      | Equation (t, u, sign) -> if sign then T.Form.eq t u else T.Form.neq t u

let as_ho_predicate (lit : t) : _ option =
   assert (no_prop_invariant lit);
   match lit with
      | Equation (lhs, rhs, _) when is_predicate_lit lit -> (
         let hd_t, args_t = T.as_app lhs in
            match (T.view hd_t, args_t) with
               | T.Var v, _ :: _ -> Some (v, hd_t, args_t, is_positivoid lit)
               | _ -> None)
      | _ -> None

let is_ho_predicate lit = CCOpt.is_some (as_ho_predicate lit)

let is_ho_unif lit =
   match lit with
      | Equation (t, u, _) when is_negativoid lit -> Term.is_ho_app t || Term.is_ho_app u
      | _ -> false

let of_unif_subst renaming (s : Unif_subst.t) : t list =
   Unif_subst.constr_l_subst renaming s
   |> List.map (fun (t, u) ->
          (* upcast *)
          let t = T.of_term_unsafe t in
          let u = T.of_term_unsafe u in
             mk_constraint t u)

let normalize_eq lit =
   let as_neg t = match T.view t with T.AppBuiltin (Not, [ f ]) -> Some f | _ -> None in

   let is_negativoid t = CCOpt.is_some @@ as_neg t in

   let eq_builder ~pos ~neg l r =
      match (as_neg l, as_neg r) with
         | Some f1, Some f2 -> pos f1 f2
         | Some f1, None -> neg f1 r
         | None, Some f2 -> neg l f2
         | None, None -> pos l r
   in

   let mk_eq_ l r = eq_builder ~pos:mk_eq ~neg:mk_neq l r in
   let mk_neq_ l r = eq_builder ~pos:mk_neq ~neg:mk_eq l r in

   let rec aux lit =
      match lit with
         | Equation (lhs, rhs, _) when is_predicate_lit lit -> (
            let sign = is_positivoid lit in
               match T.view lhs with
                  | T.AppBuiltin (Builtin.(Eq | Equiv), ([ _; l; r ] | [ l; r ])) ->
                     (* first arg can be type variable *)
                     let eq_cons = if sign then mk_eq_ else mk_neq_ in
                        Some (eq_cons l r)
                  | T.AppBuiltin (Builtin.(Neq | Xor), ([ _; l; r ] | [ l; r ])) ->
                     let eq_cons = if sign then mk_neq_ else mk_eq_ in
                        Some (eq_cons l r)
                  | T.AppBuiltin (Builtin.Not, [ f ]) ->
                     let elim_not = mk_lit f T.true_ (not sign) in
                        Some (CCOpt.get_or ~default:elim_not (aux elim_not))
                  | _ -> None)
         | Equation (lhs, rhs, sign) when is_negativoid lhs || is_negativoid rhs ->
            assert (not (T.is_true_or_false rhs));
            Some ((if sign then mk_eq_ else mk_neq_) lhs rhs)
         | _ -> None
   in
      aux lit

(** {2 IO} *)

let pp_debug ?(hooks = []) out lit =
   (* assert(no_prop_invariant lit); *)
   if List.for_all (fun h -> not (h out lit)) hooks then
     match lit with
        | Equation (p, t, _) when is_predicate_lit lit ->
           Format.fprintf out "@[%s%a@]" (if is_positivoid lit then "" else "¬") T.pp p
        | True -> CCFormat.string out "Τ"
        | False -> CCFormat.string out "⊥"
        | Equation (l, r, true) -> Format.fprintf out "@[<1>%a@ = %a@]" T.pp l T.pp r
        | Equation (l, r, false) -> Format.fprintf out "@[<1>%a@ ≠ %a@]" T.pp l T.pp r

let pp_tstp out lit =
   match lit with
      | Equation (p, t, _) when is_predicate_lit lit ->
         Format.fprintf out "%s %a" (if is_positivoid lit then "" else "~") T.TPTP.pp p
      | True -> CCFormat.string out "$true"
      | False -> CCFormat.string out "$false"
      | Equation (l, r, true) -> Format.fprintf out "(@[<1>%a@ = %a@])" T.TPTP.pp l T.TPTP.pp r
      | Equation (l, r, false) -> Format.fprintf out "(@[<1>%a@ != %a@])" T.TPTP.pp l T.TPTP.pp r

let pp_zf out lit =
   match lit with
      | Equation (p, t, _) when is_predicate_lit lit ->
         Format.fprintf out "%s %a" (if is_positivoid lit then "" else "~") T.ZF.pp p
      | True -> CCFormat.string out "true"
      | False -> CCFormat.string out "false"
      | Equation (l, r, true) -> Format.fprintf out "@[<1>%a@ = %a@]" T.ZF.pp l T.ZF.pp r
      | Equation (l, r, false) -> Format.fprintf out "@[<1>%a@ != %a@]" T.ZF.pp l T.ZF.pp r

type print_hook = CCFormat.t -> t -> bool

let __hooks = ref []
let add_default_hook h = __hooks := h :: !__hooks
let pp buf lit = pp_debug ~hooks:!__hooks buf lit
let to_string t = CCFormat.to_string pp t

(* comparison should live in its scope *)
module Comp = struct
  module O = Ordering
  module C = Comparison

  let _maxterms2 ~ord l r =
     match O.compare ord l r with
        | C.Gt | Geq -> [ l ]
        | Lt | Leq -> [ r ]
        | Eq -> [ l ]
        | Incomparable -> [ l; r ]

  (* maximal terms of the literal *)
  let max_terms ~ord lit =
     (* assert(no_prop_invariant lit); *)
     match lit with Equation (l, r, _) -> _maxterms2 ~ord l r | True | False -> []

  (* general comparison is a bit complicated.
     - First we compare literals l1 and l2
        by their (set of potential) maximal terms
     - then by their polarity (neg > pos)
     - then by their kind (regular equation/prop on bottom)
     - then, l1 and l2 must be of the same kind, so we use a
        kind-specific comparison.
  *)

  type dominator_result = NoDominator of bool | NonstrictDominator | StrictDominator

  (* Is there an element of the first list that dominates all elements of the
     second list? *)
  let rec _find_dominators ~met_geq_or_leq f ts1 ts2 =
     match ts1 with
        | [] -> NoDominator met_geq_or_leq
        | t :: ts1 ->
           let cmps = List.map (fun y -> f t y) ts2 in
              if List.for_all (fun cmp -> cmp = C.Gt) cmps then StrictDominator
              else if List.for_all C.is_Gt_or_Geq cmps then
                match _find_dominators ~met_geq_or_leq f ts1 ts2 with
                   | StrictDominator -> StrictDominator
                   | _ -> NonstrictDominator
              else
                let met_geq_or_leq' =
                   met_geq_or_leq || List.exists (fun cmp -> cmp = C.Geq || cmp = C.Leq) cmps
                in
                   _find_dominators ~met_geq_or_leq:met_geq_or_leq' f ts1 ts2

  let _cmp_by_maxterms ~ord l1 l2 =
     let maxs1 = max_terms ~ord l1 and maxs2 = max_terms ~ord l2 in
     let f = Ordering.compare ord in
        match
          ( _find_dominators ~met_geq_or_leq:false f maxs1 maxs2,
            _find_dominators ~met_geq_or_leq:false f maxs2 maxs1 )
        with
           | NoDominator met_geq_or_leq1, NoDominator met_geq_or_leq2 ->
              if met_geq_or_leq1 || met_geq_or_leq2 then C.Eq (* check next criterion *)
              else
                let set1 = CCList.fold_right T.Set.add maxs1 T.Set.empty
                and set2 = CCList.fold_right T.Set.add maxs2 T.Set.empty in
                   if T.Set.equal set1 set2 then Eq (* check next criterion *)
                   else (* no need to continue; [_cmp_specific] will fail anyway *)
                     Incomparable
           | StrictDominator, NoDominator _ -> Gt
           | NoDominator _, StrictDominator -> Lt
           | NonstrictDominator, NoDominator _ -> Geq
           | NoDominator _, NonstrictDominator -> Leq
           | _, _ -> assert false

  (* negative literals dominate *)
  let _cmp_by_polarity l1 l2 =
     let p1 = is_positivoid l1 in
     let p2 = is_positivoid l2 in
        match (p1, p2) with true, true | false, false -> C.Eq | true, false -> Lt | false, true -> Gt

  let _cmp_by_kind l1 l2 =
     let _to_int = function False | True -> 0 | Equation _ -> 30 in
        C.of_total (CCShims_.Stdlib.compare (_to_int l1) (_to_int l2))

  (* by multiset of terms *)
  let _cmp_by_term_multiset ~ord l1 l2 =
     Multisets.MT.compare_partial (Ordering.compare ord) (to_multiset l1) (to_multiset l2)

  let _cmp_specific ~ord l1 l2 =
     match (l1, l2) with
        | True, True
        | True, False
        | True, Equation _
        | False, False
        | False, True
        | False, Equation _
        | Equation _, Equation _
        | Equation _, True
        | Equation _, False ->
           _cmp_by_term_multiset ~ord l1 l2

  let compare ~ord = C.(_cmp_by_maxterms ~ord @>> _cmp_by_polarity @>> _cmp_by_kind @>> _cmp_specific ~ord)
end

module Pos = struct
  type split = { lit_pos : Position.t; term_pos : Position.t; term : term }

  let _fail_lit lit pos =
     let msg = CCFormat.sprintf "@[<2>invalid position @[%a@]@ in lit @[%a@]@]" Position.pp pos pp lit in
        invalid_arg msg

  let split lit pos =
     match (lit, pos) with
        | True, P.Stop -> { lit_pos = P.stop; term_pos = P.stop; term = T.true_ }
        | False, P.Stop -> { lit_pos = P.stop; term_pos = P.stop; term = T.false_ }
        | Equation (l, _, _), P.Left pos' -> { lit_pos = P.(left stop); term_pos = pos'; term = l }
        | Equation (_, r, _), P.Right pos' -> { lit_pos = P.(right stop); term_pos = pos'; term = r }
        | _ -> _fail_lit lit pos

  let cut lit pos =
     let s = split lit pos in
        (s.lit_pos, s.term_pos)

  let at lit pos =
     let s = split lit pos in
        T.Pos.at s.term s.term_pos

  let replace lit ~at ~by =
     match (lit, at) with
        | Equation (l, r, sign), P.Left pos' ->
           let cons = if sign then mk_eq else mk_neq in
              cons (T.Pos.replace l pos' ~by) r
        | Equation (l, r, sign), P.Right pos' ->
           let cons = if sign then mk_eq else mk_neq in
              cons l (T.Pos.replace r pos' ~by)
        | True, _ | False, _ -> lit (* flexible, lit can be the result of a simplification *)
        | _ -> _fail_lit lit at

  let root_term lit pos = at lit (fst (cut lit pos))
  let term_pos lit pos = snd (cut lit pos)

  let is_max_term ~ord lit pos =
     match (lit, pos) with
        | Equation (l, r, _), P.Left _ -> not (Comparison.is_Lt_or_Leq (Ordering.compare ord l r))
        | Equation (l, r, _), P.Right _ -> not (Comparison.is_Lt_or_Leq (Ordering.compare ord r l))
        | True, _ | False, _ -> true (* why not. *)
        | Equation _, _ -> _fail_lit lit pos
end

let replace lit ~old ~by = map (T.replace ~old ~by) lit

module Conv = struct
  type hook_from = term SLiteral.t -> t option
  type hook_to = t -> term SLiteral.t option

  let rec try_hooks x hooks =
     match hooks with
        | [] -> None
        | h :: hooks' -> ( match h x with None -> try_hooks x hooks' | Some _ as res -> res)

  let of_form ?(hooks = []) f =
     match try_hooks f hooks with
        | Some lit -> lit
        | None -> (
           match f with
              | SLiteral.True -> True
              | SLiteral.False -> False
              | SLiteral.Atom (t, b) -> mk_prop t b
              | SLiteral.Eq (l, r) -> mk_eq l r
              | SLiteral.Neq (l, r) -> mk_neq l r)

  let to_form ?(hooks = []) lit =
     assert (no_prop_invariant lit);
     match try_hooks lit hooks with
        | Some f -> f
        | None -> (
           match lit with
              | Equation (l, r, _) ->
                 assert (Type.equal (Term.ty l) (Term.ty r));
                 let sign = is_positivoid lit in
                    if Type.is_prop (Term.ty l) then
                      if T.is_true_or_false r then SLiteral.atom l sign
                      else
                        let hd = if sign then Builtin.Equiv else Builtin.Xor in
                           SLiteral.atom (T.app_builtin ~ty:Type.prop hd [ l; r ]) true
                    else if sign then SLiteral.eq l r
                    else SLiteral.neq l r
              | True -> SLiteral.true_
              | False -> SLiteral.false_)

  let lit_to_tst ?(ctx = T.Conv.create ()) lit =
     match lit with
        | SLiteral.Atom (p, s) ->
           let p = if s then p else T.Form.not_ p in
              T.Conv.to_simple_term ctx p
        | SLiteral.Eq (l, r) when T.equal T.true_ r -> T.Conv.to_simple_term ctx l
        | SLiteral.Eq (l, r) when T.equal T.false_ r -> T.Conv.to_simple_term ctx (T.Form.not_ l)
        | SLiteral.Neq (l, r) when T.equal T.true_ r -> T.Conv.to_simple_term ctx (T.Form.not_ l)
        | SLiteral.Neq (l, r) when T.equal T.false_ r -> T.Conv.to_simple_term ctx l
        | SLiteral.Eq (l, r) ->
           let l, r = CCPair.map_same (T.Conv.to_simple_term ctx) (l, r) in
              TypedSTerm.Form.eq l r
        | SLiteral.Neq (l, r) ->
           let l, r = CCPair.map_same (T.Conv.to_simple_term ctx) (l, r) in
              TypedSTerm.Form.neq l r
        | SLiteral.True -> TypedSTerm.Form.true_
        | SLiteral.False -> TypedSTerm.Form.false_

  let to_s_form ?allow_free_db ?(ctx = T.Conv.create ()) ?hooks lit =
     to_form ?hooks lit |> SLiteral.map ~f:(T.Conv.to_simple_term ?allow_free_db ctx) |> SLiteral.to_form
end

module View = struct
  let as_eqn lit =
     assert (no_prop_invariant lit);
     match lit with Equation (l, r, sign) -> Some (l, r, sign) | True | False -> None

  let get_eqn lit position =
     match (lit, position) with
        | Equation (l, r, sign), P.Left _ -> Some (l, r, sign)
        | Equation (l, r, sign), P.Right _ -> Some (r, l, sign)
        | True, _ | False, _ -> None
        | _ -> invalid_arg "get_eqn: wrong literal or position"

  let get_lhs = function Equation (lhs, _, _) -> Some lhs | _ -> None
  let get_rhs = function Equation (_, rhs, _) -> Some rhs | _ -> None
end

let _as_var t = T.as_var_exn (Lambda.eta_reduce t)

let as_inj_def lit =
   match View.as_eqn lit with
      | Some (l, r, false) -> (
         try
           let hd_l, hd_r = (T.head_exn l, T.head_exn r) in
           let vars_l, vars_r = (List.map _as_var (T.args l), List.map _as_var (T.args r)) in
           let args_l, args_r = (VS.of_list vars_l, VS.of_list vars_r) in

           (* We are looking for literal f X1 ... Xn ~= f Y1 ... Yn
              where all X_i, Y_i are different pairwise, but form each
              other also. *)
           if
             hd_l = hd_r
             && VS.cardinal args_l = List.length (T.args l)
             && VS.cardinal args_r = List.length (T.args r)
             && VS.cardinal args_l = VS.cardinal args_r
             && VS.inter args_l args_r = VS.empty
           then Some (hd_l, List.combine vars_l vars_r)
           else None
         with Invalid_argument _ -> None)
      | _ -> None

let is_pure_var lit =
   match lit with
      | Equation (l, r, _) -> (
         try
           ignore (_as_var l, _as_var r);
           true
         with Invalid_argument _ -> false)
      | _ -> false

let max_term_positions ~ord = function
   | Equation (lhs, rhs, _) -> (
      match Ordering.compare ord lhs rhs with
         | Comparison.Gt | Geq -> Term.ho_weight lhs
         | Lt | Leq -> Term.ho_weight rhs
         | _ -> Term.ho_weight lhs + Term.ho_weight rhs)
   | _ -> 1

let as_pos_pure_var lit =
   match View.as_eqn lit with
      | Some (l, r, true) when is_pure_var lit && is_positivoid lit -> Some (_as_var l, _as_var r)
      | _ -> None
