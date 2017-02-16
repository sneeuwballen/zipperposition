

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dismatching Constraint} *)

open Libzipperposition

module T = FOTerm
module Fmt = CCFormat
module BV = CCBV

type term = FOTerm.t

type constr = term * term

type t =
  | Trivial
  | Pairs of (term * term) list

(* TODO: simplification of constraints so that LHS terms are always
   variables?

   if [has_solution_ c] and the solution is unique (matching),
   then the solution is itself a constraint [x1…xn </| t1…tn]
   that is equivalent to [c]

   → maybe the form should be [Pairs of (var * term) list] to reflect that,
     and we normalize upon substitution
*)

(* is the constraint trivially true?
   the criterion is: [(t, u)]  is trivial if [t] and [u] are not
   unifiable, because no substitution will be able to make [t] equal
   to [u].
*)
let is_trivial_ (c:constr): bool =
  let t, u = c in
  not (Unif.FO.are_unifiable t u)

let cmp_pair = CCOrd.pair T.compare T.compare

let make_simpl_ l =
  if List.exists is_trivial_ l
  then Trivial
  else Pairs (CCList.sort_uniq ~cmp:cmp_pair l)

let empty = Trivial

let make = make_simpl_

let combine c1 c2 : t = match c1, c2 with
  | Trivial, c
  | c, Trivial -> c
  | Pairs l1, Pairs l2 ->
    (* must rename variables in right-hand side pairs, so that there is
       no collision between [c1] and [c2]. *)
    let renaming = Subst.Renaming.create () in
    let l1 =
      List.map (fun (t,u) -> t, Subst.FO.apply ~renaming Subst.empty (u,0)) l1
    and l2 =
      List.map (fun (t,u) -> t, Subst.FO.apply ~renaming Subst.empty (u,1)) l2
    in
    make (List.rev_append l1 l2)

(* apply substitution. The RHS of each pair is left untouched *)
let apply_subst ~renaming subst (c, sc_l) : t = match c with
  | Trivial -> Trivial
  | Pairs l ->
    List.map
      (fun (t,u) ->
         let t = Subst.FO.apply ~renaming subst (t, sc_l) in
         t, u)
      l
    |> make

let is_trivial = function
  | Trivial -> true
  | Pairs l ->
    (* try to unify all pairs. No mgu -> no ground matching either. *)
    try
      let _ =
        List.fold_left
          (fun subst (t,u) -> Unif.FO.unification ~subst (t,0) (u,1))
          Subst.empty l
      in
      false
    with Unif.Fail ->
      true

(* given [t1…tn, u1…un], find a substitution
   such that [forall i. t_i = u_iσ]. *)
let match_rhs_to_lhs l =
  try
    List.fold_left
      (fun subst (t,u) -> Unif.FO.matching ~subst ~pattern:(u,1) (t,0))
      Subst.empty l
    |> CCOpt.return
  with Unif.Fail -> None

let pp out (c:t): unit = match c with
  | Trivial -> ()
  | Pairs l ->
    let lhs_l, rhs_l = List.split l in
    Fmt.fprintf out "(@[<2>(@[<hv>%a@])@ ⋪ (@[<hv>%a@])@])"
      (Util.pp_list T.pp) lhs_l (Util.pp_list T.pp) rhs_l

let to_string = Fmt.to_string pp

(* absurd if there is a substitution σ such that [forall. t </| u],
   [t = u\sigma]. Indeed, any instance [tρ] of [t] will match [uσρ]. *)
let is_absurd c = match c with
  | Trivial -> false
  | Pairs l ->
    begin match match_rhs_to_lhs l with
      | None -> false
      | Some subst ->
        Util.debugf 5 "(@[constr_is_absurd %a@ :subst %a@])"
          (fun k->k pp c Subst.pp subst);
        true
    end

let vars_seq = function
  | Trivial -> Sequence.empty
  | Pairs l ->
    Sequence.of_list l
    |> Sequence.map fst
    |> Sequence.flat_map T.Seq.vars

let vars_l (t:t): _ list =
  vars_seq t
  |> Sequence.to_rev_list
  |> CCList.sort_uniq ~cmp:(HVar.compare Type.compare)

(* find the substitutions making [a1] and [a2] the same constraint *)
let variants_arr_ subst a1 sc1 a2 sc2 : _ Sequence.t =
  (* perform simultaneous unification of LHS terms pairwise,
     and RHS terms pairwise. *)
  Unif.unif_array_com
    (subst,Subst.empty)
    (a1,sc1)
    (a2,sc2)
    ~op:(fun (subst,subst_rhs) ((t1,u1),sc1) ((t2,u2),sc2) ->
      try
        let subst = Unif.FO.variant ~subst (t1,sc1) (t2,sc2) in
        let subst_rhs = Unif.FO.variant ~subst:subst_rhs (u1,0)(u2,1) in
        Sequence.return (subst,subst_rhs)
      with Unif.Fail ->
        Sequence.empty)
  |> Sequence.map fst (* drop the internal substitution *)

let variant ?(subst=Subst.empty) (c1,sc1)(c2,sc2) : Subst.t Sequence.t =
  begin match c1, c2 with
    | Trivial, Trivial -> Sequence.return subst
    | Trivial, Pairs _
    | Pairs _, Trivial -> Sequence.empty
    | Pairs l1, Pairs l2 ->
      variants_arr_ subst (Array.of_list l1) sc1 (Array.of_list l2) sc2
  end

let are_variant a b =
  not (variant (a,0) (b,0) |> Sequence.is_empty)
