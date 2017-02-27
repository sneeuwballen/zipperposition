

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dismatching Constraint} *)

open Logtk

module T = FOTerm
module Fmt = CCFormat
module BV = CCBV

type term = FOTerm.t

type constr = term * term

let prof_is_absurd  = Util.mk_profiler "dismatch.is_absurd"
let prof_is_trivial = Util.mk_profiler "dismatching.is_trivial"

type t =
  | Empty (** totally trivial *)
  | Pairs of pairs

and pairs = {
  pairs: (term * term) list;
  absurd: bool lazy_t;
}

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
let is_trivial_pair (c:constr): bool =
  let t, u = c in
  not (Unif.FO.are_unifiable t u)

let pp_pairs out l =
  let lhs_l, rhs_l = List.split l in
  Fmt.fprintf out "(@[<2>(@[<hv>%a@])@ ⋪ (@[<hv>%a@])@])"
    (Util.pp_list T.pp) lhs_l (Util.pp_list T.pp) rhs_l

(* given [t1…tn, u1…un], find a substitution [σ] extending [subst]
   such that [forall i. t_i = u_iσ]. *)
let match_rhs_to_lhs ~subst (l,sc) =
  let sc_rhs = ~-1 in
  assert (sc> ~-1);
  assert (Subst.codomain subst
          |> Sequence.map Scoped.scope |> Sequence.for_all (fun s->s>sc_rhs));
  try
    List.fold_left
      (fun subst (t,u) -> Unif.FO.matching ~subst ~pattern:(u,sc_rhs) (t,sc))
      subst l
    |> CCOpt.return
  with Unif.Fail -> None

(* absurd if there is a substitution σ such that [forall. t </| u],
   [t = u\sigma]. Indeed, any instance [tρ] of [t] will match [uσρ]. *)
let is_absurd_ l =
  begin match match_rhs_to_lhs ~subst:Subst.empty (l,0) with
    | None -> false
    | Some subst ->
      Util.debugf 5 "(@[constr_is_absurd %a@ :subst %a@])"
        (fun k->k pp_pairs l Subst.pp subst);
      true
  end

let is_absurd = function
  | Empty -> false
  | Pairs {absurd=lazy b; _} -> b

let cmp_pair = CCOrd.pair T.compare T.compare

let make_simpl_ l =
  if List.exists is_trivial_pair l
  then Empty
  else begin match l with
    | [] -> Empty
    | _ ->
      Pairs {
        pairs=CCList.sort_uniq ~cmp:cmp_pair l;
        absurd=lazy (is_absurd_ l);
      }
  end

let compare (c1:t)(c2:t): int = match c1, c2 with
  | Empty, Empty -> 0
  | Empty, Pairs _ -> -1
  | Pairs _, Empty -> 1
  | Pairs {pairs=l1;_}, Pairs {pairs=l2;_} ->
    CCList.compare cmp_pair l1 l2

let empty = Empty

let is_empty = function
  | Empty -> true
  | Pairs _ -> false

let make = make_simpl_

let combine c1 c2 : t = match c1, c2 with
  | Empty, c
  | c, Empty -> c
  | Pairs {pairs=l1;_}, Pairs {pairs=l2;_} ->
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
  | Empty -> Empty
  | Pairs {pairs=l; _} ->
    List.map
      (fun (t,u) ->
         let t = Subst.FO.apply ~renaming subst (t, sc_l) in
         t, u)
      l
    |> make

let is_trivial_ = function
  | Empty -> true
  | Pairs {pairs=l;_} ->
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

let is_trivial d = Util.with_prof prof_is_trivial is_trivial_ d

let pp out (c:t): unit = match c with
  | Empty -> ()
  | Pairs {pairs=[t,u];_} -> Fmt.fprintf out "(@[<2>%a@ ⋪ %a@])" T.pp t T.pp u
  | Pairs {pairs=l; _} -> pp_pairs out l

let to_string = Fmt.to_string pp

let is_absurd_with_ subst (c,sc): bool = match c with
  | Empty -> false
  | Pairs {pairs=l;_} ->
    begin match match_rhs_to_lhs ~subst (l,sc) with
      | None -> false
      | Some subst ->
        Util.debugf 5 "(@[constr_is_absurd %a@ :subst %a@])"
          (fun k->k pp c Subst.pp subst);
        true
    end

let is_absurd_with subst x =
  Util.with_prof prof_is_absurd (is_absurd_with_ subst) x

let vars_seq = function
  | Empty -> Sequence.empty
  | Pairs {pairs=l; _} ->
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
    | Empty, Empty -> Sequence.return subst
    | Empty, Pairs _
    | Pairs _, Empty -> Sequence.empty
    | Pairs {pairs=l1;_}, Pairs {pairs=l2;_} ->
      variants_arr_ subst (Array.of_list l1) sc1 (Array.of_list l2) sc2
  end

(* try to match pairwise [t1,u1] and [t2,u2] such that:
   [subst(t1)=t2], and there is [subst_rhs] s.t. [subst_rhs(u2)=u1]
   (that way, [t1,u1] covers all the terms covered by [t2,u2] *)
let subsumes_arr_ subst a1 sc1 a2 sc2 : _ Sequence.t =
  Unif.unif_array_com
    (subst,Subst.empty)
    (a1,sc1)
    (a2,sc2)
    ~op:(fun (subst,subst_rhs) ((t1,u1),sc1) ((t2,u2),sc2) ->
      try
        let subst = Unif.FO.matching ~subst ~pattern:(t1,sc1) (t2,sc2) in
        let subst_rhs = Unif.FO.matching ~subst:subst_rhs ~pattern:(u2,1)(u1,0) in
        Sequence.return (subst,subst_rhs)
      with Unif.Fail ->
        Sequence.empty)
  |> Sequence.map fst (* drop the internal substitution *)

let matching ?(subst=Subst.empty) (c1,sc1)(c2,sc2) : Subst.t Sequence.t =
  begin match c1, c2 with
    | Empty, _ -> Sequence.return subst
    | Pairs _, Empty ->
      Sequence.empty (* not finitary, would need to make [c1] trivial *)
    | Pairs {pairs=l1;_}, Pairs {pairs=l2;_} ->
      subsumes_arr_ subst (Array.of_list l1) sc1 (Array.of_list l2) sc2
  end

let are_variant a b =
  not (variant (a,0) (b,0) |> Sequence.is_empty)
