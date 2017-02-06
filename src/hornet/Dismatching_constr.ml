

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dismatching Constraint} *)

open Libzipperposition

module T = FOTerm
module Fmt = CCFormat

type term = FOTerm.t

type constr = term * term

type t =
  | Trivial
  | Pairs of (term * term) list

(* is the constraint trivially true?
   the criterion is: [(t, u)]  is trivial if [t] does not
   match [u], because no substitution will be able to make [t] equal
   to [u].
*)
let is_trivial_ (c:constr): bool =
  let t, u = c in
  not (Unif.FO.matches ~pattern:t u)

let make_simpl_ l =
  if List.exists is_trivial_ l
  then Trivial
  else Pairs l

let make = make_simpl_

let combine c1 c2 : t = match c1, c2 with
  | Trivial, _
  | _, Trivial -> Trivial
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

let find_solution_ l =
  try
    List.fold_left
      (fun subst (t,u) -> Unif.FO.matching ~subst ~pattern:(t,0) (u,1))
      Subst.empty l
    |> CCOpt.return
  with Unif.Fail -> None

(* real test for satisfiability: look for a solution *)
let has_solution_ (l:constr list): bool = match find_solution_ l with
  | None -> false
  | Some _ -> true

let find_solution = function
  | Trivial -> None
  | Pairs l -> find_solution_ l

let is_trivial = function
  | Trivial -> true
  | Pairs l -> not (has_solution_ l)

let is_sat = function
  | Trivial -> false
  | Pairs l -> has_solution_ l

(* use "⋪"? *)

let pp out (c:t): unit = match c with
  | Trivial -> ()
  | Pairs l ->
    let lhs_l, rhs_l = List.split l in
    Fmt.fprintf out "(@[<2>(@[<hv>%a@])@ ⋪ (@[<hv>%a@])@])"
      (Util.pp_list T.pp) lhs_l (Util.pp_list T.pp) rhs_l

let to_string = Fmt.to_string pp
