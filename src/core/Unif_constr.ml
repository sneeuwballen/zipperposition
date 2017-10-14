
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Unification Constraint} *)

module T = InnerTerm

type term = T.t

type t = {
  t1: term;
  sc1: Scoped.scope;
  t2: term;
  sc2: Scoped.scope;
  tags: Proof.tag list;
}

let[@inline] make ~tags (t1,sc1) (t2,sc2) = {t1;sc1;t2;sc2;tags}
let[@inline] tags t = t.tags

let apply_subst renaming subst (c:t): term * term =
  Subst.apply renaming subst (c.t1, c.sc1),
  Subst.apply renaming subst (c.t2, c.sc2)

let apply_subst_l renaming subst (l:t list): _ list =
  List.map (apply_subst renaming subst) l

let pp out (c:t) =
  CCFormat.fprintf out "(@[%a =?=@ %a@])" T.pp c.t1 T.pp c.t2

let hash (c:t) = Hash.combine4 (T.hash c.t1) c.sc1 (T.hash c.t2) c.sc2
let equal c1 c2 =
  T.equal c1.t1 c2.t1 &&
  T.equal c1.t2 c2.t2 &&
  c1.sc1 = c2.sc1 &&
  c1.sc2 = c2.sc2

let compare c1 c2: int =
  let open CCOrd.Infix in
  T.compare c1.t1 c2.t1
  <?> (T.compare, c1.t2, c2.t2)
  <?> (CCOrd.int, c1.sc1, c2.sc1)
  <?> (CCOrd.int, c1.sc2, c2.sc2)

let to_string = CCFormat.to_string pp

module FO = struct
  let make ~tags (t1,sc1) (t2,sc2) = make ~tags ((t1:Term.t:>T.t),sc1) ((t2:Term.t:>T.t),sc2)
end
