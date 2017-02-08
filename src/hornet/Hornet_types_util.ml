
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basics for Hornet Types} *)

open Libzipperposition
open Hornet_types

module T = FOTerm
module Fmt = CCFormat

let pp_lit out (t:lit): unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t
  | Eq (t,u,true) -> Fmt.fprintf out "@[%a@ = %a@]" T.pp t T.pp u
  | Eq (t,u,false) -> Fmt.fprintf out "@[%a@ @<1>≠ %a@]" T.pp t T.pp u

let pp_clause out (a:clause) =
  Fmt.fprintf out "[@[%a@]]" (Fmt.seq pp_lit) (IArray.to_seq a.c_lits)

let pp_proof out (p:proof) : unit = match p with
  | P_from_stmt st ->
    Fmt.fprintf out "(@[from_stmt@ %a@])" Statement.pp_clause st
  | P_instance (c, subst) ->
    Fmt.fprintf out "(@[<hv2>instance@ :clause %a@ :subst %a@])"
      pp_clause c Subst.pp subst
  | P_avatar_split c ->
    Fmt.fprintf out "(@[<hv2>avatar_split@ %a@])" pp_clause c
  | P_split c ->
    Fmt.fprintf out "(@[<hv2>split@ %a@])" pp_clause c
  | P_superposition _ -> assert false (* TODO *)

let pp_constraint out (c:c_constraint_): unit = match c with
  | C_dismatch d -> Dismatching_constr.pp out d

let pp_hclause out (c:horn_clause): unit =
  let pp_constr out = function
    | [] -> Fmt.unit out ()
    | l -> Fmt.fprintf out "@[<hv2>| %a@]" (Fmt.list pp_constraint) l
  in
  Fmt.fprintf out "(@[%a@ <- @[<hv>%a@]@,%a@])"
    pp_lit c.hc_head
    (Fmt.seq pp_lit) (IArray.to_seq c.hc_body)
    pp_constr c.hc_constr

let equal_lit (a:lit) (b:lit): bool = match a, b with
  | Bool b1, Bool b2 -> b1=b2
  | Atom (t1,sign1), Atom (t2,sign2) -> T.equal t1 t2 && sign1=sign2
  | Eq (t1,u1,sign1), Eq (t2,u2,sign2) ->
    sign1=sign2 &&
    T.equal t1 t2 && T.equal u1 u2
  | Bool _, _
  | Atom _, _
  | Eq _, _
    -> false

let hash_lit : lit -> int = function
  | Bool b -> Hash.combine2 10 (Hash.bool b)
  | Atom (t,sign) -> Hash.combine3 20 (T.hash t) (Hash.bool sign)
  | Eq (t,u,sign) -> Hash.combine4 30 (T.hash t) (T.hash u) (Hash.bool sign)

let equal_bool_lit a b : bool =
  a.bl_sign = b.bl_sign
  &&
  begin match a.bl_atom, b.bl_atom with
    | A_fresh i, A_fresh j ->  i=j
    | A_box_clause (_,i1), A_box_clause (_,i2) -> i1=i2
    | A_ground l1, A_ground l2 -> equal_lit l1 l2
    | A_select (_,_,id1), A_select (_,_,id2) -> id1=id2
    | A_fresh _, _
    | A_box_clause _, _
    | A_ground _, _
    | A_select _, _
      -> false
  end

let hash_bool_lit a : int = match a.bl_atom with
  | A_fresh i -> Hash.combine3 10 (Hash.bool a.bl_sign) (Hash.int i)
  | A_box_clause (_,i) -> Hash.combine2 15 (Hash.int i)
  | A_select (_,_,i) ->
    Hash.combine3 20 (Hash.bool a.bl_sign) (Hash.int i)
  | A_ground lit -> Hash.combine2 50 (hash_lit lit)

let pp_bool_lit out l =
  let pp_atom out = function
    | A_fresh i -> Fmt.fprintf out "fresh_%d" i
    | A_box_clause (c,i) -> Fmt.fprintf out "%a/%d" pp_clause c i
    | A_select (c,i,id) ->
      Fmt.fprintf out "@[select@ :idx %d@ :id %d :clause %a@]" i id pp_clause c
    | A_ground lit -> pp_lit out lit
  in
  if l.bl_sign
  then Fmt.within "(" ")" pp_atom out l.bl_atom
  else Fmt.fprintf out "(¬%a)" pp_atom l.bl_atom
