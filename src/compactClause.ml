
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compact clause representation} *)

open Logtk

module Lit = Literal
module Lits = Literals

module BLit = Bool_lit

type term = FOTerm.t

type bool_lit = bool * Literal.t array
(** A boolean literal, here, is a boxed (unsplittable) clause
    with a sign. *)

(* compare boolean literals *)
let _cmp_blit (s1,l1) (s2,l2) =
  CCOrd.(bool_ s1 s2 <?> (Lits.compare, l1, l2))

let _eq_blit l1 l2 = _cmp_blit l1 l2 = 0

let _hash_blit (s,l) h = h |> CCHash.bool_ s |> Lits.hash_fun l

type t = {
  lits : Literal.t array;
  trail : bool_lit list;
}

let equal c1 c2 =
  Lits.equal_com c1.lits c2.lits &&
  List.length c1.trail = List.length c2.trail &&
  List.for_all2 _eq_blit c1.trail c2.trail

let hash_fun {lits;trail} h =
  h |> Lits.hash_fun lits |> CCHash.list_ _hash_blit trail

let hash = CCHash.apply hash_fun

let compare c1 c2 =
  CCOrd.(Lits.compare c1.lits c2.lits <?> (list_ _cmp_blit, c1.trail, c2.trail))

let make lits trail = {lits; trail; }

let has_absurd_lits c = Lits.is_absurd c.lits

let is_empty c = has_absurd_lits c && List.length c.trail = 0

let iter c f = Array.iter f c.lits

let to_seq c = Sequence.of_array c.lits

let _pp_blit out (s,l) =
  let prefix = if s then "" else "¬" in
  Format.fprintf out "%s⟦%a⟧" prefix Lits.pp l

let _pp_trail out = function
  | [] -> ()
  | l -> Format.fprintf out " ← %a" (Util.pp_list ~sep:" ⊓ " _pp_blit) l

let pp out c =
  begin match c.lits with
    | [| |] -> CCFormat.string out "⊥"
    | [| x |] -> Lit.pp out x
    | l -> Format.fprintf out "%a" (CCFormat.array ~sep:" ∨ " Lit.pp) l
  end;
  _pp_trail out c.trail

let to_string c = CCFormat.to_string pp c

let pp_trail_tstp out = function
  | [] -> ()
  | l ->
      (* TODO: gather all variables, print them quantified, prints lits *)
      let pp_blit out (_,_) = assert false in
      Format.fprintf out " <= (%a)" (Util.pp_list ~sep:" & " pp_blit) l

let pp_tstp out c =
  begin match c.lits with
    | [| |] -> CCFormat.string out "$false"
    | [| x |] -> Lit.pp_tstp out x
    | l -> Format.fprintf out "(%a)" (CCFormat.array ~sep:" | " Lit.pp_tstp) l
  end;
  pp_trail_tstp out c.trail

let to_forms c =
  Array.map (fun l -> Lit.Conv.to_form l) c.lits

let lits c = c.lits
let trail c = c.trail

