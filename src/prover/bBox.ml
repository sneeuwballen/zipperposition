
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module Lits = Literals

let section = Util.Section.make ~parent:Const.section "bbox"
let prof_inject_lits = Util.mk_profiler "bbox.inject_lits"

module StringTbl = CCHashtbl.Make(struct
    type t = string
    let hash = CCString.hash
    let equal = CCString.equal
  end)

type inductive_path = Ind_cst.path

type payload =
  | Fresh (* fresh literal with no particular payload *)
  | Clause_component of Literals.t
  | Lemma of Literals.t list
  | Case of inductive_path (* branch in the induction tree *)

module Lit = Bool_lit.Make(struct
  type t = payload
  let dummy = Fresh
end)

type t = Lit.t
type lit = t

let dummy = Lit.dummy

let payload_to_int_ = function
  | Fresh -> 0
  | Clause_component _ -> 1
  | Case _ -> 2
  | Lemma _ -> 3

let compare_payload l1 l2 = match l1, l2 with
  | Fresh, Fresh -> 0
  | Clause_component l1, Clause_component l2 -> Lits.compare l1 l2
  | Lemma l1, Lemma l2 -> CCList.compare Lits.compare l1 l2
  | Case p1, Case p2 -> Ind_cst.path_compare p1 p2
  | Fresh, _
  | Clause_component _, _
  | Lemma _, _
  | Case _, _ ->
    CCInt.compare (payload_to_int_ l1) (payload_to_int_ l2)

let pp_payload out = function
  | Fresh -> CCFormat.string out "<dummy>"
  | Clause_component lits ->
      Format.fprintf out "@<1>⟦@[<hv>%a@]@<1>⟧" Lits.pp lits
  | Lemma lits_l ->
      Format.fprintf out "@<1>⟦lemma @[<hv>%a@]@<1>⟧"
        (Util.pp_list ~sep:" & " Lits.pp) lits_l
  | Case p ->
      Format.fprintf out "@<1>⟦@[<hv1>%a@]@<1>⟧" Ind_cst.pp_path p

module FV = FeatureVector.Make(struct
    type t = Lits.t * payload * lit
    let compare (l1,i1,j1)(l2,i2,j2) =
      CCOrd.(Lits.compare l1 l2
             <?> (compare_payload, i1, i2)
             <?> (Lit.compare, j1, j2))
    let to_lits (l,_,_) = Lits.to_form l |> Sequence.of_list
  end)

module ICaseTbl = CCHashtbl.Make(struct
    type t = inductive_path
    let equal = Ind_cst.path_equal
    let hash = Ind_cst.path_hash
  end)

let _clause_set = ref (FV.empty()) (* FO lits -> blit *)
let _case_set = ICaseTbl.create 15 (* cst=cst -> blit *)

(* should never be used *)
let dummy_payload = Fresh
let dummy_t = Lit.make dummy_payload

let _retrieve_alpha_equiv lits =
  FV.retrieve_alpha_equiv_c !_clause_set (lits,dummy_payload,dummy_t)

(* put [lit] inside mappings, for retrieval by definition *)
let save_ lit =
  let payload = Lit.payload lit in
  match payload with
  | Fresh -> ()
  | Clause_component lits ->
      (* be able to retrieve by lits *)
      _clause_set := FV.add !_clause_set (lits, payload, lit)
  | Lemma _ -> () (* no retrieval *)
  | Case p ->
      ICaseTbl.add _case_set p (payload, lit)

(* clause -> boolean lit *)
let inject_lits_ lits  =
  (* special case: one negative literal. *)
  let lits, sign = match lits with
    | [| lit0 |] when Literal.is_ground lit0 && Literal.is_neg lit0 ->
        [| Literal.negate lits.(0) |], false
    | _ -> lits, true
  in
  (* retrieve clause. the index doesn't matter for retrieval *)
  _retrieve_alpha_equiv lits
  |> Sequence.filter_map
    (function
      | lits', Clause_component _, blit
        when Lits.are_variant lits lits' ->
          Some blit
      | _ -> None)
  |> Sequence.head
  |> (function
      | Some t -> Lit.apply_sign sign t
      | None ->
          (* build new literal *)
          let lits_copy = Array.copy lits in
          let t = Lit.make (Clause_component lits_copy) in
          (* maintain mapping *)
          save_ t;
          Lit.apply_sign sign t)

let inject_lits lits =
  Util.with_prof prof_inject_lits inject_lits_ lits

let inject_lemma l =
  assert (l<>[]);
  let t = Lit.make (Lemma l) in
  t

let inject_case p =
  try
    let _, i = ICaseTbl.find _case_set p in
    i
  with Not_found ->
    let payload = Case p in
    let t = Lit.make payload in
    save_ t;
    t

let must_be_kept lit =
  match Lit.payload (Lit.abs lit) with
    | Fresh
    | Clause_component _ -> false
    | Lemma _
    | Case _ -> true

let as_case lit = match Lit.payload (Lit.abs lit) with
  | Case p -> Some p
  | _ -> None

(* boolean lit -> payload *)
let payload = Lit.payload

let pp out i =
  if not (Lit.sign i) then Format.pp_print_string out "¬";
  pp_payload out (payload i)

let pp_bclause out lits =
  Format.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ⊔ " pp) lits
