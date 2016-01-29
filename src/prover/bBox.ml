
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module Lits = Literals

module StringTbl = CCHashtbl.Make(struct
    type t = string
    let hash = CCString.hash
    let equal = CCString.equal
  end)

module type S = BBox_intf.S

let section = Util.Section.make ~parent:Const.section "bbox"
let prof_inject_lits = Util.mk_profiler "bbox.inject_lits"

module Make(Fresh : sig end) = struct

  type inductive_cst = Ind_cst.cst
  type inductive_case = Ind_cst.case

  type payload =
    | Fresh (* fresh literal with no particular payload *)
    | Clause_component of Literals.t
    | Case of inductive_cst * inductive_case

  module Lit = Bool_lit.Make(struct
    type t = payload
    let dummy = Fresh
  end)

  type t = Lit.t
  type lit = t

  let dummy = Lit.dummy

  let compare_payload l1 l2 = match l1, l2 with
    | Fresh, Fresh -> 0
    | Fresh, _  -> -1
    | _, Fresh -> 1
    | Clause_component l1, Clause_component l2 -> Lits.compare l1 l2
    | Case (l1,r1), Case (l2,r2) ->
        CCOrd.(Ind_cst.cst_compare l1 l2 <?> (Ind_cst.case_compare, r1, r2))
    | Clause_component _, Case _ -> 1
    | Case _, Clause_component _ -> -1

  let pp_payload out = function
    | Fresh -> CCFormat.string out "<dummy>"
    | Clause_component lits ->
        Format.fprintf out "⟦@[<hv>%a@]⟧" Lits.pp lits
    | Case (c, t) ->
        Format.fprintf out "⟦@[<hv1>%a@ = @[%a@]@]⟧"
          Ind_cst.pp_cst c Ind_cst.pp_case t

  module FV = Libzipperposition.FeatureVector.Make(struct
      type t = Lits.t * payload * lit
      let compare (l1,i1,j1)(l2,i2,j2) =
        CCOrd.(Lits.compare l1 l2
               <?> (compare_payload, i1, i2)
               <?> (Lit.compare, j1, j2))
      let to_lits (l,_,_) = Lits.to_form l |> Sequence.of_list
    end)

  module ICaseTbl = CCHashtbl.Make(struct
      type t = inductive_cst * inductive_case
      let equal (c1,t1) (c2,t2) = Ind_cst.cst_equal c1 c2 && Ind_cst.case_equal t1 t2
      let hash_fun (c,t) h =
        h |> CCHash.int_ (Ind_cst.cst_hash c) |> CCHash.int_ (Ind_cst.case_hash t)
      let hash = CCHash.apply hash_fun
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
    | Case (c, case) ->
        ICaseTbl.add _case_set (c,case) (payload, lit)

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

  let inject_case c t =
    try
      let _, i = ICaseTbl.find _case_set (c,t) in
      i
    with Not_found ->
      let payload = Case (c, t) in
      let t = Lit.make payload in
      save_ t;
      t

  (* boolean lit -> payload *)
  let payload = Lit.payload

  let inductive_cst b = match payload b with
    | Fresh
    | Clause_component _ -> None
    | Case (t, _) -> Some t

  let pp out i =
    if not (Lit.sign i) then Format.pp_print_string out "¬";
    pp_payload out (payload i)
end
