
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Libzipperposition.FOTerm
module Util = Libzipperposition.Util
module Lits = Literals

module StringTbl = CCHashtbl.Make(struct
    type t = string
    let hash = CCString.hash
    let equal = CCString.equal
  end)

module type S = BBox_intf.S

let section = Util.Section.make ~parent:Const.section "bbox"

module Make(I : BBox_intf.TERM)
    (Case : BBox_intf.TERM)
  : S
    with module I = I
     and module Case = Case
= struct
  type t = Sat_solver.Lit.t
  type lit = t

  module I = I
  module Case = Case

  type inductive_cst = I.t
  type inductive_case = Case.t

  type injected =
    | Clause_component of Literals.t
    | Case of inductive_cst * inductive_case

  let compare_injected l1 l2 = match l1, l2 with
    | Clause_component l1, Clause_component l2 -> Lits.compare l1 l2
    | Case (l1,r1), Case (l2,r2) ->
        CCOrd.(I.compare l1 l2 <?> (Case.compare, r1, r2))
    | Clause_component _, Case _ -> 1
    | Case _, Clause_component _ -> -1

  let pp_injected out = function
    | Clause_component lits ->
        Format.fprintf out "⟦%a⟧" (CCFormat.array ~sep:" ∨ " Literal.pp) lits
    | Case (c, t) ->
        Format.fprintf out "⟦%a=%a⟧" I.pp c Case.pp t

  module FV = Libzipperposition.FeatureVector.Make(struct
      type t = Lits.t * injected * lit
      let compare (l1,i1,j1)(l2,i2,j2) =
        CCOrd.(Lits.compare l1 l2
               <?> (compare_injected, i1, i2)
               <?> (Sat_solver.Lit.compare, j1, j2))
      let to_lits (l,_,_) = Lits.to_form l |> Sequence.of_list
    end)
  module ITbl = CCHashtbl.Make(Sat_solver.Lit)
  module ICaseTbl = CCHashtbl.Make(struct
      type t = inductive_cst * inductive_case
      let equal (c1,t1) (c2,t2) = I.equal c1 c2 && Case.equal t1 t2
      let hash_fun (c,t) h = h |> CCHash.int_ (I.hash c) |> CCHash.int_ (Case.hash t)
      let hash = CCHash.apply hash_fun
    end)

  let _clause_set = ref (FV.empty())
  let _case_set = ICaseTbl.create 15
  let _lit2inj = ITbl.create 56
  let _names = StringTbl.create 15

  let fresh_lit =
    let n = ref 1 in
    fun () ->
      let lit = Bool_lit.make !n in
      incr n;
      lit

  let dummy_t = fresh_lit ()

  let _retrieve_alpha_equiv lits =
    let dummy_injected = Clause_component lits in
    FV.retrieve_alpha_equiv_c !_clause_set (lits,dummy_injected,dummy_t)

  let _save injected t =
    ITbl.add _lit2inj t injected;
    Util.debugf ~section 4 "@[save bool_lit %d =@ @[%a@]@]"
      (fun k->k (t:t:>int) pp_injected injected);
    match injected with
    | Clause_component lits ->
        (* also be able to retrieve by lits *)
        _clause_set := FV.add !_clause_set (lits, injected, t)
    | Case (c, case) ->
        ICaseTbl.add _case_set (c,case) (injected, t)

  (* clause -> boolean lit *)
  let inject_lits lits  =
    (* special case: one negative literal. *)
    let lits, sign =
      if Array.length lits = 1 && Literal.is_neq lits.(0) && Literal.is_ground lits.(0)
      then [| Literal.negate lits.(0) |], false
      else lits, true
    in
    (* retrieve clause. the index doesn't matter for retrieval *)
    _retrieve_alpha_equiv lits
    |> Sequence.filter_map
      (function
        | lits', Clause_component _, blit
          when Lits.are_variant lits lits' ->
            Some blit
        | _ -> None
      )
    |> Sequence.head
    |> (function
        | Some t -> Bool_lit.apply_sign sign t
        | None ->
            let i = fresh_lit () in
            (* maintain mapping *)
            let lits_copy = Array.copy lits in
            _save (Clause_component lits_copy) i;
            Bool_lit.apply_sign sign i
      )

  let inject_case c t =
    try
      let _, i = ICaseTbl.find _case_set (c,t) in
      i
    with Not_found ->
      let inj = Case (c, t) in
      let i = fresh_lit () in
      _save inj i;
      i

  (* boolean lit -> injected *)
  let extract i =
    if not (Bool_lit.sign i)
    then failwith "BBox.extract: require absolute bool lit";
    try Some (ITbl.find _lit2inj i)
    with Not_found -> None

  let extract_exn i =
    if not (Bool_lit.sign i)
    then failwith "BBox.extract: require absolute bool lit";
    try ITbl.find _lit2inj i
    with Not_found -> failwith "BBox.extact: not a proper injected lit"

  let inductive_cst b = match extract_exn b with
    | Clause_component _ -> None
    | Case (t, _) -> Some t

  let iter_injected k = ITbl.values _lit2inj k

  let pp out i =
    if not (Bool_lit.sign i) then Format.pp_print_string out "¬";
    let i = Bool_lit.abs i in
    match extract i with
    | None -> Format.fprintf out "L%d" (i:t:>int)
    | Some (Clause_component lits) ->
        Format.fprintf out "@[⟦%a⟧@]"
          (CCArray.print ~sep:" ∨ " Literal.pp) lits
    | Some (Case (c, case)) ->
        Format.fprintf out "⟦%a=%a⟧" I.pp c Case.pp case
end
