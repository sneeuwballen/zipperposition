(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
module Lits = Literals

let section = Util.Section.make ~parent:Const.section "bbox"
let pp_bbox_id : bool ref = ref true

module StringTbl = CCHashtbl.Make (struct
  type t = string

  let hash = CCString.hash
  let equal = CCString.equal
end)

type inductive_case = Cover_set.case

type payload =
  | Fresh (* fresh literal with no particular payload *)
  | Clause_component of Literals.t
  | Lemma of Cut_form.t
  | Case of inductive_case list (* branch in the induction tree *)

module Lit = Bool_lit.Make (struct
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

let compare_payload l1 l2 =
  match l1, l2 with
  | Fresh, Fresh -> 0
  | Clause_component l1, Clause_component l2 -> Lits.compare l1 l2
  | Lemma l1, Lemma l2 -> Cut_form.compare l1 l2
  | Case p1, Case p2 -> CCList.compare Cover_set.Case.compare p1 p2
  | Fresh, _ | Clause_component _, _ | Lemma _, _ | Case _, _ ->
    CCInt.compare (payload_to_int_ l1) (payload_to_int_ l2)

let pp_payload out = function
  | Fresh -> CCFormat.string out "<dummy>"
  | Clause_component lits ->
    Format.fprintf out "@<1>⟦@[<hv>%a@]@<1>⟧" Lits.pp lits
  | Lemma f -> Format.fprintf out "@<1>⟦lemma %a@<1>⟧" Cut_form.pp f
  | Case c ->
    Format.fprintf out "@<1>⟦@[<hv1>%a@]@<1>⟧"
      (Util.pp_list ~sep:" ∧ " Literal.pp)
      (List.map Cover_set.Case.to_lit c)

(* index for components (to ensure α-equivalence components map to the same
   boolean lit *)
module FV_components = FV_tree.Make (struct
  type t = Lits.t * payload * lit

  let compare (l1, i1, j1) (l2, i2, j2) =
    CCOrd.(
      Lits.compare l1 l2 <?> (compare_payload, i1, i2) <?> (Lit.compare, j1, j2))

  let to_lits (l, _, _) = Lits.to_form l |> Iter.of_list
  let labels _ = Util.Int_set.empty
end)

(* index for lemmas, to ensure α-equivalent lemmas have the same lit *)
module FV_lemma = Cut_form.FV_tbl (struct
  type t = payload * lit

  let compare (i1, j1) (i2, j2) =
    let open CCOrd.Infix in
    compare_payload i1 i2 <?> (Lit.compare, j1, j2)
end)

module ICaseTbl = CCHashtbl.Make (struct
  type t = Cover_set.case list

  let equal = CCList.equal Cover_set.Case.equal
  let hash = Hash.list Cover_set.Case.hash
end)

type state = {
  mutable clause_set: FV_components.t;
  mutable lemma_set: FV_lemma.t;
  mutable case_set: (payload * lit) ICaseTbl.t;
  mutable term_set: Lit.t Term.Tbl.t;
}

let k_state : state Flex_state.key = Flex_state.create_key ()

let get_state (flex_ref : Flex_state.t ref) =
  match Flex_state.get k_state !flex_ref with
  | Some st -> st
  | None ->
    let st =
      {
        clause_set = FV_components.empty ();
        lemma_set = FV_lemma.create ();
        case_set = ICaseTbl.create 16;
        term_set = Term.Tbl.create 16;
      }
    in
    flex_ref := Flex_state.add k_state st !flex_ref;
    st

(* should never be used *)
let dummy_payload = Fresh
let dummy_t = Lit.make dummy_payload
let make_fresh () = Lit.make dummy_payload

let _retrieve_alpha_equiv st lits =
  FV_components.retrieve_alpha_equiv_c st.clause_set
    (lits, dummy_payload, dummy_t)

let _retrieve_lemma st (f : Cut_form.t) = FV_lemma.get st.lemma_set f

(* put [lit] inside mappings, for retrieval by definition *)
let save_ st lit =
  let payload = Lit.payload lit in
  match payload with
  | Fresh -> ()
  | Clause_component lits ->
    (* be able to retrieve by lits *)
    st.clause_set <- FV_components.add st.clause_set (lits, payload, lit)
  | Lemma f -> FV_lemma.add st.lemma_set f (payload, lit)
  | Case p -> ICaseTbl.add st.case_set p (payload, lit)

let _check_variant lits lits' =
  Lits.matches lits lits' && Lits.matches lits' lits

let negate_ground lits =
  match lits with
  | [| lit0 |]
    when Literal.is_ground lit0 && Literal.is_negativoid lit0
         && not (Literal.is_constraint lit0) ->
    [| Literal.negate lits.(0) |], false
  | _ -> lits, true

let find_boolean_lit ~flex_ref lits =
  let st = get_state flex_ref in
  (* special case, negative ground literal *)
  let lits, sign = negate_ground lits in
  (* retrieve clause. the index doesn't matter for retrieval *)
  _retrieve_alpha_equiv st lits
  |> Iter.find_map (function
       | lits', Clause_component _, blit when Lits.are_variant lits lits' ->
         assert (Lit.sign blit);
         (* assert (_check_variant lits lits'); *)
         Some blit
       | _ -> None)
  |> CCOpt.map (fun t -> Lit.apply_sign sign t)

(* clause -> boolean lit *)
let inject_lits ~flex_ref lits =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "bbox.inject-lits" in
  let old_lit = find_boolean_lit ~flex_ref lits in
  match old_lit with
  | Some t -> t (* sign already applied*)
  | None ->
    let st = get_state flex_ref in
    (* build new literal *)
    let lits, sign = negate_ground lits in
    let lits_copy = Array.copy lits in
    let t = Lit.make (Clause_component lits_copy) in
    (* maintain mapping *)
    save_ st t;
    Lit.apply_sign sign t

let inject_lit ~flex_ref lit =
  let st = get_state flex_ref in
  let exception CantConvert in
  let lit_to_term lit =
    match lit with
    | Literal.Equation (lhs, _, _) when Literal.is_predicate_lit lit -> lhs
    | Literal.Equation (lhs, rhs, _) ->
      if Term.compare lhs rhs < 0 then
        Term.Form.eq lhs rhs
      else
        Term.Form.eq rhs lhs
    | Literal.False -> raise CantConvert
    | _ ->
      CCFormat.printf "lit: @[%a@]@." Literal.pp lit;
      invalid_arg "unknown literal"
  in

  try
    CCOpt.return
    @@
    match Term.Tbl.find_opt st.term_set (lit_to_term lit) with
    | Some t -> Lit.apply_sign (Literal.is_positivoid lit) t
    | None ->
      let term = lit_to_term lit in
      let bool_lit = Lit.make Fresh in
      Term.Tbl.add st.term_set term bool_lit;
      Lit.apply_sign (Literal.is_positivoid lit) bool_lit
  with CantConvert -> None

let inject_lemma ~flex_ref (f : Cut_form.t) : t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "bbox.inject-lemma" in
  let st = get_state flex_ref in
  let old_lit =
    match _retrieve_lemma st f with
    | None -> None
    | Some (Lemma _, blit) -> Some blit
    | Some _ -> assert false
  in
  match old_lit with
  | Some lit -> lit
  | None ->
    (* build new literal *)
    let lit = Lit.make (Lemma f) in
    (* maintain mapping *)
    save_ st lit;
    lit

let inject_case ~flex_ref p =
  let st = get_state flex_ref in
  (* normalize by sorting the list of cases *)
  let p = List.sort Cover_set.Case.compare p in
  try
    let _, i = ICaseTbl.find st.case_set p in
    i
  with Not_found ->
    let payload = Case p in
    let t = Lit.make payload in
    save_ st t;
    t

let must_be_kept lit =
  match Lit.payload (Lit.abs lit) with
  | Fresh | Clause_component _ -> false
  | Lemma _ | Case _ -> true

let is_lemma lit =
  match Lit.payload (Lit.abs lit) with
  | Lemma _ -> true
  | _ -> false

let is_case lit =
  match Lit.payload (Lit.abs lit) with
  | Case _ -> true
  | _ -> false

let as_case lit =
  match Lit.payload (Lit.abs lit) with
  | Case p -> Some p
  | _ -> None

let as_lemma lit =
  match Lit.payload (Lit.abs lit) with
  | Lemma f -> Some f
  | _ -> None

let as_lits lit =
  match Lit.payload (Lit.abs lit) with
  | Clause_component lits -> Some lits
  | _ -> None

(* boolean lit -> payload *)
let payload = Lit.payload
let sign = Lit.sign

let to_s_form (lit : t) =
  let module T = Term in
  let module F = TypedSTerm.Form in
  let f =
    match payload lit with
    | Fresh -> assert false (* TODO? *)
    | Clause_component lits ->
      F.box_opaque (Literals.Conv.to_s_form lits |> F.close_forall)
    | Lemma f -> Cut_form.to_s_form f |> F.box_opaque
    | Case l ->
      let ctx = T.Conv.create () in
      l
      |> List.map (fun t ->
             Cover_set.Case.to_lit t |> Literal.Conv.to_s_form ~ctx)
      |> F.and_
  in
  if sign lit then
    f
  else
    F.not_ f

let pp out i =
  if not (Lit.sign i) then CCFormat.string out "¬";
  pp_payload out (payload i);
  if !pp_bbox_id then Format.fprintf out "/%d" (Lit.to_int i |> abs);
  ()

let pp_bclause out lits =
  Format.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ⊔ " pp) lits

(* print a single boolean box *)
let pp_tstp out b =
  let pp_box_unsigned out b =
    match payload b with
    | Case p ->
      let lits = List.map Cover_set.Case.to_lit p |> Array.of_list in
      Literals.pp_tstp out lits
    | Clause_component lits ->
      CCFormat.within "(" ")" Literals.pp_tstp_closed out lits
    | Lemma f -> CCFormat.within "(" ")" Cut_form.pp_tstp out f
    | Fresh -> failwith "cannot print <fresh> boolean box"
  in
  if Lit.sign b then
    pp_box_unsigned out b
  else
    Format.fprintf out "@[~@ %a@]" pp_box_unsigned b

let pp_zf out i =
  let pp_payload out = function
    | Fresh -> CCFormat.string out "'dummy_sym'"
    | Clause_component lits -> Format.fprintf out "(@[<hv>%a@])" Lits.pp_zf lits
    | Lemma f -> Cut_form.pp_zf out f
    | Case c ->
      Format.fprintf out "(@[<hv>%a@])"
        (Util.pp_list ~sep:" && " Literal.pp_zf)
        (List.map Cover_set.Case.to_lit c)
  in
  if not (Lit.sign i) then CCFormat.string out "~";
  pp_payload out (payload i)

let () =
  Options.add_opts
    [
      "--pp-bbox-id", Arg.Set pp_bbox_id, " print boolean literals' IDs";
      ( "--no-pp-bbox-id",
        Arg.Clear pp_bbox_id,
        " do not print boolean literals' IDs" );
    ]
