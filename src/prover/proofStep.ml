
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Logtk

module Loc = ParseLocation
module Stmt = Statement
module T = TypedSTerm
module F = T.Form

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make ~parent:Const.section "proof"

type statement_src = Statement.source

type rule = string
let rule_name r = r

let mk_rule name = name
let mk_rulef fmt = CCFormat.ksprintf ~f:mk_rule fmt

(** Classification of proof steps *)
type kind =
  | Inference of rule * string option
  | Simplification of rule * string option
  | Esa of rule * string option
  | Assert of statement_src
  | Goal of statement_src
  | Lemma
  | Data of statement_src * Type.t Statement.data
  | Trivial (** trivial, or trivial within theories *)

type result =
  | Form of form
  | Clause of SClause.t
  | BoolClause of bool_lit list
  | Stmt of Statement.input_t

(** A proof step, without the conclusion *)
type t = {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: of_ list;
}

(* FIXME: should be either a real step; or a [subst, scoped of_]
   for cases with instantiation *)
(** Proof Step with its conclusion *)
and of_ = {
  step: t;
  result : result
}

type proof = of_

let result p = p.result
let step p = p.step
let kind p = p.kind
let parents p = p.parents

let equal p1 p2 = p1.id=p2.id
let compare p1 p2 = CCInt.compare p1.id p2.id
let hash p = p.id

let res_to_int_ = function
  | Clause _ -> 0
  | Form _ -> 1
  | BoolClause _ -> 2
  | Stmt _ -> 3

let compare_result a b = match a, b with
  | Clause c1, Clause c2 -> SClause.compare c1 c2
  | Form f1, Form f2 -> TypedSTerm.compare f1 f2
  | BoolClause l1, BoolClause l2 -> CCOrd.list BBox.Lit.compare l1 l2
  | Stmt s1, Stmt s2 -> Statement.compare s1 s2
  | Clause _, _
  | Form _, _
  | BoolClause _, _
  | Stmt _, _
    -> CCInt.compare (res_to_int_ a) (res_to_int_ b)

let compare_proof a b =
  let (<?>) = CCOrd.(<?>) in
  compare a.step b.step <?> (compare_result, a.result, b.result)

let equal_result a b = match a, b with
  | Clause c1, Clause c2 -> SClause.equal c1 c2
  | Form f1, Form f2 -> TypedSTerm.equal f1 f2
  | BoolClause l1, BoolClause l2 -> CCList.equal BBox.Lit.equal l1 l2
  | Stmt s1, Stmt s2 -> Statement.compare s1 s2 = 0
  | Clause _, _
  | Form _, _
  | BoolClause _, _
  | Stmt _, _
    -> false

let equal_proof a b =
  equal a.step b.step && equal_result a.result b.result

let hash_proof a = hash a.step

module PTbl = CCHashtbl.Make(struct
    type t = of_
    let equal = equal_proof
    let hash = hash_proof
  end)

let compare_by_result a b = compare_result a.result b.result

(** {2 Constructors and utils} *)

let id_ = ref 0
let get_id_ () =
  let n = !id_ in
  incr id_;
  n

(* TODO: if subst not empty, record the instantiation *)
(* TODO: use this in every inference step that has a substitution *)
let instantiate (s:Subst.t) (o,sc_o): of_ = o

let mk_trivial = {id=get_id_(); parents=[]; kind=Trivial; dist_to_goal=None; }
let mk_lemma = {id=get_id_(); parents=[]; kind=Lemma; dist_to_goal=Some 0; }

let combine_dist o p = match o, p.step.dist_to_goal with
  | None, None -> None
  | (Some _ as res), None
  | None, (Some _ as res) -> res
  | Some x, Some y -> Some (max x y)

let mk_step_ kind parents =
  (* distance to goal *)
  let dist_to_goal = match parents with
    | [] -> None
    | [p] -> CCOpt.map succ p.step.dist_to_goal
    | [p1;p2] ->
      CCOpt.map succ (combine_dist p1.step.dist_to_goal p2)
    | p::l ->
      CCOpt.map succ (List.fold_left combine_dist p.step.dist_to_goal l)
  in
  { id=get_id_(); kind; parents; dist_to_goal; }

let mk_data src data =
  mk_step_ (Data (src,data)) []

let mk_assert src = mk_step_ (Assert src) []

let mk_goal src = mk_step_ (Goal src) []

let mk_assert' ?loc ~file ~name () =
  let src = Stmt.Src.from_file ?loc ~name file Stmt.R_assert in
  mk_assert src

let mk_goal' ?loc ~file ~name () =
  let src = Stmt.Src.from_file ?loc ~name file Stmt.R_goal in
  mk_goal src

let mk_inference ?comment ~rule parents =
  mk_step_ (Inference (rule,comment)) parents

let mk_simp ?comment ~rule parents =
  mk_step_ (Simplification (rule,comment)) parents

let mk_esa ?comment ~rule parents =
  mk_step_ (Esa (rule,comment)) parents

let mk_f step res = {step; result=Form res; }

let mk_f_trivial = mk_f mk_trivial

let mk_f_inference ~rule f parents =
  let step = mk_inference ~rule parents in
  mk_f step f

let mk_f_simp ~rule f parents =
  let step = mk_simp ~rule parents in
  mk_f step f

let mk_f_esa ~rule f parents =
  let step = mk_esa ~rule parents in
  mk_f step f

let mk_c step c = {step; result=Clause c; }
let mk_bc step c = {step; result=BoolClause c; }
let mk_stmt step stmt = {step; result=Stmt stmt; }

let adapt_c p c =
  { p with result=Clause c; }

let adapt_f p f =
  { p with result=Form f; }

let is_trivial = function
  | {kind=Trivial; _} -> true
  | _ -> false

let rule p = match p.kind with
  | Trivial
  | Lemma
  | Assert _
  | Data _
  | Goal _-> None
  | Esa (rule,_)
  | Simplification (rule,_)
  | Inference (rule,_)
    -> Some rule

let comment p = match p.kind with
  | Trivial
  | Lemma
  | Assert _
  | Data _
  | Goal _-> None
  | Esa (_,c)
  | Simplification (_,c)
  | Inference (_,c)
    -> Some c

let is_assert p = match p.kind with Assert _ -> true | _ -> false
let is_goal p = match p.kind with Goal _ | Lemma -> true | _ -> false

(** {2 Proof traversal} *)

let distance_to_goal p = p.dist_to_goal

(** {2 IO} *)

let pp_comment out = function
  | None -> ()
  | Some s -> Format.fprintf out " %s" s

let pp_rule out r = Format.fprintf out "'%s'" r

let rec pp_src_tstp out src = match Stmt.Src.view src with
  | Stmt.Internal _
  | Stmt.Input _ -> ()
  | Stmt.From_file (src,_) ->
    let file = src.Stmt.file in
    begin match src.Stmt.name with
      | None -> Format.fprintf out "file('%s')" file
      | Some name -> Format.fprintf out "file('%s', '%s')" file name
    end
  | Stmt.Neg (_,src') ->
    Format.fprintf out "inference(@['negate_goal',@ [status(thm)],@ [%a]@])"
      pp_src_tstp src'
  | Stmt.CNF (_,src') ->
    Format.fprintf out "inference(@['clausify',@ [status(esa)],@ [%a]@])"
      pp_src_tstp src'
  | Stmt.Preprocess ((_,src'),msg) ->
    Format.fprintf out
      "inference(@['%s',@ [status(esa)],@ [%a]@])" msg pp_src_tstp src'
  | Stmt.Renaming ((_,src'), id, form) ->
    Format.fprintf out
      "inference(@['renaming',@ [status(esa)],@ [%a],@ on(@[%a<=>%a@])@])"
      pp_src_tstp src' ID.pp id TypedSTerm.TPTP.pp form

let pp_kind_tstp out k =
  match k with
    | Assert src
    | Goal src -> pp_src_tstp out src
    | Lemma -> Format.fprintf out "lemma"
    | Data _ -> Util.error ~where:"ProofStep" "cannot print `Data` step in TPTP"
    | Inference (rule,_) ->
      Format.fprintf out "inference(%a, [status(thm)])" pp_rule rule
    | Simplification (rule,_) ->
      Format.fprintf out "inference(%a, [status(thm)])" pp_rule rule
    | Esa (rule,_) ->
      Format.fprintf out "inference(%a, [status(esa)])" pp_rule rule
    | Trivial ->
      Format.fprintf out "trivial([status(thm)])"

let rec pp_src out src = match Stmt.Src.view src with
  | Stmt.Internal _
  | Stmt.Input _ -> ()
  | Stmt.From_file (src,_) ->
    let file = src.Stmt.file in
    begin match src.Stmt.name with
      | None -> Format.fprintf out "'%s'" file
      | Some name -> Format.fprintf out "'%s' in '%s'" name file
    end
  | Stmt.Neg (_,src') ->
    Format.fprintf out "(@[neg@ %a@])" pp_src src'
  | Stmt.CNF (_,src') ->
    Format.fprintf out "(@[CNF@ %a@])" pp_src src'
  | Stmt.Renaming ((_,src'), id, form) ->
    Format.fprintf out "(@[renaming@ [%a]@ :name %a@ :on @[%a@]@])"
      pp_src src' ID.pp id TypedSTerm.pp form
  | Stmt.Preprocess ((_,src'),msg) ->
    Format.fprintf out "(@[preprocess@ [%a]@ :msg %S@])" pp_src src' msg

let pp_kind out k =
  match k with
    | Assert src -> pp_src out src
    | Goal src -> Format.fprintf out "goal %a" pp_src src
    | Lemma -> Format.fprintf out "lemma"
    | Data (src, _) -> Format.fprintf out "data %a" pp_src src
    | Inference (rule,c) ->
      Format.fprintf out "inf %a%a" pp_rule rule pp_comment c
    | Simplification (rule,c) ->
      Format.fprintf out "simp %a%a" pp_rule rule pp_comment c
    | Esa (rule,c) ->
      Format.fprintf out "esa %a%a" pp_rule rule pp_comment c
    | Trivial -> CCFormat.string out "trivial"


(** {2 Conversion} *)


let res_to_form (r:result): form = match r with
  | Form f -> f
  | Clause c -> SClause.to_s_form c |> F.close_forall
  | BoolClause c ->
    List.map BBox.to_s_form c |> F.or_
  | Stmt st ->
    (* assimilate the statement to its formulas *)
    Stmt.Seq.forms st |> Sequence.to_list |> F.and_

(* FIXME: write this properly, including instantiation steps *)
let to_llproof (p:of_): LLProof.t =
  let tbl = PTbl.create 32 in
  let rec conv p: LLProof.t =
    begin match PTbl.get tbl p with
      | Some r -> r
      | None ->
        let res = conv_step p in
        PTbl.add tbl p res;
        res
    end
  and conv_step p =
    let res = res_to_form (result p) in
    let parents = List.map conv (parents @@ step p) in
    begin match kind @@ step p with
      | Inference (name,_)
      | Simplification (name,_) ->
        (* TODO: check, perhaps with side conditions *)
        LLProof.inference `No_check res name parents
      | Esa (name,_) ->
        (* TODO: check, perhaps with side conditions *)
        LLProof.esa `No_check res name parents
      |Trivial -> LLProof.trivial res
      | Assert _ -> LLProof.assert_ res
      | Goal _ -> LLProof.assert_ res
      | Lemma -> LLProof.trivial res
      | Data (_,_) -> assert false (* TODO *)
    end
  in
  conv p
