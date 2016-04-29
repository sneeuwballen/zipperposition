
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Loc = ParseLocation
module Hash = CCHash

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make ~parent:Const.section "proof"

type rule_info =
  | I_subst of Substs.t
  | I_pos of Position.t
  | I_comment of string

type rule = {
  rule_name: string;
  rule_info: rule_info list;
}

let mk_rule ?(subst=[]) ?(pos=[]) ?(comment=[]) name =
  let rec map_append f l1 l2 = match l1 with
    | [] -> l2
    | x :: l1' -> map_append f l1' (f x :: l2)
  in
  { rule_name=name;
    rule_info=
      []
      |> map_append (fun x->I_subst x) subst
      |> map_append (fun x->I_pos x) pos
      |> map_append (fun x->I_comment x) comment;
  }

(** Classification of proof steps *)
type kind =
  | Inference of rule
  | Simplification of rule
  | Esa of rule
  | Assert of StatementSrc.t
  | Goal of StatementSrc.t
  | Data of StatementSrc.t * Type.t Statement.data
  | Trivial (** trivial, or trivial within theories *)

type result =
  | Form of form
  | Clause of SClause.t
  | BoolClause of bool_lit list

(** A proof step, without the conclusion *)
type t = {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: of_ list;
}

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

let result_as_clause p = match p.result with
  | Clause c -> c
  | Form _ | BoolClause _ -> invalid_arg "result_as_clause"

let result_as_form p = match p.result with
  | Clause _ | BoolClause _ -> invalid_arg "result_as_form"
  | Form f -> f

(** {2 Constructors and utils} *)

let id_ = ref 0
let get_id_ () =
  let n = !id_ in
  incr id_;
  n

let mk_trivial = {id=get_id_(); parents=[]; kind=Trivial; dist_to_goal=None; }

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
  let src = StatementSrc.make ?loc ~name file in
  mk_assert src

let mk_goal' ?loc ~file ~name () =
  let src = StatementSrc.make ?loc ~name file in
  mk_goal src

let mk_inference ~rule parents =
  mk_step_ (Inference rule) parents

let mk_simp ~rule parents =
  mk_step_ (Simplification rule) parents

let mk_esa ~rule parents =
  mk_step_ (Esa rule) parents

let mk_f_ step res = {step; result=Form res; }

let mk_f_trivial = mk_f_ mk_trivial

let mk_f_inference ~rule f parents =
  let step = mk_inference ~rule parents in
  mk_f_ step f

let mk_f_simp ~rule f parents =
  let step = mk_simp ~rule parents in
  mk_f_ step f

let mk_f_esa ~rule f parents =
  let step = mk_esa ~rule parents in
  mk_f_ step f

let mk_c step c = {step; result=Clause c; }
let mk_bc step c = {step; result=BoolClause c; }

let adapt_c p c =
  { p with result=Clause c; }

let adapt_f p f =
  { p with result=Form f; }

let is_trivial = function
  | {kind=Trivial; _} -> true
  | _ -> false

let rule p = match p.kind with
  | Trivial
  | Assert _
  | Data _
  | Goal _-> None
  | Esa rule
  | Simplification rule
  | Inference rule -> Some rule

let is_assert p = match p.kind with Assert _ -> true | _ -> false
let is_goal p = match p.kind with Goal _ -> true  | _ -> false

let equal p1 p2 = p1.id=p2.id
let compare p1 p2 = CCInt.compare p1.id p2.id
let hash p = p.id

let compare_by_result p1 p2 = match p1.result, p2.result with
  | Clause c1, Clause c2 -> SClause.compare c1 c2
  | Form f1, Form f2 -> TypedSTerm.compare f1 f2
  | BoolClause l1, BoolClause l2 -> CCOrd.list_ BBox.Lit.compare l1 l2
  | Clause _, _ -> 1
  | _, Clause _ -> -1
  | Form _, _ -> 1
  | _, Form _ -> -1

(** {2 Proof traversal} *)

module Tbl = CCHashtbl.Make(CCInt)

let traverse_depth ?(traversed=Tbl.create 16) proof k =
  let depth = ref 0 in
  let current, next = ref [proof], ref [] in
  while !current <> [] do
    (* exhaust the current layer of proofs to explore *)
    while !current <> [] do
      let proof = List.hd !current in
      current := List.tl !current;
      if Tbl.mem traversed proof.id then ()
      else (
        Tbl.add traversed proof.id ();
        (* traverse premises first *)
        List.iter (fun proof' -> next := proof'.step :: !next) proof.parents;
        (* yield proof *)
        k (proof, !depth);
      )
    done;
    (* explore next layer *)
    current := !next;
    next := [];
    incr depth;
  done

let traverse ?traversed proof k =
  traverse_depth ?traversed proof (fun (p, _depth) -> k p)

let distance_to_goal p = p.dist_to_goal

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(* Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = Tbl.create 11 in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    if Tbl.mem explored proof.id then () else begin
      Tbl.add explored proof.id ();
      begin match p.kind with
        | Assert _ | Goal _ | Trivial | Data _ -> depth := max d !depth
        | Inference _ | Esa _ | Simplification _ -> ()
      end;
      (* explore parents *)
      List.iter (fun p' -> Queue.push (p'.step, d+1) q) p.parents
    end
  done;
  !depth

(** {2 IO} *)

let pp_rule ~info out r =
  let pp_info out = function
    | I_subst s -> Format.fprintf out " with @[%a@]" Substs.pp s
    | I_pos p -> Format.fprintf out " at @[%a@]" Position.pp p
    | I_comment s -> Format.fprintf out " %s" s
  in
  let pp_list pp = Util.pp_list ~sep:"" pp in
  if info
  then Format.fprintf out "@[%s%a@]" r.rule_name (pp_list pp_info) r.rule_info
  else Format.fprintf out "'%s'" r.rule_name

let pp_kind_tstp out k =
  match k with
  | Assert src
  | Goal src ->
      let file = src.StatementSrc.file in
      begin match src.StatementSrc.name with
      | None -> Format.fprintf out "file('%s')" file
      | Some name -> Format.fprintf out "file('%s', '%s')" file name
      end
  | Data _ -> Util.error ~where:"ProofStep" "cannot print `Data` step in TPTP"
  | Inference rule ->
      Format.fprintf out "inference(%a, [status(thm)])" (pp_rule ~info:false) rule
  | Simplification rule ->
      Format.fprintf out "inference(%a, [status(thm)])" (pp_rule ~info:false) rule
  | Esa rule ->
      Format.fprintf out "inference(%a, [status(esa)])" (pp_rule ~info:false) rule
  | Trivial ->
      Format.fprintf out "trivial([status(thm)])"

let pp_kind out k =
  let pp_src kind out src =
    let file = src.StatementSrc.file in
    match src.StatementSrc.name with
    | None -> Format.fprintf out "'%s'%s" file kind
    | Some name -> Format.fprintf out "'%s' in '%s'%s" name file kind
  in
  match k with
  | Assert src -> pp_src "" out src
  | Goal src -> pp_src " (goal)" out src
  | Data (src, _) -> pp_src " (data)" out src
  | Inference rule ->
      Format.fprintf out "inf %a" (pp_rule ~info:true) rule
  | Simplification rule ->
      Format.fprintf out "simp %a" (pp_rule ~info:true) rule
  | Esa rule ->
      Format.fprintf out "esa %a" (pp_rule ~info:true) rule
  | Trivial -> CCFormat.string out "trivial"

