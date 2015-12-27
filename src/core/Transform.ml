
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Transformations on Fs and Terms} *)

module T = FOTerm
module F = Formula.FO

type term = FOTerm.t
type form = Formula.FO.t

type t =
| RwTerm of Rewriting.TRS.t
| RwForm of Rewriting.FormRW.t
| Tr of string * (F.t -> F.t list)
  (** the function can return a conjunction of formulas. The
      string is a short name/description of the transformation *)

type transformation = t

let of_term_rule rule =
  RwTerm (Rewriting.TRS.(add (empty ()) rule))

let of_term_rules_seq seq =
  let trs = Rewriting.TRS.(add_seq (empty ()) seq) in
  RwTerm trs

let of_term_rules l =
  let trs = Rewriting.TRS.(add_list (empty ()) l) in
  RwTerm trs

let of_form_rule rule =
  RwForm (Rewriting.FormRW.(add (empty ()) rule))

let of_form_rules_seq seq =
  let frs = Rewriting.FormRW.(add_seq (empty ()) seq) in
  RwForm frs

let of_form_rules l =
  let frs = Rewriting.FormRW.(add_list (empty ()) l) in
  RwForm frs

let of_term_tr name term2term =
  let transform f = [F.map term2term f] in
  Tr (name, transform)

let open_and =
  Tr ("open_and", F.open_and)

let remove_trivial =
  Tr ("remove_trivial", fun f -> if F.is_trivial f then [] else [f])

let rec apply tr f = match tr with
  | RwTerm trs ->
    let f' = F.map (fun t -> Rewriting.TRS.rewrite trs t) f in
    [f']
  | RwForm frs ->
    let f' = Rewriting.FormRW.rewrite frs f in
    [f']
  | Tr (_, transform) ->
    let f' = transform f in
    match f' with
    | [f''] when F.equal f f'' -> f'
    | _ -> CCList.flat_map (apply tr) f'

let pp out tr = match tr with
  | RwTerm _ -> CCFormat.string out "TRS"
  | RwForm _ -> CCFormat.string out "FormRW"
  | Tr (name, _) -> CCFormat.string out name

let to_string = CCFormat.to_string pp

(** {2 Transformation DAG} *)

(** Abstraction over formulas with additional information. A FORM.t
    contains a formula, and is built from parent formula wrappers
    upon a transformation.
*)

module type FORM = sig
  type t

  val of_form : rule:string -> parents:t list -> F.t -> t

  val to_form : t -> F.t
end

(** This module provides an infrastructure to efficiently compute
    the fixpoint of a set of transformations on a set of formulas.
    Fs form a DAG, whose edges go from a formula to the formulas it
    transforms into; result set is the set of leaves reachable from the
    initial formulas.
*)

module type DAG = sig
  module Form : FORM

  type t

  val create : (string * transformation) list -> t
    (** Create a DAG that implements the given list of transformations *)

  val transform : t -> Form.t list -> Form.t list
    (** Transform a set of formulas recursively *)
end

module MakeDAG(Form : FORM) = struct
  module Form = Form

  type node = {
    form : Form.t;
    mutable children : node list;
    mutable explored : bool;
  } (** Node of the DAG, annotated with formula *)

  type t = {
    trans : (string * transformation) list;
    forms : node F.Tbl.t;
  } (** the DAG itself *)

  let create trans =
    { trans; forms = F.Tbl.create 23; }

  (* get node for this formula. Also returns true if it's a new node. *)
  let _get_node dag form =
    try
      F.Tbl.find dag.forms (Form.to_form form)
    with Not_found ->
      let node = { children=[]; form; explored = false; } in
      F.Tbl.add dag.forms (Form.to_form form) node;
      node

  (* add edge n1 -> n2 *)
  let _add_edge _dag n1 n2 =
    n1.children <- n2 :: n1.children

  let transform dag forms =
    (* initial queue for BFS *)
    let q = Queue.create () in
    List.iter
      (fun form -> Queue.push (_get_node dag form) q)
      forms;
    (* here will be formulas that cannot be transformed *)
    let leaves = F.Tbl.create 9 in
    (* attempt to apply any transformation of [tr_list] to the formula *)
    let rec try_trans tr_list node = match tr_list with
      | [] -> None
      | (rule,tr)::tr_list' ->
        let f = Form.to_form node.form in
        begin match apply tr f with
        | [f'] when F.equal f f' ->
          (* transformation failed *)
          try_trans tr_list' node
        | forms ->
          (* build list of new formulas, from [form], and update the DAG *)
          let parents = [node.form] in
          let forms = List.map (fun f -> Form.of_form ~rule ~parents f) forms in
          let nodes = List.map (_get_node dag) forms in
          List.iter (fun n' -> _add_edge dag node n') nodes;
          Some nodes
        end
    in
    while not (Queue.is_empty q); do
      let node = Queue.pop q in
      if node.explored
        then match node.children with
        | [] ->  (* already explored leaf *)
          F.Tbl.replace leaves (Form.to_form node.form) node.form
        | _::_ -> (* just explore children *)
          List.iter (fun n' -> Queue.push n' q) node.children
        else begin match try_trans dag.trans node with
          | None -> F.Tbl.replace leaves (Form.to_form node.form) node.form
          | Some nodes ->
            List.iter (fun n' -> Queue.push n' q) nodes;
            node.explored <- true
        end
    done;
    F.Tbl.fold (fun _ form acc -> form :: acc) leaves []
end

module FormDag = MakeDAG(struct
  type t = F.t

  let of_form ~rule:_ ~parents:_ f = f

  let to_form f = f
end)
