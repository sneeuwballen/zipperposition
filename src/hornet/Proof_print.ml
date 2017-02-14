
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Print Proofs} *)

open Libzipperposition
open Hornet_types

module HC = Horn_clause
module P_res = Proof_res
module U = Hornet_types_util
module Fmt = CCFormat

type t = proof_with_res

(* traverse recursively, with a table to preserve DAG *)
let pp_dag out (p:t): unit =
  let tbl : int P_res.Tbl.t = P_res.Tbl.create 32 in
  let count = ref 0 in
  (* recursive traversal. Returns the unique ID of this step *)
  let rec aux (x:t): int =
    let p, res = x in
    begin match P_res.Tbl.get tbl res with
      | Some id -> id
      | None ->
        let id = CCRef.incr_then_get count in
        P_res.Tbl.add tbl res id;
        (* print parents, get their ID *)
        let l =
          Proof.parents p |> List.map aux
        in
        Format.fprintf out "@[<hv4>@[@{<Green>* [%d]@} %a@]@ :from %a@ :dep %a@]@,"
          id Proof_res.pp res U.pp_proof p Fmt.Dump.(list int) l;
        id
    end
  in
  let aux' _ () = ignore (aux p) in
  Format.fprintf out "@[<v>%a@]" aux' ()

let get_proof (x:proof_with_res): proof = fst x
let get_res (x:proof_with_res): proof_res = snd x

let as_graph : (t,string) CCGraph.t =
  CCGraph.make
    (fun (p,_) ->
       let name = Proof.name p in
       Proof.parents p
       |> Sequence.of_list
       |> Sequence.map (fun p' -> name, p'))

let is_proof_of_false p = Proof_res.is_absurd (get_res p)
let is_goal p = match get_proof p with
  | P_from_stmt st ->
    begin match Statement.view st with
      | Statement.Goal _ | Statement.NegatedGoal _ -> true
      | _ -> false
    end
  | _ -> false
let is_assert p = match get_proof p with
  | P_from_stmt st ->
    begin match Statement.view st with
      | Statement.Assert _ | Statement.Def _
      | Statement.RewriteForm _ | Statement.RewriteTerm _ -> true
      | _ -> false
    end
  | _ -> false
let is_trivial p = match get_proof p with
  | P_trivial | P_bool_tauto -> true
  | _ -> false

let pp_dot out (p:t) : unit =
  let equal = CCFun.compose_binop get_res Proof_res.equal in
  let hash = CCFun.compose get_res Proof_res.hash in
  let pp_node (p:t) = Proof_res.to_string (get_res p) in
  (* how to display a node *)
  let attrs_v p =
    let label = pp_node p in
    let attrs = [`Label label; `Style "filled"; `Shape "box"] in
    if is_proof_of_false p then `Color "red" ::attrs
    (* else if has_absurd_lits p then `Color "orange" :: attrs *)
    else if is_assert p then `Color "yellow" :: attrs
    else if is_goal p then `Color "green" :: attrs
    else if is_trivial p then `Color "cyan" :: attrs
    else attrs
  in
  let attrs_e s = [`Label s; `Other ("dir", "back")] in
  CCGraph.Dot.pp
    ~tbl:(CCGraph.mk_table ~eq:equal ~hash 32)
    ~eq:equal
    ~attrs_v
    ~attrs_e
    ~name:"proof"
    ~graph:as_graph
    out p

let pp_dot_file file p =
  CCIO.with_out file
    (fun oc ->
       let out = Format.formatter_of_out_channel oc in
       pp_dot out p;
       Format.pp_print_flush out ();
       flush oc)
