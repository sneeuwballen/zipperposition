(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Manipulate proofs} *)

open Basic

module T = Terms
module S = FoSubst
module C = Clauses
module Lits = Literals
module Utils = FoUtils

(** {2 Constructors and utils} *)

let mk_axiom x filename clause_name = Axiom (x, filename, clause_name)

let mk_proof x rule_name premises = Proof (x, rule_name, premises)

let is_axiom = function | Axiom _ -> true | _ -> false
let is_proof = function | Proof _ -> true | _ -> false

let proof_clause proof = match proof with
  | Axiom (c, _, _) -> c
  | Proof (c, _, _) -> c

let proof_id proof = fst (proof_clause proof)

let proof_lits proof = snd (proof_clause proof)

(** Is the proof a proof of the clause? *)
let is_proof_of proof hc = proof_id proof = hc.hctag

(** Re-build a clause from its proof. Does not re-build parents list. *)
let recover_clause ~ctx proof =
  let lits = proof_lits proof in
  let proof = fun c -> C.adapt_proof proof c in
  C.mk_hclause_a ~ctx lits proof

(** Traverse the proof. Each proof node is traversed only once. *)
let traverse ?(traversed=ref Ptset.empty) proof k =
  (* set of already traversed proof nodes; queue of proof nodes
     yet to traverse *)
  let queue = Queue.create () in
  Queue.push proof queue;
  while not (Queue.is_empty queue) do
    let proof = Queue.take queue in
    if Ptset.mem (proof_id proof) !traversed then ()
    else begin
      traversed := Ptset.add (proof_id proof) !traversed;
      (* call [k] on the proof *)
      k proof;
      match proof with
      | Axiom _ -> ()
      | Proof (_, _, l) ->
        List.iter (fun proof' -> Queue.push proof' queue) l
    end
  done

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(** Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = ref Ptset.empty in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    let i = proof_id p in
    if Ptset.mem i !explored then () else begin
      explored := Ptset.add i !explored;
      match p with
      | Axiom _ -> depth := max d !depth
      | Proof (_, _, l) -> (* explore parents *)
        List.iter (fun p -> Queue.push (p, d+1) q) l
    end
  done;
  !depth

(** {2 Conversion to a graph of proofs} *)

let mk_graph () =
  Graph.empty
    ~hash:(fun p -> proof_id p)
    ~eq:(fun p1 p2 -> proof_id p1 = proof_id p2)
    10

(** Get a graph of the proof *)
let to_graph proof =
  let g = mk_graph () in
  traverse proof
    (fun p -> match p with
     | Axiom _ -> ()
     | Proof (_, rule, l) ->
       List.iter (fun p' ->
        Graph.add g p' rule p) l);
  g

let to_json json_key proof =
  (* how to translate the proof step *)
  let pp_step proof = 
    match proof with
    | Axiom (c, filename, name) ->
      Json.List [Json.String "axiom"; json_key c; Json.String filename; Json.String name]
    | Proof (c, rule, l) ->
      let premises = List.map (fun proof -> Json.Int (proof_id proof)) l in
      Json.List [Json.String "proof"; json_key c; Json.String rule; Json.List premises]
  in
  (* sequence *)
  let seq k =
    traverse proof
      (fun proof ->
        let step = Json.List [Json.Int (proof_id proof); pp_step proof] in
        k step)
  in Sequence.from_iter seq

let rec of_json json_key json = failwith "not implemented"

(** {2 Pretty printer for proofs} *)

let pp_proof_debug formatter proof =
  (* how to print the premises of a proof *)
  let pp_premises formatter l =
    Utils.pp_list ~sep:", "
      (fun formatter proof ->
        Format.fprintf formatter "@[<h>%a@]"
          Lits.pp_lits (proof_lits proof))
      formatter l
  in
  traverse proof
    (function
      | Axiom (c, f, s) ->
        Format.fprintf formatter
          "@[<hov 4>@[<h>%a@]@ <--- @[<h>axiom %s in %s@]@]@;"
          Lits.pp_lits (snd c) s f
      | Proof (c, rule, premises) ->
        (* print the proof step *)
        Format.fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>%s with @[<hv>%a@]@]@]@;"
          Lits.pp_lits (snd c) rule pp_premises premises)

let pp_proof_tstp formatter proof =
  traverse proof
    (function
      | Axiom (c, f, ax_name) ->
        let t = Lits.term_of_lits (snd c) in
        Format.fprintf formatter
          "@[<h>fof(%d, axiom, %a,@ @[<h>file('%s', %s)@]).@]@;"
          (fst c) T.pp_term_tstp#pp t f ax_name
      | Proof (c, name, premises) ->
        let t = T.close_forall (Lits.term_of_lits (snd c)) in
        let premises = List.map proof_id premises in
        let status = if name = "elim" || name = "to_cnf" then "esa" else "thm" in
        (* print the inference *)
        Format.fprintf formatter ("@[<h>fof(%d, plain, %a,@ " ^^
          "@[<h>inference('%s', [status(%s)], @[<h>[%a, theory(equality)]@])@]).@]@;")
          (fst c) T.pp_term_tstp#pp t name status
          (Utils.pp_list ~sep:"," Format.pp_print_int) premises)

let pp_proof_json formatter proof =
  (* how to print a single element *)
  let pp_item formatter json = Format.pp_print_string formatter (Json.to_string json) in
  let seq = to_json C.compact_to_json proof in
  Format.fprintf formatter "@[<hv>%a@]@;"
    (Sequence.pp_seq pp_item) seq

(** Prints the proof according to the given input switch *)
let pp_proof switch formatter proof = match switch with
  | "none" -> Utils.debug 1 "%% proof printing disabled"
  | "tstp" -> pp_proof_tstp formatter proof
  | "debug" -> pp_proof_debug formatter proof
  | "json" -> pp_proof_json formatter proof
  | _ -> failwith ("unknown proof-printing format: " ^ switch)

let print_vertex proof =
  let label = `Label (Utils.sprintf "@[<h>%a@]" Lits.pp_lits (proof_lits proof)) in
  let attributes = [`Shape "box"; `Style "filled"] in
  let attributes =
    if proof_lits proof = [||] then `Color "red" :: `Label "[]" :: attributes
    else if is_axiom proof then label :: `Color "yellow" :: attributes
    else label :: attributes in
  attributes
and print_edge v1 e v2 =
  [`Label e]

(** Add the proof to the given graph *)
let pp_dot ~name formatter proof =
  let graph = to_graph proof in
  assert (Graph.is_dag graph);
  Graph.pp ~name ~print_vertex ~print_edge formatter graph

(** print to dot into a file *)
let pp_dot_file ?(name="proof") filename proof =
  (* print graph on file *)
  let out = open_out filename in
  try
    (* write on the opened out channel *)
    let formatter = Format.formatter_of_out_channel out in
    Format.printf "%% print proof to %s@." filename;
    Format.fprintf formatter "%a@." (pp_dot ~name) proof;
    close_out out
  with _ -> close_out out
