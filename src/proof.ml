
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Manipulate proofs} *)

open Logtk

module T = Term
module S = Substs

type t =
  | Axiom of CompactClause.t * string * string (** file, axiom name *)
  | Infer of CompactClause.t * string * t list

(** {2 Constructors and utils} *)

let mk_axiom x filename clause_name = Axiom (x, filename, clause_name)

let mk_infer x rule_name premises = Infer (x, rule_name, premises)

let is_axiom = function | Axiom _ -> true | _ -> false
let is_infer = function | Infer _ -> true | _ -> false

let proof_clause proof = match proof with
  | Axiom (c, _, _) -> c
  | Infer (c, _, _) -> c

let proof_id proof = CompactClause.id (proof_clause proof)

let proof_lits proof = CompactClause.lits (proof_clause proof)

let is_proof_of proof c = proof_id proof = CompactClause.id c

module IntSet = Set.Make(struct
  type t = int
  let compare i j = i - j
end)

(** Traverse the proof. Each proof node is traversed only once. *)
let traverse ?(traversed=ref IntSet.empty) proof k =
  (* set of already traversed proof nodes; queue of proof nodes
     yet to traverse *)
  let queue = Queue.create () in
  Queue.push proof queue;
  while not (Queue.is_empty queue) do
    let proof = Queue.take queue in
    if IntSet.mem (proof_id proof) !traversed then ()
    else begin
      traversed := IntSet.add (proof_id proof) !traversed;
      (* traverse premises first *)
      (match proof with
      | Axiom _ -> ()
      | Infer (_, _, l) ->
        List.iter (fun proof' -> Queue.push proof' queue) l);
        (* call [k] on the proof *)
        k proof;
    end
  done

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(** Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = ref IntSet.empty in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    let i = proof_id p in
    if IntSet.mem i !explored then () else begin
      explored := IntSet.add i !explored;
      match p with
      | Axiom _ -> depth := max d !depth
      | Infer (_, _, l) -> (* explore parents *)
        List.iter (fun p -> Queue.push (p, d+1) q) l
    end
  done;
  !depth

(** {2 Conversion to a graph of proofs} *)

let mk_graph () =
  PersistentGraph.empty
    ~hash:(fun p -> proof_id p)
    ~eq:(fun p1 p2 -> proof_id p1 = proof_id p2)
    10

(** Get a graph of the proof *)
let to_graph proof =
  let g = mk_graph () in
  traverse proof
    (fun p -> match p with
     | Axiom _ -> ()
     | Infer (_, rule, l) ->
       List.iter (fun p' -> PersistentGraph.add g p' rule p) l
    );
  g

let bij ~ord =
  let open Bij in
  let tbl = Hashtbl.create 15 in
  (* bijection for a step. [tbl] is used during parsing, for retrieving steps
      by their ID. *)
  let bij_step =
    let bij_axiom = triple (CompactClause.bij ~ord) string_ string_ in
    let bij_proof = triple (CompactClause.bij ~ord) string_ (list_ int_) in
    switch
      ~inject:(function
      | Axiom (c, file, name) -> 'a', BranchTo (bij_axiom, (c,file,name))
      | Infer (c, rule, l) -> 'p',
        BranchTo (bij_proof, (c, rule, List.map proof_id l)))
      ~extract:(function
      | 'a' -> BranchFrom (bij_axiom, (fun (c,file,name) ->
        let proof = mk_axiom c file name in
        Hashtbl.replace tbl (proof_id proof) proof;  (* save *)
        proof))
      | 'p' -> BranchFrom (bij_proof, (fun (c,rule,ids) ->
        let premises = List.map (fun i -> Hashtbl.find tbl i) ids in
        let proof = mk_infer c rule premises in
        Hashtbl.replace tbl (proof_id proof) proof;  (* save *)
        proof))
      | _ -> raise (DecodingError "expected proof step"))
  in
  map
    ~inject:(fun p -> Sequence.to_list (traverse p), p)
    ~extract:(fun (l,p) -> p)
    (pair (list_ bij_step) bij_step)

(** {2 IO} *)

let pp_debug buf proof =
  traverse proof
    (function
      | Axiom (c, f, s) ->
        Printf.bprintf buf "%a <--- axiom %s in %s\n" CompactClause.pp c s f
      | Infer (c, rule, premises) ->
        Printf.bprintf buf "%a <--- %s with\n" CompactClause.pp c rule;
        List.iter
          (fun premise -> Printf.bprintf buf "  %a\n" CompactClause.pp premise)
          (List.map proof_clause premises)
    )

let pp_tstp buf proof =
  traverse proof
    (function
      | Axiom (c, f, ax_name) ->
        let t = Literal.term_of_lits (CompactClause.lits c) in
        Printf.bprintf buf "fof(%d, axiom, %a, file('%s', %s)).\n"
          (CompactClause.id c) T.pp_tstp t f ax_name
      | Infer (c, name, premises) ->
        let t = T.close_forall (Literal.term_of_lits (CompactClause.lits c)) in
        let premises = List.map proof_id premises in
        let status = if name = "elim" || name = "to_cnf" then "esa" else "thm" in
        (* print the inference *)
        Printf.bprintf buf
          ("fof(%d, plain, %a, inference('%s', " ^^
           "[status(%s), theory(equality)], [%a])).\n")
          (CompactClause.id c) T.pp_tstp t name status
          (Util.pp_list ~sep:"," (fun b i -> Printf.bprintf b "%d" i)) premises
    )

(** Prints the proof according to the given input switch *)
let pp switch buf proof = match switch with
  | "none" -> Util.debug 1 "%% proof printing disabled"
  | "tstp" -> pp_tstp buf proof
  | "debug" -> pp_debug buf proof
  | _ -> failwith ("unknown proof-printing format: " ^ switch)

let print_vertex proof =
  let label = `Label (Util.sprintf "%a" Literal.pp_lits (proof_lits proof)) in
  let attributes = [`Shape "box"; `Style "filled"] in
  let attributes =
    if proof_lits proof = [||] then `Color "red" :: `Label "[]" :: attributes
    else if is_axiom proof then label :: `Color "yellow" :: attributes
    else label :: attributes in
  attributes
and print_edge v1 e v2 =
  [`Label e]

(** Add the proof to the given graph *)
let pp_dot ~name buf proof =
  let graph = to_graph proof in
  assert (PersistentGraph.is_dag graph);
  let fmt = Format.formatter_of_buffer buf in
  PersistentGraph.pp ~name ~print_vertex ~print_edge fmt graph;
  Format.pp_print_flush fmt ();
  ()

(** print to dot into a file *)
let pp_dot_file ?(name="proof") filename proof =
  (* print graph on file *)
  let out = open_out filename in
  try
    let buf = Buffer.create 1024 in
    pp_dot ~name buf proof;
    Util.debug 1 "%% print proof to %s" filename;
    (* write on the opened out channel *)
    Buffer.output_buffer out buf;
    close_out out
  with _ ->
    close_out out
