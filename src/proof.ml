
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
module F = Formula
module S = Substs
module CC = CompactClause

type t =
  | Axiom of string * string
  | InferForm of Formula.t * step
  | InferClause of CompactClause.t * step
and step = {
  rule : string;
  parents : t array;
  esa : bool;
}

type proof = t 

let rec eq p1 p2 = match p1, p2 with
  | Axiom (f1, n1), Axiom (f2, n2) -> f1 = f2 && n1 = n2
  | InferForm (f1, step1), InferForm(f2, step2) ->
    F.eq f1 f2 && step_eq step1 step2
  | InferClause (c1, step1), InferClause(c2, step2) ->
    CC.eq c1 c2 && step_eq step1 step2
  | _ -> false
and step_eq step1 step2 =
  step1.rule = step2.rule &&
  Array.length step1.parents = Array.length step2.parents &&
  Util.array_forall2 eq step1.parents step2.parents

let hash p = match p with
  | Axiom (f, n) -> Hash.hash_int2 (Hash.hash_string f) (Hash.hash_string n)
  | InferForm (f,_) -> F.hash f
  | InferClause (c, _) -> CC.hash c

let cmp p1 p2 = Pervasives.compare p1 p2

(** {2 Constructors and utils} *)

let mk_f_axiom f ~file ~name =
  InferForm (f, {rule="axiom"; parents = [| Axiom (file,name) |]; esa=false; })

let mk_c_axiom c ~file ~name =
  InferClause (c, {rule="axiom"; parents = [| Axiom (file,name) |]; esa=false; })

let mk_f_step ?(esa=false) f ~rule parents =
  assert(rule <> "axiom");
  InferForm (f, {rule; parents=Array.of_list parents; esa;})

let mk_c_step ?(esa=false) c ~rule parents =
  assert(rule <> "axiom");
  InferClause (c, {rule; parents=Array.of_list parents; esa;})

let adapt_c p c =
  match p with
  | Axiom _ -> p
  | InferClause(_,step)
  | InferForm (_,step) -> InferClause(c,step)

let adapt_f p f =
  match p with
  | Axiom _ -> p
  | InferClause(_,step)
  | InferForm (_,step) -> InferForm(f,step)

let is_axiom = function
  | Axiom _ -> true
  | _ -> false

let is_proof_of_false = function
  | InferForm ({F.form=F.False}, _) -> true
  | InferClause(c,_) when CC.is_empty c -> true
  | _ -> false

(** {2 Proof traversal} *)

module ProofTbl = Hashtbl.Make(struct
  type t = proof
  let equal = eq
  let hash = hash
end)

type proof_set = unit ProofTbl.t

type proof_name = int ProofTbl.t

(** Traverse the proof. Each proof node is traversed only once. *)
let traverse ?(traversed=ProofTbl.create 11) proof k =
  (* set of already traversed proof nodes; queue of proof nodes
     yet to traverse *)
  let queue = Queue.create () in
  Queue.push proof queue;
  while not (Queue.is_empty queue) do
    let proof = Queue.take queue in
    if ProofTbl.mem traversed proof then ()
    else begin
      ProofTbl.add traversed proof ();
      (* traverse premises first *)
      begin match proof with
      | Axiom _ -> ()
      | InferForm (_, step)
      | InferClause (_, step) ->
        Array.iter (fun proof' -> Queue.push proof' queue) step.parents
      end;
      (* call [k] on the proof *)
      k proof;
    end
  done

let get_name ~namespace p =
  try
    ProofTbl.find namespace p
  with Not_found ->
    let n = ProofTbl.length namespace in
    ProofTbl.add namespace p n;
    n

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(** Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = ProofTbl.create 11 in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    if ProofTbl.mem explored proof then () else begin
      ProofTbl.add explored proof ();
      match p with
      | Axiom _ -> depth := max d !depth
      | InferForm(_, step)
      | InferClause (_, step) ->
        (* explore parents *)
        Array.iter (fun p -> Queue.push (p, d+1) q) step.parents
    end
  done;
  !depth

(* physically share subproofs, to save memory *)
let share t =
  Util.debug 0 "Proof.share: not implemented";  (* TODO *)
  t

(** {2 Conversion to a graph of proofs} *)

let mk_graph () = PersistentGraph.empty ~hash ~eq 10

(** Get a graph of the proof *)
let to_graph proof =
  let g = mk_graph () in
  traverse proof
    (fun p -> match p with
      | Axiom _ -> ()
      | InferForm (_, step)
      | InferClause (_, step) ->
        Array.iter (fun p' -> PersistentGraph.add g p' step.rule p) step.parents);
  g

let bij ~ord =
  let open Bij in
  fix
    (fun bij_ ->
      let bij_step = map
        ~inject:(fun step -> step.rule, step.parents, step.esa)
        ~extract:(fun (rule,parents,esa) -> {rule;parents;esa;})
          (triple string_ (array_ (bij_ ())) bool_) in
      let bij_axiom = pair string_ string_
      and bij_form = pair F.bij bij_step
      and bij_clause = pair (CC.bij ~ord) bij_step
      in switch
          ~inject:(function
            | Axiom (f,n) -> 'a', BranchTo(bij_axiom, (f,n))
            | InferForm(f,step) -> 'f', BranchTo(bij_form, (f,step))
            | InferClause(c,step) -> 'c', BranchTo(bij_clause, (c,step)))
          ~extract:(function
            | 'a' -> BranchFrom(bij_axiom, (fun (f,n) -> Axiom(f,n)))
            | 'f' -> BranchFrom(bij_form, (fun(f,step) -> InferForm(f,step)))
            | 'c' -> BranchFrom(bij_clause, (fun(c,step) -> InferClause(c,step)))
            | c -> raise (DecodingError "expected proof step"))
    )

(** {2 IO} *)

let pp_notrec buf proof =
  match proof with
  | Axiom(f,n) -> Printf.bprintf buf "axiom \"%s\" in %s" f n
  | InferForm (f, _) -> F.pp buf f
  | InferClause(c, _) -> CC.pp buf c

let pp_debug buf proof =
  traverse proof
    (function
      | Axiom (f,n) -> ()
      | InferForm (f, step) ->
        Printf.bprintf buf "%a <--- %s with\n" F.pp f step.rule;
        Array.iter
          (fun premise -> Printf.bprintf buf "  %a\n" pp_notrec premise)
          step.parents
      | InferClause (c, step) ->
        Printf.bprintf buf "%a <--- %s with\n" CC.pp c step.rule;
        Array.iter
          (fun premise -> Printf.bprintf buf "  %a\n" pp_notrec premise)
          step.parents
    )

let _extract_axiom proof = match proof with
  | Axiom (f,n) -> f,n
  | _ -> assert false

let _pp_parent_names buf names =
  Util.pp_array ~sep:"," (fun buf -> Printf.bprintf buf "%i") buf names

let pp_tstp buf proof =
  let namespace = ProofTbl.create 5 in
  traverse proof
    (fun p ->
      let name = get_name ~namespace p in
      match p with
      | Axiom _ -> failwith "cannot print an axiom step as TSTP"
      | InferClause(c, ({rule="axiom"} as step)) when is_axiom step.parents.(0)->
        let f,n = _extract_axiom step.parents.(0) in
        Printf.bprintf buf "cnf(%d, axiom, %a, file('%s', %s)).\n"
          name CC.pp_tstp c f n
      | InferForm(f, ({rule="axiom"} as step)) when is_axiom step.parents.(0)->
        let file,n = _extract_axiom step.parents.(0) in
        Printf.bprintf buf "tff(%d, axiom, %a, file('%s', %s)).\n"
          name F.pp_tstp f file n
      | InferForm(f, step) ->
        let names = Array.map (get_name ~namespace) step.parents in
        let status = if step.esa then "esa" else "thm" in
        Printf.bprintf buf
          "tff(%d, axiom, %a, inference('%s', [status(%s),theory(equality)], [%a])).\n"
          name F.pp_tstp (F.close_forall f) step.rule status _pp_parent_names names
      | InferClause(c, step) ->
        let names = Array.map (get_name ~namespace) step.parents in
        let status = if step.esa then "esa" else "thm" in
        Printf.bprintf buf
          "cnf(%d, axiom, %a, inference('%s', [status(%s),theory(equality)], [%a])).\n"
          name CC.pp_tstp c step.rule status _pp_parent_names names
    )

(** Prints the proof according to the given input switch *)
let pp switch buf proof = match switch with
  | "none" -> Util.debug 1 "proof printing disabled"
  | "tstp" -> pp_tstp buf proof
  | "debug" -> pp_debug buf proof
  | _ -> failwith ("unknown proof-printing format: " ^ switch)

let print_vertex proof =
  let label = `Label (Util.on_buffer pp_notrec proof) in
  let attributes = [`Shape "box"; `Style "filled"] in
  let attributes =
    if is_proof_of_false proof then `Color "red" :: `Label "[]" :: attributes
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
    Util.debug 1 "print proof to %s" filename;
    (* write on the opened out channel *)
    Buffer.output_buffer out buf;
    close_out out
  with _ ->
    close_out out
