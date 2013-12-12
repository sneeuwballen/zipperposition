
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

module F = FOFormula
module CC = CompactClause

(** Classification of proof steps *)
type step_kind =
  | Inference of string
  | Simplification of string
  | Esa of string
  | File of string * string * string
  | Trivial  (* trivial, or trivial within theories *)

type step_result =
  | Form of FOFormula.t
  | Clause of CompactClause.t

type t = {
  result : step_result;       (** conclusion of the step *)
  kind : step_kind;           (** kind of step *)
  parents : t array;          (** parent proof steps *)
  theories : string list;     (** theories used for the proof step *)
  additional_info : string list;   (** additional info, prover-dependent *)
}

type proof = t 

let eq p1 p2 =
  p1.kind = p2.kind &&
  p1.additional_info = p2.additional_info &&
  p1.theories = p2.theories &&
  match p1.result, p2.result with
  | Form f1, Form f2 -> F.eq f1 f2
  | Clause c1, Clause c2 -> CC.eq c1 c2
  | _ -> false

let hash p =
  Hash.combine
    (Hashtbl.hash p.kind)
    (match p.result with
      | Form f -> F.hash f
      | Clause c -> CC.hash c)

let cmp p1 p2 =
  let c = Pervasives.compare p1.kind p2.kind in
  if c = 0
    then match p1.result, p2.result with
      | Form f1, Form f2 -> F.compare f1 f2
      | Clause c1, Clause c2 -> CC.cmp c1 c2
      | Form _, Clause _ -> 1
      | Clause _, Form _ -> ~-1
    else c

(** {2 Constructors and utils} *)

let mk_f_trivial ?(info=[]) ?(theories=[]) f =
  { result=Form f; kind=Trivial; theories;
    parents = [| |]; additional_info=info; }

let mk_f_file ?(info=[]) ?(theories=[]) ~role ~file ~name f =
  { result=Form f; kind=File(role,file,name); theories;
    parents = [| |]; additional_info=info; }

let mk_f_inference ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; theories; kind=Inference(rule);
    parents = Array.of_list parents; additional_info=info; }

let mk_f_simp ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; kind=Simplification(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_f_esa ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; kind=Esa(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_c_trivial ?(info=[]) ?(theories=[]) c =
  { result=Clause c; kind=Trivial; theories;
    parents = [| |]; additional_info=info; }

let mk_c_file ?(info=[]) ?(theories=[]) ~role ~file ~name c =
  { result=Clause c; kind=File(role,file,name); theories;
    parents = [| |]; additional_info=info; }

let mk_c_inference ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; theories; kind=Inference(rule);
    parents = Array.of_list parents; additional_info=info; }

let mk_c_simp ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; kind=Simplification(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_c_esa ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; kind=Esa(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let adapt_c p c =
  { p with result=Clause c; }

let adapt_f p f =
  { p with result=Form f; }

let is_file = function
  | {kind=File _} -> true
  | _ -> false

let is_axiom = function
  | {kind=File("axiom", _, _)} -> true
  | _ -> false

let is_proof_of_false p =
  match p.result with
  | Form {F.form=F.False} -> true
  | Clause c when CC.is_empty c -> true
  | _ -> false

let rule p = match p.kind with
  | Trivial
  | File _ -> None
  | Esa rule
  | Simplification rule
  | Inference rule -> Some rule

let role p = match p.kind with
  | Trivial
  | Inference _
  | Esa _
  | Simplification _ -> "plain"
  | File(role,_,_) -> role

module Theories = struct
  let eq = ["equality"]
  let arith = ["equality"; "arith"]
end

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
      Array.iter (fun proof' -> Queue.push proof' queue) proof.parents;
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
      begin match p.kind with
      | File _ | Trivial -> depth := max d !depth
      | Inference _ | Esa _ | Simplification _ -> ()
      end;
      (* explore parents *)
      Array.iter (fun p -> Queue.push (p, d+1) q) p.parents
    end
  done;
  !depth

(* physically share subproofs, to save memory *)
let share t = failwith "Proof.share: not implemented"  (* TODO *)

(** {2 Conversion to a graph of proofs} *)

(** Get a graph of the proof *)
let as_graph =
  let f p =
    match rule p with
    | None -> LazyGraph.Node(p, p, Sequence.empty)
    | Some rule ->
      let parents = Sequence.of_array p.parents in
      let parents = Sequence.map (fun p' -> (rule, p')) parents in
      LazyGraph.Node (p, p, parents)
  in
  LazyGraph.make ~eq:eq ~hash:hash f

let bij = Bij.map ~inject:(fun _ -> assert false) ~extract:(fun _ -> assert false) Bij.unit_
(* FIXME
  Bij.(fix
    (fun bij_ ->
      let bij_step = lazy (map
        ~inject:(fun step -> step.rule, step.parents, step.esa, step.theories)
        ~extract:(fun (rule,parents,esa,theories) -> {rule;parents;esa;theories;})
          (quad string_ (array_ (Lazy.force bij_)) bool_ (list_ string_))) in
      let bij_axiom = pair string_ string_
      and bij_form = lazy (pair F.bij (Lazy.force bij_step))
      and bij_clause = lazy (pair CC.bij (Lazy.force bij_step))
      in switch
          ~inject:(function
            | Axiom (f,n) -> "axiom", BranchTo(bij_axiom, (f,n))
            | InferForm(f,step) -> "form", BranchTo(Lazy.force bij_form, (f,step))
            | InferClause(c,step) -> "clause", BranchTo(Lazy.force bij_clause, (c,step)))
          ~extract:(function
            | "axiom" -> BranchFrom(bij_axiom, (fun (f,n) -> Axiom(f,n)))
            | "form" -> BranchFrom(Lazy.force bij_form, (fun(f,step) -> InferForm(f,step)))
            | "clause" -> BranchFrom(Lazy.force bij_clause, (fun(c,step) -> InferClause(c,step)))
            | c -> raise (DecodingError "expected proof step"))
    ))
*)

(** {2 IO} *)

let pp_kind_tstp buf k = match k with
  | File (role,file,name) ->
    Printf.bprintf buf "file('%s', '%s')" file name
  | Inference rule ->
    Printf.bprintf buf "inference(%s, [status(thm)])" rule
  | Simplification rule ->
    Printf.bprintf buf "inference(%s, [status(thm)])" rule
  | Esa rule ->
    Printf.bprintf buf "inference(%s, [status(esa)])" rule
  | Trivial ->
    Printf.bprintf buf "trivial([status(thm)])"

let pp_kind buf k = match k with
  | File (role,file,name) ->
    Printf.bprintf buf "%s '%s' in '%s'" role name file
  | Inference rule ->
    Printf.bprintf buf "inf %s" rule
  | Simplification rule ->
    Printf.bprintf buf "simp %s" rule
  | Esa rule ->
    Printf.bprintf buf "esa %s" rule
  | Trivial -> Buffer.add_string buf "trivial"

let pp_result buf = function
  | Form f -> F.pp buf f
  | Clause c -> CC.pp buf c

let pp_result_of buf proof = pp_result buf proof.result

let pp_notrec buf p =
  Printf.bprintf buf "%a <-- %a [%a]"
    pp_result_of p pp_kind p.kind
    (Util.pp_array pp_result_of) p.parents

let fmt fmt proof =
  Format.pp_print_string fmt (Util.on_buffer pp_notrec proof)

let pp_debug buf proof =
  traverse proof
    begin fun p -> match p.kind with
      | File(role,file,name) ->
        Printf.bprintf buf "%a <--- %a, theories [%a]\n"
          pp_result p.result pp_kind p.kind
          (Util.pp_list Buffer.add_string) p.theories;
      | Trivial ->
        Printf.bprintf buf "%a <--- trivial, theories [%a]\n"
          pp_result p.result (Util.pp_list Buffer.add_string) p.theories;
      | Inference _
      | Simplification _
      | Esa _ ->
        Printf.bprintf buf "%a <--- %a, theories [%a] with\n"
          pp_result p.result pp_kind p.kind
          (Util.pp_list Buffer.add_string) p.theories;
        Array.iter
          (fun premise -> Printf.bprintf buf "    %a\n" pp_result premise.result)
          p.parents
    end

let _pp_parent buf = function
  | `Name i -> Printf.bprintf buf "%d" i
  | `Theory s -> Printf.bprintf buf "theory(%s)" s

let _pp_kind_tstp buf (k,parents) = match k with
  | Trivial ->
    Printf.bprintf buf "trivial([%a])"
      (Util.pp_list _pp_parent) parents
  | File (role,file,name) ->
    Printf.bprintf buf "file('%s', '%s', [%a])"
      file name (Util.pp_list _pp_parent) parents
  | Inference rule ->
    Printf.bprintf buf "inference('%s', [status(thm)], [%a])"
      rule (Util.pp_list _pp_parent) parents
  | Simplification rule ->
    Printf.bprintf buf "inference('%s', [status(thm)], [%a])"
      rule (Util.pp_list _pp_parent) parents
  | Esa rule ->
    Printf.bprintf buf "inference('%s', [status(esa)], [%a])"
      rule (Util.pp_list _pp_parent) parents

let pp_tstp buf proof =
  let namespace = ProofTbl.create 5 in
  traverse proof
    begin fun p ->
      let name = get_name ~namespace p in
      let parents =
        List.map (fun p -> `Name (get_name namespace p)) (Array.to_list p.parents) @
        List.map (fun s -> `Theory s) p.theories
      in
      match p.result with
      | Form f ->
        Printf.bprintf buf "tff(%d, %s, %a, %a).\n"
          name (role p) F.pp_tstp f _pp_kind_tstp (p.kind,parents)
      | Clause c ->
        Printf.bprintf buf "cnf(%d, %s, %a, %a).\n"
          name (role p) CC.pp_tstp c _pp_kind_tstp (p.kind,parents)
    end

(** Prints the proof according to the given input switch *)
let pp switch buf proof = match switch with
  | "none" -> Util.debug 1 "proof printing disabled"
  | "tstp" -> pp_tstp buf proof
  | "debug" -> pp_debug buf proof
  | _ -> failwith ("unknown proof-printing format: " ^ switch)

let as_dot_graph =
  let label proof = `Label (Util.on_buffer pp_notrec proof) in
  let attributes = [`Shape "box"; `Style "filled"] in
  LazyGraph.map
    ~vertices:(fun p ->
      if is_proof_of_false p then `Color "red" :: `Label "[]" :: attributes
      else if is_file p then label p :: `Color "yellow" :: attributes
      else label p :: attributes)
    ~edges:(fun e -> [`Label e])
    as_graph

let pp_dot_seq ~name buf seq =
  let fmt = Format.formatter_of_buffer buf in
  Sequence.iter (fun proof ->
    if not (LazyGraph.is_dag as_graph proof) then begin
      (* output warning, cyclic proof *)
      let cycle = LazyGraph.find_cycle as_graph proof in
      let cycle = List.map (fun (v,_,_) -> v) cycle in
      let pp_squared buf pf = Printf.bprintf buf "[%a]" pp_notrec pf in
      Util.debug 0 "warning: proof is not a DAG (cycle %a)"
        (Util.pp_list pp_squared) cycle;
      end)
    seq;
  LazyGraph.Dot.pp ~name as_dot_graph fmt seq;
  Format.pp_print_flush fmt ();
  ()

(** Add the proof to the given graph *)
let pp_dot ~name buf proof =
  pp_dot_seq ~name buf (Sequence.singleton proof)

(** print to dot into a file *)
let pp_dot_seq_file ?(name="proof") filename seq =
  (* print graph on file *)
  let out = open_out filename in
  try
    Util.debug 1 "print proof graph to %s" filename;
    Util.fprintf out "%a\n" (pp_dot_seq ~name) seq;
    flush out;
    close_out out
  with e ->
    Util.debug 1 "error: %s" (Printexc.to_string e);
    close_out out

let pp_dot_file ?name filename proof =
  pp_dot_seq_file ?name filename (Sequence.singleton proof)
