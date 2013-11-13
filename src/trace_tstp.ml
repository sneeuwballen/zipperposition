
(*
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

(** {6 Trace of a TSTP prover} *)

module T = FOTerm
module F = FOFormula
module S = Substs.FO
module A = Ast_tptp

type id = A.name

type t =
  | Axiom of string * string (* filename, axiom name *)
  | Theory of string (* a theory used to do an inference *)
  | InferForm of F.t * step lazy_t
  | InferClause of F.t list * step lazy_t
and step = {
  id : id;
  rule : string;
  parents : t array;
  esa : bool;  (** Equisatisfiable step? *)
}

type proof = t 

let rec eq p1 p2 = match p1, p2 with
  | Axiom (f1, n1), Axiom (f2, n2) -> f1 = f2 && n1 = n2
  | Theory s1, Theory s2 -> s1 = s2
  | InferForm (f1, lazy step1), InferForm(f2, lazy step2) ->
    F.eq f1 f2 && step1.id = step2.id
  | InferClause (c1, lazy step1), InferClause(c2, lazy step2) ->
    begin try
      List.for_all2 F.eq c1 c2 && step1.id = step2.id
    with Invalid_argument _ -> false
    end
  | _ -> false

let hash p = match p with
  | Axiom (f, n) -> Hash.hash_int2 (Hash.hash_string f) (Hash.hash_string n)
  | Theory s -> Hash.hash_string s
  | InferForm (f,_) -> F.hash f
  | InferClause (c, _) -> Hash.hash_list F.hash 42 c

let cmp p1 p2 = Pervasives.compare p1 p2  (* FIXME *)

(** {2 Constructors and utils} *)

let mk_f_axiom ~id f ~file ~name =
  let step = {id; rule="axiom"; parents = [| Axiom (file,name) |]; esa=false; } in
  InferForm (f, Lazy.from_val step)

let mk_c_axiom ~id c ~file ~name =
  let step = {id; rule="axiom"; parents = [| Axiom (file,name) |]; esa=false; } in 
  InferClause (c, Lazy.from_val step)

let mk_f_step ?(esa=false) ~id f ~rule parents =
  assert(rule <> "axiom");
  let step = {id; rule; parents=Array.of_list parents; esa;} in
  InferForm (f, Lazy.from_val step)

let mk_c_step ?(esa=false) ~id c ~rule parents =
  assert(rule <> "axiom");
  let step = {id; rule; parents=Array.of_list parents; esa;} in
  InferClause (c, Lazy.from_val step)

let is_axiom = function
  | Axiom _ -> true
  | _ -> false

let is_theory = function
  | Theory _ -> true
  | _ -> false

let is_step = function
  | InferClause _
  | InferForm _ -> true
  | Axiom _
  | Theory _ -> false

let is_proof_of_false = function
  | InferForm ({F.form=F.False}, _) -> true
  | InferClause(l,_) -> List.for_all (F.eq F.mk_false) l
  | _ -> false

let get_id = function
  | InferClause (_, lazy step)
  | InferForm (_, lazy step) -> step.id
  | Axiom (_, name) -> A.NameString name
  | Theory _ -> invalid_arg "Trace_tstp: Theory does not have an ID"

let force = function
  | InferForm (_, step)
  | InferClause (_, step) -> ignore (Lazy.force step)
  | Axiom _
  | Theory _ -> ()

(** {2 Proof traversal} *)

module StepTbl = Hashtbl.Make(struct
  type t = proof
  let equal = eq
  let hash = hash
end)

type proof_set = unit StepTbl.t

let is_dag proof =
  (* steps currently being explored *)
  let current = StepTbl.create 10 in
  (* steps totally explored *)
  let closed = StepTbl.create 10 in
  (* recursive DFS traversal *)
  let rec check_proof proof =
    if StepTbl.mem closed proof
      then ()  (* ok *)
    else if StepTbl.mem current proof
      then raise Exit  (* we followed a back link! *)
    else begin
      StepTbl.add current proof ();
      begin match proof with
      | InferClause (_, lazy step)
      | InferForm (_, lazy step) ->
        Array.iter check_proof step.parents
      | Axiom _
      | Theory _ -> ()
      end;
      (* proof is now totally explored *)
      StepTbl.remove current proof;
      StepTbl.add closed proof ();
    end
  in
  try
    check_proof proof;  (* check from root *)
    true
  with Exit ->
    false  (* loop detected *)

(** Traverse the proof. Each proof node is traversed only once. *)
let traverse ?(traversed=StepTbl.create 11) proof k =
  (* set of already traversed proof nodes; queue of proof nodes
     yet to traverse *)
  let queue = Queue.create () in
  Queue.push proof queue;
  while not (Queue.is_empty queue) do
    let proof = Queue.take queue in
    if StepTbl.mem traversed proof then ()
    else begin
      StepTbl.add traversed proof ();
      (* traverse premises first *)
      begin match proof with
      | Axiom _
      | Theory _ -> ()
      | InferForm (_, lazy step)
      | InferClause (_, lazy step) ->
        Array.iter (fun proof' -> Queue.push proof' queue) step.parents
      end;
      (* call [k] on the proof *)
      k proof;
    end
  done

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(** Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = StepTbl.create 11 in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    if StepTbl.mem explored proof then () else begin
      StepTbl.add explored proof ();
      match p with
      | Axiom _ -> depth := max d !depth
      | Theory _ -> ()
      | InferForm(_, lazy step)
      | InferClause (_, lazy step) ->
        (* explore parents *)
        Array.iter (fun p -> Queue.push (p, d+1) q) step.parents
    end
  done;
  !depth

let size proof = Sequence.length (to_seq proof)

(** {2 IO} *)

let of_decls ?(base=Signature.base) decls =
  let steps = Hashtbl.create 13 in (* maps names to steps *)
  let root = ref None in (* (one) root of proof *)
  let ctx = TypeInference.Ctx.of_signature base in
  (* find a proof name *)
  let find_step name =
    try
      Hashtbl.find steps name
    with Not_found ->
      failwith (Util.sprintf "proof step %a not found in derivation" A.pp_name name)
  in
  (* read information about the source of the clause/formula *)
  let read_info info = match info with
    | A.GList [A.GString ("'proof'" | "proof")] -> `Proof
    | A.GNode("inference", [A.GString rule;
                            A.GList [A.GNode ("status", [A.GString status])];
                            A.GList parents]) ->
      (* lazily lookup parent steps by their name in the derivation *)
      let parents = lazy
        (Array.map
          (fun data -> match data with
            | A.GInt i -> find_step (A.NameInt i)
            | A.GString s -> find_step (A.NameString s)
            | A.GNode ("theory", [A.GString th]) -> Theory th
            | _ -> failwith (Util.sprintf "not a valid parent: %a" A.pp_general data))
        (Array.of_list parents))
      in
      let esa = status <> "thm" in
      `Parents (rule, esa, parents)
    | A.GNode ("file", [A.GString file; A.GString name]) ->
      let parents = Lazy.from_val [|(Axiom (file,name))|] in
      `Parents ("axiom", false, parents)
    | _ ->
      Util.debug 1 "not a valid proof step: %a" A.pp_general_debug info;
      `NoIdea
  in
  (* what to do if a step is read *)
  let add_step id step = match step with
  | InferForm _
  | InferClause _ ->
    if is_proof_of_false step && !root = None
      then root := Some step;
    Util.debug 3 "add step %a (root? %B)" A.pp_name id (is_proof_of_false step);
    Hashtbl.replace steps id step;
  | Axiom _
  | Theory _ -> ()
  in
  (* traverse declarations *)
  Sequence.iter
    begin fun decl -> match decl with
    | A.CNF (name, role, c, info :: _) ->
      Util.debug 3 "convert step %a" A.pp_declaration decl;
      let c = TypeInference.FO.convert_clause ~ctx c in
      begin match read_info info with
      | `Proof
      | `NoIdea -> ()
      | `Parents (rule, esa, parents) ->
        let step = lazy {id=name; esa; rule; parents=Lazy.force parents} in
        let p = InferClause (c, step) in
        add_step name p
      end
    | A.FOF(name, role, f, info :: _)
    | A.TFF (name, role, f, info :: _) ->
      Util.debug 3 "convert step %a" A.pp_declaration decl;
      let f = TypeInference.FO.convert_form ~ctx f in
      begin match read_info info with
      | `Proof
      | `NoIdea -> ()
      | `Parents (rule, esa, parents) ->
        let step = lazy{id=name; esa; rule; parents=Lazy.force parents} in
        let p = InferForm (f, step) in
        add_step name p
      end
    | A.TypeDecl (_, s, ty) ->
      let ty = TypeConversion.of_basic ty in
      TypeInference.Ctx.declare ctx s ty
    | A.FOF _
    | A.CNF _
    | A.TFF _
    | A.THF _
    | A.Include _
    | A.IncludeOnly _
    | A.NewType _ -> ()
    end
    decls;
  match !root with
  | None -> None
  | Some p ->
    (* force proofs to trigger errors here *)
    traverse p force; 
    Some p

let parse ?(recursive=true) filename =
  let decls = Util_tptp.parse_file ~recursive filename in
  Util.debug 1 "decls:\n  %a" (Util.pp_seq ~sep:"\n  " A.pp_declaration) decls;
  of_decls decls

let _extract_axiom proof = match proof with
  | Axiom (f,n) -> f,n
  | _ -> assert false

let _pp_clause buf c = match c with
  | [] -> Buffer.add_string buf "$false"
  | _ -> Util.pp_list ~sep:" | "  F.pp_tstp buf c

let _print_parent p = match p with
  | InferForm _
  | InferClause _ -> Util.on_buffer A.pp_name (get_id  p)
  | Theory s -> Util.sprintf "theory(%s)" s
  | Axiom (f,n) -> Util.sprintf "file('%s', %s)" f n

let pp_tstp buf proof =
  traverse proof
    (fun p ->
      match p with
      | Axiom _ -> () 
      | Theory s -> Printf.bprintf buf "theory(%s)" s
      | InferClause(c, lazy ({rule="axiom"} as step)) when is_axiom step.parents.(0)->
        let id = get_id p in
        let f,n = _extract_axiom step.parents.(0) in
        Printf.bprintf buf "cnf(%a, axiom, (%a), file('%s', %s)).\n"
          A.pp_name id _pp_clause c f n
      | InferForm(f, lazy ({rule="axiom"} as step)) when is_axiom step.parents.(0)->
        let id = get_id p in
        let file,n = _extract_axiom step.parents.(0) in
        Printf.bprintf buf "tff(%a, axiom, %a, file('%s', %s)).\n"
          A.pp_name id F.pp_tstp f file n
      | InferForm(f, lazy step) ->
        let id = get_id p in
        let ids = Array.map _print_parent step.parents in
        let status = if step.esa then "esa" else "thm" in
        Printf.bprintf buf
          "tff(%a, plain, %a, inference('%s', [status(%s)], [%a])).\n"
          A.pp_name id F.pp_tstp (F.close_forall f) step.rule status
          (Util.pp_array Buffer.add_string) ids
      | InferClause(c, lazy step) ->
        let id = get_id p in
        let ids = Array.map _print_parent step.parents in
        let status = if step.esa then "esa" else "thm" in
        Printf.bprintf buf
          "cnf(%a, plain, %a, inference('%s', [status(%s)], [%a])).\n"
          A.pp_name id _pp_clause c step.rule status
          (Util.pp_array Buffer.add_string) ids
    )

let pp0 buf proof = match proof with
  | Axiom (f,n) -> Printf.bprintf buf "axiom(%s, %s)" f n
  | Theory s -> Printf.bprintf buf "theory(%s)" s
  | InferClause (c, _) ->
    Printf.bprintf buf "proof for %a (id %a)" _pp_clause c A.pp_name (get_id proof)
  | InferForm (f, _) ->
    Printf.bprintf buf "proof for %a (id %a)" F.pp_tstp f A.pp_name (get_id proof)

let pp1 buf proof = match proof with
  | Axiom (f,n) -> Printf.bprintf buf "axiom(%s, %s)" f n
  | Theory s -> Printf.bprintf buf "theory(%s)" s
  | InferClause (c, lazy step) ->
    Printf.bprintf buf "proof for %a (id %a) from\n  %a"
      _pp_clause c A.pp_name (get_id proof)
      (Util.pp_array ~sep:"\n  " pp0) step.parents
  | InferForm (f, lazy step) ->
    Printf.bprintf buf "proof for %a (id %a) from\n %a"
      F.pp_tstp f A.pp_name (get_id proof)
      (Util.pp_array ~sep:"\n  " pp0) step.parents

let fmt fmt proof = Format.pp_print_string fmt (Util.on_buffer pp0 proof)
