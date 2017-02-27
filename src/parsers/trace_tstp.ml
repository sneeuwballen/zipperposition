
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Trace of a TSTP prover} *)

open Logtk

type id = Ast_tptp.name

module T = STerm
module A = Ast_tptp
module Err = CCResult

type term = STerm.t
type form = STerm.t
type clause = term SLiteral.t list

type t =
  | Axiom of string * string (* filename, axiom name *)
  | Theory of string (* a theory used to do an inference *)
  | InferForm of form * step lazy_t
  | InferClause of clause * step lazy_t
and step = {
  id : id;
  rule : string;
  parents : t array;
  esa : bool;  (** Equisatisfiable step? *)
}

type proof = t

let equal p1 p2 = match p1, p2 with
  | Axiom (f1, n1), Axiom (f2, n2) -> f1 = f2 && n1 = n2
  | Theory s1, Theory s2 -> s1 = s2
  | InferForm (f1, lazy step1), InferForm(f2, lazy step2) ->
    step1.id = step2.id && T.equal f1 f2
  | InferClause (c1, lazy step1), InferClause(c2, lazy step2) ->
    begin try
        step1.id = step2.id && List.for_all2 (SLiteral.equal T.equal) c1 c2
      with Invalid_argument _ -> false
    end
  | _ -> false

let hash p = match p with
  | Axiom _
  | Theory _ -> Hashtbl.hash p
  | InferForm (_, lazy s)
  | InferClause (_, lazy s) -> Hashtbl.hash s.id

let compare p1 p2 = Pervasives.compare p1 p2  (* FIXME *)

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
  | InferForm (form, _) when T.equal form T.false_ -> true
  | InferClause([],_) -> true
  | InferClause(l,_) -> List.for_all SLiteral.is_false l
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
    let equal = equal
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

type 'a or_error = ('a, string) CCResult.t

let of_decls decls =
  let steps = Hashtbl.create 16 in (* maps names to steps *)
  let root = ref None in (* (one) root of proof *)
  (* find a proof name *)
  let find_step name =
    try
      Hashtbl.find steps name
    with Not_found ->
      failwith (CCFormat.sprintf "proof step %a not found in derivation" A.pp_name name)
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
              | _ -> failwith (CCFormat.sprintf "not a valid parent: %a" A.pp_general data))
           (Array.of_list parents))
      in
      let esa = status <> "thm" in
      `Parents (rule, esa, parents)
    | A.GNode ("file", (A.GString file :: A.GString name :: _)) ->
      let parents = Lazy.from_val [|(Axiom (file,name))|] in
      `Parents ("axiom", false, parents)
    | A.GNode ("trivial", _) ->
      `Parents ("trivial", false, Lazy.from_val [||])
    | A.GInt i ->
      let parent = find_step (A.NameInt i) in
      `Parents ("trivial", false, Lazy.from_val [|parent|])
    | A.GString s ->
      let parent = find_step (A.NameString s) in
      `Parents ("trivial", false, Lazy.from_val [|parent|])
    | _ ->
      Util.debugf 1 "not a valid proof step: %a" (fun k->k A.pp_general_debugf info);
      `NoIdea
  in
  (* what to do if a step is read *)
  let add_step id step = match step with
    | InferForm _
    | InferClause _ ->
      if is_proof_of_false step && !root = None
      then root := Some step;
      Util.debugf 3 "add step %a (root? %B)" (fun k->k A.pp_name id (is_proof_of_false step));
      Hashtbl.replace steps id step;
    | Axiom _
    | Theory _ -> ()
  in
  (* traverse declarations *)
  Sequence.iter
    begin fun decl -> match decl with
      | A.CNF (_name, _role, _c, info :: _) ->
        Util.debugf 3 "@[<2>convert step@ @[%a@]@]" (fun k->k (A.pp T.pp) decl);
        begin match read_info info with
          | `Proof
          | `NoIdea -> ()
          | `Parents (_rule, _esa, _parents) ->
            assert false
            (* FIXME
               let step = lazy {id=name; esa; rule; parents=Lazy.force parents} in
               let c = List.map SLiteral.of_form c in
               let p = InferClause (c, step) in
               add_step name p
            *)
        end
      | A.FOF(name, _role, f, info :: _)
      | A.TFF (name, _role, f, info :: _) ->
        Util.debugf 3 "convert step %a" (fun k->k (A.pp T.pp) decl);
        begin match read_info info with
          | `Proof
          | `NoIdea -> ()
          | `Parents (rule, esa, parents) ->
            let step = lazy{id=name; esa; rule; parents=Lazy.force parents} in
            let p = InferForm (f, step) in
            add_step name p
        end
      | A.TypeDecl _
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
    | None -> Err.fail "could not find the root of the TSTP proof"
    | Some p ->
      try
        (* force proofs to trigger errors here *)
        traverse p force;
        Err.return p
      with Failure msg -> Err.fail msg

let parse ?(recursive=true) filename =
  Err.(
    Util_tptp.parse_file ~recursive filename
    >>= fun decls ->
    Util.debugf 1 "@[<2>decls:@ @[<hv>%a@]@]"
      (fun k->k (Util.pp_seq ~sep:"" (A.pp T.pp)) decls);
    of_decls decls
  )

let _extract_axiom proof = match proof with
  | Axiom (f,n) -> f,n
  | _ -> assert false

let _pp_clause out c = match c with
  | [] -> T.pp out T.false_
  | _ ->
    Format.fprintf out "@[%a@]"
      (Util.pp_list ~sep:" | " (SLiteral.pp T.pp)) c

let _print_parent p = match p with
  | InferForm _
  | InferClause _ -> CCFormat.to_string A.pp_name (get_id  p)
  | Theory s -> CCFormat.sprintf "theory(%s)" s
  | Axiom (f,n) -> CCFormat.sprintf "file('%s', %s)" f n

(* pure fof? (no types but $i/$o) *)
let _form_is_fof _f = false (* FIXME *)

let pp_tstp out proof =
  traverse proof
    (fun p ->
       match p with
         | Axiom _ -> ()
         | Theory s -> CCFormat.fprintf out "theory(%s)" s
         | InferClause(c, lazy ({rule="axiom";_} as step)) when is_axiom step.parents.(0)->
           let id = get_id p in
           let f,n = _extract_axiom step.parents.(0) in
           Format.fprintf out "cnf(%a, axiom, (%a), file('%s', %s)).\n"
             A.pp_name id _pp_clause c f n
         | InferForm(f, lazy ({rule="axiom"; _} as step)) when is_axiom step.parents.(0)->
           let id = get_id p in
           let file,n = _extract_axiom step.parents.(0) in
           Format.fprintf out "tff(%a, axiom, %a, file('%s', %s)).\n"
             A.pp_name id T.pp f file n
         | InferForm(f, lazy step) ->
           let id = get_id p in
           let ids = Array.map _print_parent step.parents in
           let status = if step.esa then "esa" else "thm" in
           let kind = if _form_is_fof f then "fof" else "tff" in
           Format.fprintf out
             "%s(%a, plain, %a, inference('%s', [status(%s)], [%a])).\n"
             kind A.pp_name id T.pp (T.close_all Binder.Forall f) step.rule status
             (CCFormat.array CCFormat.string) ids
         | InferClause(c, lazy step) ->
           let id = get_id p in
           let ids = Array.map _print_parent step.parents in
           let status = if step.esa then "esa" else "thm" in
           Format.fprintf out
             "cnf(%a, plain, %a, inference('%s', [status(%s)], [%a])).\n"
             A.pp_name id _pp_clause c step.rule status
             (CCFormat.array CCFormat.string) ids
    )

let pp0 out proof = match proof with
  | Axiom (f,n) -> Format.fprintf out "axiom(%s, %s)" f n
  | Theory s -> Format.fprintf out "theory(%s)" s
  | InferClause (c, _) ->
    Format.fprintf out "proof for %a (id %a)" _pp_clause c A.pp_name (get_id proof)
  | InferForm (f, _) ->
    Format.fprintf out "proof for %a (id %a)" T.pp f A.pp_name (get_id proof)

let pp1 out proof = match proof with
  | Axiom (f,n) -> Format.fprintf out "axiom(%s, %s)" f n
  | Theory s -> Format.fprintf out "theory(%s)" s
  | InferClause (c, lazy step) ->
    Format.fprintf out "proof for %a (id %a) from\n  %a"
      _pp_clause c A.pp_name (get_id proof)
      CCFormat.(array ~sep:(return "@.  ") pp0) step.parents
  | InferForm (f, lazy step) ->
    Format.fprintf out "proof for %a (id %a) from\n %a"
      T.pp f A.pp_name (get_id proof)
      CCFormat.(array ~sep:(return "@.  ") pp0) step.parents

let pp = pp0
let to_string = CCFormat.to_string pp
