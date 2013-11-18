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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBBTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BBT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBBTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BBT NOT LIMITED TO, PROCUREMENT OF SUBSTITBTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OBT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Knowledge base} *)

open Logtk

module A = Ast_theory
module T = HOTerm
module Sym = Basic.Sym
module BT = Basic.HO
module F = FOFormula
module MRT = MetaReasoner.Translate

(** {2 Basic knowledge} *)

type lemma =
  | Lemma of MetaPattern.t * T.t list * premise list
and axiom =
  | Axiom of string * T.t list * MetaPattern.t * T.t list
and theory =
  | Theory of string * T.t list * premise list
and premise =
  | IfAxiom of string * T.t list
  | IfTheory of string * T.t list
  | IfPattern of MetaPattern.t * T.t list
and clause =
  | Clause of raw_lit * raw_lit list
and raw_lit = string * string list

(* XXX not very efficient, may we need a lazy {! Util.lexicograph_combine}? *)
let rec compare_lemma l1 l2 = match l1, l2 with
  | Lemma (p1, args1, l1), Lemma (p2, args2, l2) ->
    Util.lexicograph_combine
      [MetaPattern.compare p1 p2; Util.lexicograph T.compare args1 args2; compare_premises l1 l2]
and compare_axiom a1 a2 = match a1, a2 with
  | Axiom (s1, left1, p1, right1), Axiom (s2, left2, p2, right2) ->
    Util.lexicograph_combine
      [String.compare s1 s2; Util.lexicograph T.compare left1 left2;
        MetaPattern.compare p1 p2; Util.lexicograph T.compare right1 right2]
and compare_theory t1 t2 = match t1, t2 with
  | Theory (s1, args1, l1), Theory (s2, args2, l2) ->
    Util.lexicograph_combine
      [String.compare s1 s2; Util.lexicograph T.compare args1 args2;
        compare_premises l1 l2]
and compare_premise p1 p2 = match p1, p2 with
  | IfTheory (s1, l1), IfTheory (s2, l2)
  | IfAxiom (s1, l1), IfAxiom (s2, l2) ->
    Util.lexicograph_combine [String.compare s1 s2; Util.lexicograph T.compare l1 l2]
  | IfPattern (p1, args1), IfPattern (p2, args2) ->
    Util.lexicograph_combine [MetaPattern.compare p1 p2; Util.lexicograph T.compare args1 args2]
  | _, _ -> _premise_to_int p1 - _premise_to_int p2
and _premise_to_int = function
  | IfAxiom _ -> 0
  | IfPattern _ -> 1
  | IfTheory _ -> 2
and compare_premises l1 l2 =
  Util.lexicograph compare_premise l1 l2
and compare_clause = Pervasives.compare

module AxiomMap = MultiMap.Make(String)(struct type t = axiom let compare = compare_axiom end)
module LemmaSet = Sequence.Set.Make(struct type t = lemma let compare = compare_lemma end)
module TheoryMap = MultiMap.Make(String)(struct type t = theory let compare = compare_theory end)
module ClauseSet = Sequence.Set.Make(struct type t = clause let compare = compare_clause end)

let name_of_axiom = function
  | Axiom (s, _, _, _) -> s

let name_of_theory = function
  | Theory (s, _, _) -> s

(** {2 Knowledge base} *)

type t = {
  lemmas : LemmaSet.t;            (* set of lemmas *)
  axioms : AxiomMap.t;            (* axioms, by name *)
  theories : TheoryMap.t;         (* theories, by name *)
  clauses : ClauseSet.t;          (* set of raw clauses *)
} (* KB *)

let eq t1 t2 =
  LemmaSet.equal t1.lemmas t2.lemmas &&
  AxiomMap.equal t1.axioms t2.axioms &&
  TheoryMap.equal t1.theories t2.theories &&
  ClauseSet.equal t1.clauses t2.clauses

let empty =
  { lemmas = LemmaSet.empty;
    axioms = AxiomMap.empty;
    theories = TheoryMap.empty;
    clauses = ClauseSet.empty;
  }

let add_lemma kb l =
  { kb with lemmas = LemmaSet.add l kb.lemmas }

let add_axiom kb a =
  let name = name_of_axiom a in
  assert (not (AxiomMap.mem kb.axioms name));
  { kb with axioms = AxiomMap.add kb.axioms name a }

let add_theory kb t =
  let name = name_of_theory t in
  { kb with theories = TheoryMap.add kb.theories name t }

let add_clause kb c =
  { kb with clauses = ClauseSet.add c kb.clauses }

let get_axiom kb s = AxiomMap.find kb.axioms s

let get_theory kb s = TheoryMap.find kb.theories s

let all_patterns kb =
  let l = ref [] in
  let add_pattern p = l := p :: !l in
  let rec iter_premises = function
    | [] -> ()
    | (IfAxiom _ | IfTheory _) :: premises -> iter_premises premises
    | (IfPattern (p, _)) :: premises -> add_pattern p; iter_premises premises
  in
  AxiomMap.iter kb.axioms (fun _ (Axiom (_, _, p, _)) -> add_pattern p);
  TheoryMap.iter kb.theories (fun _ (Theory (_, _, premises)) -> iter_premises premises);
  LemmaSet.iter (fun (Lemma (_, _, premises)) -> iter_premises premises) kb.lemmas;
  !l

let union t1 t2 =
  { lemmas = LemmaSet.union t1.lemmas t2.lemmas;
    axioms = AxiomMap.union t1.axioms t2.axioms;
    theories = TheoryMap.union t1.theories t2.theories;
    clauses = ClauseSet.union t1.clauses t2.clauses;
  }

let diff t1 t2 =
  { lemmas = LemmaSet.diff t1.lemmas t2.lemmas;
    axioms = AxiomMap.diff t1.axioms t2.axioms;
    theories = TheoryMap.diff t1.theories t2.theories;
    clauses = ClauseSet.diff t1.clauses t2.clauses;
  }

let to_seq kb =
  ( LemmaSet.to_seq kb.lemmas
  , AxiomMap.values kb.axioms
  , TheoryMap.values kb.theories
  , ClauseSet.to_seq kb.clauses
  )

let of_seq (lemmas, axioms, theories, clauses) =
  let kb = empty in
  let kb = Sequence.fold add_lemma kb lemmas in
  let kb = Sequence.fold add_axiom kb axioms in
  let kb = Sequence.fold add_theory kb theories in
  let kb = Sequence.fold add_clause kb clauses in
  kb

let pp_premise buf p = match p with
| IfAxiom (s, []) -> Printf.bprintf buf "axiom %s" s
| IfAxiom (s, args) -> Printf.bprintf buf "axiom %s(%a)" s (Util.pp_list T.pp) args
| IfTheory (s, []) -> Printf.bprintf buf "theory %s" s
| IfTheory (s, args) -> Printf.bprintf buf "theory %s(%a)" s (Util.pp_list T.pp) args
| IfPattern (p, args) -> MetaPattern.pp_apply buf (p, args)
let pp_premises = Util.pp_list pp_premise
let pp_lemma buf = function
  | Lemma (p, args, premises) ->
    Printf.bprintf buf "lemma(%a if %a)" MetaPattern.pp_apply (p,args) pp_premises premises
and pp_axiom buf = function
  | Axiom (s, left, p, right) ->
    Printf.bprintf buf "axiom(%s(%a) is %a)" s
      (Util.pp_list T.pp) left MetaPattern.pp_apply (p,right)
and pp_theory buf = function
  | Theory (s, args, premises) -> Printf.bprintf buf "theory(%s(%a) if %a)"
    s (Util.pp_list T.pp) args pp_premises premises
and pp_lit buf lit = match lit with
  | h, [] -> Buffer.add_string buf h
  | h, l -> Printf.bprintf buf "%s(%a)" h (Util.pp_list Buffer.add_string) l

let pp_clause buf = function
  | Clause (lit, []) -> pp_lit buf lit; Buffer.add_char buf '.'
  | Clause (lit, body) ->
    Printf.bprintf buf "%a :- %a." pp_lit lit (Util.pp_list pp_lit) body

let pp buf kb =
  Buffer.add_string buf "KB {\n";
  AxiomMap.iter kb.axioms (fun _ a -> Printf.bprintf buf "  %a\n" pp_axiom a);
  TheoryMap.iter kb.theories (fun _ t -> Printf.bprintf buf "  %a\n" pp_theory t);
  LemmaSet.iter (Printf.bprintf buf "  %a\n" pp_lemma) kb.lemmas;
  ClauseSet.iter (Printf.bprintf buf "  %a\n" pp_clause) kb.clauses;
  Buffer.add_string buf "}\n";
  ()

let fmt fmt kb =
  let s = Util.sprintf "%a" pp kb in
  Format.pp_print_string fmt s

let bij_premise =
  let open Bij in
  (* Helpers *)
  let s_args = pair string_ (list_ T.bij) in
  let p_args = pair MetaPattern.bij (list_ T.bij) in
  switch
    ~inject:(function
      | IfAxiom (s, args) -> "if_axiom", BranchTo (s_args, (s, args))
      | IfTheory (s, args) -> "if_theory", BranchTo (s_args, (s, args))
      | IfPattern (p, args) -> "if_pattern", BranchTo (p_args, (p, args)))
    ~extract:(function
      | "if_axiom" -> BranchFrom (s_args, fun (s, args) -> IfAxiom (s, args))
      | "if_theory" -> BranchFrom (s_args, fun (s, args) -> IfTheory (s, args))
      | "if_pattern" -> BranchFrom (p_args, fun (p, args) -> IfPattern (p, args))
      | _ -> raise (DecodingError "expected premise"))

let bij_lemma =
  Bij.(map
    ~inject:(fun (Lemma(p,args,premises)) -> p,args,premises)
    ~extract:(fun (p,args,premises) -> Lemma(p,args,premises))
    (triple MetaPattern.bij (list_ T.bij) (list_ bij_premise)))

let bij_axiom =
  Bij.(map
    ~inject:(fun (Axiom(s,args,p,pargs)) -> s,args,p,pargs)
    ~extract:(fun (s,args,p,pargs) -> Axiom(s,args,p,pargs))
    (quad string_ (list_ T.bij) MetaPattern.bij (list_ T.bij)))

let bij_theory =
  Bij.(map
    ~inject:(fun (Theory (s,args,premises)) -> (s,args,premises))
    ~extract:(fun (s,args,premises) -> Theory(s,args,premises))
    (triple string_ (list_ T.bij) (list_ bij_premise)))

let bij_lit =
  Bij.(pair string_ (list_ string_))

let bij_clause =
  Bij.(map
    ~inject:(fun (Clause (h,b)) -> h,b)
    ~extract:(fun (h,b) -> Clause(h,b))
    (pair bij_lit (list_ bij_lit)))

let bij =
  let open Bij in
  let bij_kb = quad (list_ bij_lemma) (list_ bij_axiom)
    (list_ bij_theory) (list_ bij_clause) in
  map
    ~inject:(fun kb ->
      let lemmas, axioms, theories, clauses = to_seq kb in
      Sequence.to_rev_list lemmas,
      Sequence.to_rev_list axioms,
      Sequence.to_rev_list theories,
      Sequence.to_rev_list clauses)
    ~extract:(fun (lemmas, axioms, theories, clauses) ->
      of_seq (Sequence.of_list lemmas,
              Sequence.of_list axioms,
              Sequence.of_list theories,
              Sequence.of_list clauses))
    bij_kb

(** {2 MetaReasoner} *)

type found_lemma =
  | NewLemma of F.t * MetaReasoner.Logic.literal
    (** formula + explanation *)

and found_theory =
  | NewTheory of string * T.t list * MetaReasoner.Logic.literal

and found_axiom =
  | NewAxiom of string * T.t list

let mapping_lemma =
  MetaPattern.mapping
and mapping_theory =
  let open MRT in
  pair str (list_ term)
and mapping_axiom =
  let open MRT in
  pair str (list_ term)
  
let encode_premise p =
  let open MRT in
  match p with
  | IfAxiom (s, terms) ->
    let mapping = pair str (list_ term) in
    encode mapping "axiom" (s, terms)
  | IfTheory (s, terms) ->
    let mapping = pair str (list_ term) in
    encode mapping "theory" (s, terms)
  | IfPattern (p, args) -> encode MetaPattern.mapping "pattern" (p, args)

(* clause -> proper datalog clause *)
let _compile_clause (Clause (head,body)) =
  let open MetaReasoner.Logic in
  let h = Hashtbl.create 5 in
  let find_var name =
    try Hashtbl.find h name
    with Not_found ->
      let n = Hashtbl.length h in
      let v = mk_var n in
      Hashtbl.add h name v;
      v
  in
  let compile_lit (s, args) =
    let s = MetaReasoner.DSName s in
    let args = List.map
      (fun arg ->
        assert (arg <> "");
        if arg.[0] = 'L'
          then try
            let i = int_of_string (String.sub arg 1 (String.length arg-1)) in
            mk_const (MetaReasoner.DSSTartList i)
          with Failure _ ->
            find_var arg 
        else if Char.uppercase arg.[0] = arg.[0]
          then find_var arg
          else mk_const (MetaReasoner.DSName arg))
      args
    in
    mk_literal s args
  in
  mk_clause (compile_lit head) (List.map compile_lit body)

let add_axioms reasoner axioms =
  let open MRT in
  AxiomMap.iter axioms
    (fun _ (Axiom (s, left, p, right) as a) ->
      Util.debug 4 "add %a to KB" pp_axiom a;
      let concl = encode mapping_axiom "axiom" (s, left) in
      let premises = [encode_premise (IfPattern (p, right))] in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
and add_theories reasoner theories =
  let open MRT in
  TheoryMap.iter theories
    (fun _ (Theory (s, args, premises) as th) ->
      Util.debug 4 "add %a to KB" pp_theory th;
      let concl = encode mapping_theory "theory" (s, args) in
      let premises = List.map encode_premise premises in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
and add_lemmas reasoner lemmas =
  let open MRT in
  LemmaSet.iter
    (fun (Lemma (p, args, premises) as l) ->
      (* add definition of lemma *)
      Util.debug 4 "add %a to KB" pp_lemma l;
      let concl = encode mapping_lemma "lemma" (p, args) in
      let premises = List.map encode_premise premises in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
    lemmas
and add_clauses reasoner clauses =
  ClauseSet.iter
    (fun c ->
      let clause = _compile_clause c in
      MetaReasoner.add reasoner clause)
    clauses

let add_reasoner reasoner kb =
  add_axioms reasoner kb.axioms;
  add_theories reasoner kb.theories;
  add_lemmas reasoner kb.lemmas;
  add_clauses reasoner kb.clauses;
  ()

let on_lemma r =
  let s = MetaReasoner.on_new_fact_by r "lemma" in
  Signal.map s (fun lit ->
    try
      let p, terms = MRT.decode_head mapping_lemma "lemma" lit in
      (* recover a formula from the raw datalog literal *)
      let f = MetaPattern.apply (p, terms) in
      let f = MetaPattern.EncodedForm.decode f in
      NewLemma (f, lit)
    with e -> 
      Util.debug 0 "KB.on_lemma: exn %s" (Printexc.to_string e);
      raise e
    )

let on_axiom r =
  let s = MetaReasoner.on_new_fact_by r "axiom" in
  Signal.map s (fun lit ->
    let name, terms = MRT.decode_head mapping_axiom "axiom" lit in
    NewAxiom (name, terms))

let on_theory r =
  let s = MetaReasoner.on_new_fact_by r "theory" in
  Signal.map s (fun lit ->
    let name, terms = MRT.decode_head mapping_theory "theory" lit in
    NewTheory (name, terms, lit))

let cur_lemmas r =
  let seq = MetaReasoner.all_facts_by r "lemma" in
  Sequence.map
    (fun lit ->
      let p, terms = MRT.decode_head mapping_lemma "lemma" lit in
      (* recover a formula from the raw datalog literal *)
      let f = MetaPattern.apply (p, terms) in
      let f = MetaPattern.EncodedForm.decode f in
      NewLemma (f, lit))
    seq 

let cur_theories r =
  let seq = MetaReasoner.all_facts_by r "theory" in
  Sequence.map
    (fun lit ->
      let name, terms = MRT.decode_head mapping_theory "theory" lit in
      NewTheory (name, terms, lit))
    seq

(** {2 Backward Chaining} *)

(* match goal against lemmas' conclusions, yielding (lemma, args) list *)
let match_lemmas kb goal =
  LemmaSet.fold
    (fun (Lemma (p, args, premises) as lemma) acc ->
      let substs = MetaPattern.matching p goal in
      Sequence.fold
        (fun acc (_, args) ->
          (lemma, args) :: acc)
        acc substs)
    kb.lemmas []

type lemma_back_chain =
  | LBC_add_goal of MetaPattern.EncodedForm.t
  | LBC_add_datalog_goal of MetaReasoner.Logic.literal
  | LBC_add_datalog_clause of MetaReasoner.Logic.clause

let term_to_lit t =
  MRT.encode MetaPattern.EncodedForm.mapping "istrue" t

let term_of_lit lit =
  MRT.decode_head MetaPattern.EncodedForm.mapping "istrue" lit

(* suggest actions to take in order to solve the given formula goal *)
let backward_chain kb goal =
  let matches = match_lemmas kb goal in
  let actions = List.fold_left
    (fun acc (lemma, args) ->
      (* instantiating the lemma's conclusion with [args] implies [goal]. *)
      match lemma with
      | Lemma (pat, _, premises) ->
        let goal' = MetaPattern.apply (pat, args) in
        let clause = MetaReasoner.Logic.mk_clause
          (term_to_lit goal)
          [term_to_lit goal']
        in
        let datalog_goal = MRT.encode mapping_lemma
          "lemma" (pat, args)
        in
        LBC_add_goal goal' ::
        LBC_add_datalog_clause clause ::
        LBC_add_datalog_goal datalog_goal :: acc)
      [] matches
  in
  actions
  
(** {2 Conversion from Ast_theory} *)

module TBij = Util.Bijection(struct
  type t = T.t
  let equal = T.eq
  let hash = T.hash
end)

(* all formulas occurring in statements *)
let formulas_of_statements statements =
  let gather_premises premises =
    Sequence.fmap
      (function
      | A.IfAxiom _
      | A.IfTheory _ -> None
      | A.IfPattern f -> Some f)
      (Sequence.of_list premises)
  in
  Sequence.flatMap
    (function
      | A.Lemma(_, _, premises) -> gather_premises premises
      | A.LemmaInline(f,premises) ->
        Sequence.append (Sequence.singleton f) (gather_premises premises)
      | A.Axiom (_, _, f) -> Sequence.singleton f
      | A.Theory (_, _, premises) -> gather_premises premises
      | A.Clause (head, body) -> Sequence.empty
      | A.Include _ -> Sequence.empty)
    statements

(* apply the axiom definition to args, getting back a pattern,args *)
let apply_axiom axiom args =
  match axiom with
  | Axiom (_, left, pat, right) ->
    assert (List.length left = List.length right);
    assert (List.length left = List.length args);
    (* we need to permute [args] in the same way [right] is a permutation of [left] *)
    let p_args = TBij.of_list args left in
    let p_inner = TBij.of_list left right in
    let p = TBij.compose_list [p_args; p_inner; TBij.rev p_args] in
    let args' = TBij.apply_list p args in
    pat, args'

(** All terms referenced in the premises *)
let gather_premises_terms premises =
  let terms_of = function
    | IfAxiom (_, l)
    | IfTheory (_, l)
    | IfPattern (_, l) -> l
  in
  List.fold_left
    (fun l p -> Util.list_union T.eq l (terms_of p))
    [] premises

(** Apply function to the terms inside each premise *)
let fmap_premises f premises =
  List.map
    (function
      | IfAxiom (a, l) -> IfAxiom (a, List.map f l)
      | IfTheory (a, l) -> IfTheory (a, List.map f l)
      | IfPattern (p, l) -> IfPattern (p, List.map f l))
    premises

(** Mapping from symbols to variables. Used for compilation *)
module MapToVar = struct
  type t = (Symbol.t * HOTerm.t) list

  let empty = []

  (* map to *untyped* variables. Type checking should be outside... *)
  let add_terms map terms =
    List.fold_left
      (fun map t -> match t.T.term with
        | T.Const s when not (Symbol.Arith.is_arith s) ->
          (* map symbol to a fresh var *)
          if List.mem_assq s map
            then map
            else
              let v = T.mk_var ~ty:Type.i (List.length map) in  (* fresh var *)
              (s, v) :: map
        | _ -> map)
      map terms

  let of_list terms = add_terms [] terms

  let apply map t = match t.T.term with
    | T.Const s ->
      begin try List.assq s map
      with Not_found -> t
      end
    | _ -> t

  let apply_list map l = List.map (apply map) l
end

let str_to_terms l =
  List.map (fun s -> BT.const (Sym.mk_const s)) l

let closure_of_terms ctx l =
  let l = List.map (fun t -> snd (TypeInference.HO.infer ctx t)) l in
  TypeInference.TraverseClosure.seq l

(* type inference closure of a premise *)
let closure_of_premise ctx p =
  let open TypeInference.Closure in
  match p with
  | A.IfPattern f ->
    TypeInference.FO.infer_form ctx f >>= fun f' ->
    let f' = MetaPattern.EncodedForm.encode f' in
    let pat, args = MetaPattern.create f' in
    return (IfPattern (pat, args))
  | A.IfAxiom (s, args) ->
    closure_of_terms ctx (str_to_terms args) >>= fun args ->
    return (IfAxiom (s, args))
  | A.IfTheory (s, args) ->
    closure_of_terms ctx (str_to_terms args) >>= fun args ->
    return (IfTheory (s, args))

let closure_of_premises ctx l =
  let l = List.map (closure_of_premise ctx) l in
  TypeInference.TraverseClosure.seq l

(* given an Ast_theory.statement, return a closure of kb->kb *)
let closure_of_statement ctx statement =
  let open TypeInference.Closure in
  (* be sure that the variables are polymorphic *)
  let statement = A.generalize_statement statement in
  match statement with
  | A.Axiom (s, args, f) ->
    (* convert axiom *)
    TypeInference.FO.infer_form ctx f >>= fun f' ->
    closure_of_terms ctx (str_to_terms args) >>= fun left ->
    let f' = MetaPattern.EncodedForm.encode f' in
    let p, right = MetaPattern.create f' in
    (* map to variables *)
    let perm = MapToVar.of_list left in
    (* replace by variables *)
    let new_left = MapToVar.apply_list perm left in
    let new_right = MapToVar.apply_list perm right in
    (* build axiom *)
    let axiom = Axiom (s, new_left, p, new_right) in
    return (fun kb -> add_axiom kb axiom)
  | A.LemmaInline (f, premises) ->
    (* describe a lemma *)
    TypeInference.FO.infer_form ctx f >>= fun f' ->
    closure_of_premises ctx premises >>= fun premises' ->
    let f' = MetaPattern.EncodedForm.encode f' in
    let pat, args = MetaPattern.create f' in
    let args' = gather_premises_terms premises' in
    (* assert (List.for_all (fun t -> List.memq t args') args);  (* check safe *) *)
    (* map args' to fresh variables *)
    let perm = MapToVar.of_list args' in
    let new_premises = fmap_premises (MapToVar.apply perm) premises' in
    let lemma = Lemma (pat, MapToVar.apply_list perm args, new_premises) in
    return (fun kb -> add_lemma kb lemma)
  | A.Lemma (s, args, premises) ->
    closure_of_terms ctx (str_to_terms args) >>= fun args ->
    closure_of_premises ctx premises >>= fun premises ->
    let l = gather_premises_terms premises in
    (* assert (List.for_all (fun t -> List.memq t l) args);  (* check safe *) *)
    (* map symbols to variables *)
    let perm = MapToVar.of_list l in
    let new_premises = fmap_premises (MapToVar.apply perm) premises in
    let new_args = MapToVar.apply_list perm args in
    return
      (fun kb -> match get_axiom kb s with
      | [] -> failwith (Util.sprintf "axiom %s is not defined" s)
      | (Axiom _ as axiom) :: _ ->
        (* use the first axiom definition to get a proper pattern *)
        let pat, args' = apply_axiom axiom new_args in 
        let lemma = Lemma (pat, args', new_premises) in
        add_lemma kb lemma)
  | A.Theory (s, args, premises) ->
    closure_of_premises ctx premises >>= fun premises ->
    closure_of_terms ctx (str_to_terms args) >>= fun args ->
    let l = gather_premises_terms premises in
    (* assert (List.for_all (fun t -> List.memq t l) args);  (* check safe *) *)
    (* map symbols to variables *)
    let perm = MapToVar.of_list l in
    let new_premises = fmap_premises (MapToVar.apply perm) premises in
    let new_args = MapToVar.apply_list perm args in
    let theory = Theory (s, new_args, new_premises) in
    return (fun kb -> add_theory kb theory)
  | A.Clause (head, body) ->
    let c = Clause (head, body) in
    return (fun kb -> add_clause kb c)
  | A.Include _ -> failwith "KB.kb_of_statements: remaining includes"

(** Conversion of a list of Ast_theory.statement to a KB *)
let kb_of_statements ?(base=Signature.base) ?(init=empty) statements =
  (* deal with a single statement *)
  let add_statement kb statement =
    Util.debug 3 "metaKB: add statement %a" A.pp statement;
    (* context is local *)
    let ctx = TypeInference.Ctx.of_signature base in
    let closure = closure_of_statement ctx statement in
    (* FIXME: generalize types as much as possible (how to deal with type params?)
    TypeInference.Ctx.generalize ctx;
    *)
    TypeInference.Ctx.bind_to_default ctx;
    let modify_kb = TypeInference.Ctx.apply_closure ctx closure in
    modify_kb kb
  in
  (* error monad *)
  let module Err = Monad.Err in
  Monad.TraverseErr.fold
    statements (Monad.Err.return init)
    (fun kb stmt ->
      try Err.return (add_statement kb stmt)
      with TypeUnif.Error e ->
        let msg = Util.sprintf "typing statement %a: error %a"
          A.pp stmt TypeUnif.pp_error e in
        Util.debug 2 "stacktrace: %s" (Printexc.get_backtrace ());
        Err.fail msg
      | Failure msg ->
        let msg = Util.sprintf "converting statement %a: error %s" A.pp stmt msg in
        Util.debug 2 "stacktrace: %s" (Printexc.get_backtrace ());
        Err.fail msg)

(** {2 IO} *)

(** Parse a theory file, and build a KB out of it *)
let parse_theory_file ?base filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    Location.set_file lexbuf filename;
    let statements = Parse_theory.parse_statements Lex_theory.token lexbuf in
    kb_of_statements ?base (Sequence.of_list statements)
  with Unix.Unix_error (e, _, _) ->
    let msg =
      Util.sprintf "IO error reading theory file %s: %s"
      filename (Unix.error_message e) in
    Monad.Err.fail msg
  | A.ParseError loc ->
    let msg = Util.sprintf "parse_theory_file: syntax error at %a" Location.pp loc in
    Monad.Err.fail msg
  
let save filename kb =
  let oc = open_out filename in
  try
    Bij.TrBencode.write ~bij oc kb;
    close_out oc;
  with e ->
    close_out oc;
    raise e

let restore filename =
  try
    let ic = open_in filename in
    begin try
      let kb = Bij.TrBencode.read ~bij ic in
      close_in ic;
      Monad.Err.return kb
    with e ->
      close_in ic;
      let msg = Util.sprintf "restoring KB from %s: error %s"
        filename (Printexc.to_string e) in
      Monad.Err.fail msg
    end
  with e ->
    let msg = Util.sprintf "restoring KB from %s: error %s"
      filename (Printexc.to_string e) in
    Monad.Err.fail msg

