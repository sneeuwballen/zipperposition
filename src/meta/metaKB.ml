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

(** {1 Knowledge base} *)

open Logtk

module A = Ast_theory
module T = Term
module F = Formula
module MRT = MetaReasoner.Translate

(** {2 Basic knowledge} *)

type lemma =
  | Lemma of MetaPattern.t * Term.t list * premise list
and axiom =
  | Axiom of string * Term.t list * MetaPattern.t * Term.t list
and theory =
  | Theory of string * Term.t list * premise list
and premise =
  | IfAxiom of string * Term.t list
  | IfTheory of string * Term.t list
  | IfPattern of MetaPattern.t * Term.t list
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

module StringMap = Sequence.Map.Make(String)
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
  axioms : axiom StringMap.t;     (* axioms, by name *)
  theories : TheoryMap.t;         (* theories, by name *)
  clauses : ClauseSet.t;          (* set of raw clauses *)
} (* KB *)

let eq t1 t2 =
  LemmaSet.equal t1.lemmas t2.lemmas &&
  StringMap.equal (fun a1 a2 -> compare_axiom a1 a2 = 0) t1.axioms t2.axioms &&
  TheoryMap.equal t1.theories t2.theories &&
  ClauseSet.equal t1.clauses t2.clauses

let empty =
  { lemmas = LemmaSet.empty;
    axioms = StringMap.empty;
    theories = TheoryMap.empty;
    clauses = ClauseSet.empty;
  }

let add_lemma kb l =
  { kb with lemmas = LemmaSet.add l kb.lemmas }

let add_axiom kb a =
  let name = name_of_axiom a in
  assert (not (StringMap.mem name kb.axioms));
  { kb with axioms = StringMap.add name a kb.axioms }

let add_theory kb t =
  let name = name_of_theory t in
  { kb with theories = TheoryMap.add kb.theories name t }

let add_clause kb c =
  { kb with clauses = ClauseSet.add c kb.clauses }

let get_axiom kb s =
  try Some (StringMap.find s kb.axioms)
  with Not_found -> None

let get_theory kb s = TheoryMap.find kb.theories s

let all_patterns kb =
  let l = ref [] in
  let add_pattern p = l := p :: !l in
  let rec iter_premises = function
    | [] -> ()
    | (IfAxiom _ | IfTheory _) :: premises -> iter_premises premises
    | (IfPattern (p, _)) :: premises -> add_pattern p; iter_premises premises
  in
  StringMap.iter (fun _ (Axiom (_, _, p, _)) -> add_pattern p) kb.axioms;
  TheoryMap.iter kb.theories (fun _ (Theory (_, _, premises)) -> iter_premises premises);
  LemmaSet.iter (fun (Lemma (_, _, premises)) -> iter_premises premises) kb.lemmas;
  !l

let union t1 t2 =
  let merge_axioms _ a1 a2 = match a1, a2 with
  | None, Some a
  | Some a, None -> Some a
  | None, None -> None
  | Some a1, Some a2 when compare_axiom a1 a2 = 0 -> Some a1
  | Some a1, Some a2 -> failwith ("two definitions for axiom " ^ (name_of_axiom a1))
  in
  { lemmas = LemmaSet.union t1.lemmas t2.lemmas;
    axioms = StringMap.merge merge_axioms t1.axioms t2.axioms;
    theories = TheoryMap.union t1.theories t2.theories;
    clauses = ClauseSet.union t1.clauses t2.clauses;
  }

let diff t1 t2 =
  (* elements of [m1] that are not in [m2] *)
  let diff_map m1 m2 =
    StringMap.filter (fun name _ -> not (StringMap.mem name m2)) m1
  in
  { lemmas = LemmaSet.diff t1.lemmas t2.lemmas;
    axioms = diff_map t1.axioms t2.axioms;
    theories = TheoryMap.diff t1.theories t2.theories;
    clauses = ClauseSet.diff t1.clauses t2.clauses;
  }

let to_seq kb =
  ( LemmaSet.to_seq kb.lemmas
  , StringMap.values kb.axioms
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
  StringMap.iter (fun _ a -> Printf.bprintf buf "  %a\n" pp_axiom a) kb.axioms;
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
  | NewLemma of Formula.t * MetaReasoner.Logic.literal
    (** formula + explanation *)

and found_theory =
  | NewTheory of string * Term.t list * MetaReasoner.Logic.literal

and found_axiom =
  | NewAxiom of string * Term.t list

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
  StringMap.iter
    (fun _ (Axiom (s, left, p, right)) ->
      let concl = encode mapping_axiom "axiom" (s, left) in
      let premises = [encode_premise (IfPattern (p, right))] in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
    axioms
and add_theories reasoner theories =
  let open MRT in
  TheoryMap.iter theories
    (fun _ (Theory (s, args, premises)) ->
      let concl = encode mapping_theory "theory" (s, args) in
      let premises = List.map encode_premise premises in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
and add_lemmas reasoner lemmas =
  let open MRT in
  LemmaSet.iter
    (fun (Lemma (p, args, premises)) ->
      (* add definition of lemma *)
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
    let p, terms = MRT.decode_head mapping_lemma "lemma" lit in
    (* recover a formula from the raw datalog literal *)
    let f = MetaPattern.apply (p, terms) in
    let f = MetaPattern.EncodedForm.decode f in
    NewLemma (f, lit))

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

module TermBij = Util.Bijection(struct
  type t = Term.t
  let equal = Term.eq
  let hash = Term.hash
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
      | A.Include _
      | A.Error _ -> Sequence.empty)
    statements

(* infer signature from statement *)
let signature_of_statement stmt =
  F.signature_seq (formulas_of_statements (Sequence.singleton stmt))

(* apply the axiom definition to args, getting back a pattern,args *)
let apply_axiom axiom args =
  match axiom with
  | Axiom (_, left, pat, right) ->
    assert (List.length left = List.length right);
    assert (List.length left = List.length args);
    (* we need to permute [args] in the same way [right] is a permutation of [left] *)
    let p_args = TermBij.of_list args left in
    let p_inner = TermBij.of_list left right in
    let p = TermBij.compose_list [p_args; p_inner; TermBij.rev p_args] in
    let args' = TermBij.apply_list p args in
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

(** Map the terms to fresh variables, returning a permutation *)
let map_to_vars ?(offset=0) terms =
  let vars = List.mapi (fun i t -> T.mk_var (i+offset)) terms in
  TermBij.of_list terms vars

let str_to_terms l = List.map (fun s -> T.mk_const (Symbol.mk_const s)) l

(** Conversion of a list of Ast_theory.statement to a KB *)
let kb_of_statements ?(init=empty) statements =
  let convert_premise ~signature = function
    | A.IfPattern f ->
      let f' = MetaPattern.EncodedForm.encode f in
      let pat, args = MetaPattern.create ~signature f' in
      IfPattern (pat, args)
    | A.IfAxiom (s, args) ->
      let args = str_to_terms args in
      IfAxiom (s, args)
    | A.IfTheory (s, args) ->
      let args = str_to_terms args in
      IfTheory (s, args)
  in
  let add_statement kb statement =
    (* infer types *)
    let signature = Signature.curry (signature_of_statement statement) in
    match statement with
    | A.Axiom (s, args, f) ->
      (* convert axiom *)
      let left = str_to_terms args in
      let f' = MetaPattern.EncodedForm.encode f in
      let p, right = MetaPattern.create ~signature f' in
      (* map to variables *)
      let perm = map_to_vars left in
      (* replace by variables *)
      let new_left = TermBij.apply_list perm left in
      let new_right = TermBij.apply_list perm right in
      (* build axiom *)
      let axiom = Axiom (s, new_left, p, new_right) in
      add_axiom kb axiom
    | A.LemmaInline (f, premises) ->
      (* describe a lemma *)
      let f' = MetaPattern.EncodedForm.encode f in
      let pat, args = MetaPattern.create ~signature f' in
      let premises = List.map (convert_premise ~signature) premises in
      let args' = gather_premises_terms premises in
      assert (List.for_all (fun t -> List.memq t args') args);  (* check safe *)
      (* map args' to fresh variables *)
      let perm = map_to_vars args' in
      let new_premises = fmap_premises (TermBij.apply perm) premises in
      let lemma = Lemma (pat, TermBij.apply_list perm args, new_premises) in
      add_lemma kb lemma
    | A.Lemma (s, args, premises) ->
      let args = str_to_terms args in
      let premises = List.map (convert_premise ~signature) premises in
      let l = gather_premises_terms premises in
      assert (List.for_all (fun t -> List.memq t l) args);  (* check safe *)
      (* map symbols to variables *)
      let perm = map_to_vars l in
      let new_premises = fmap_premises (TermBij.apply perm) premises in
      let new_args = TermBij.apply_list perm args in
      begin match get_axiom kb s with
      | None -> failwith (Util.sprintf "axiom %s is not defined" s)
      | Some (Axiom _ as axiom) ->
        (* use the axiom definition to get a proper pattern *)
        let pat, args' = apply_axiom axiom new_args in 
        let lemma = Lemma (pat, args', new_premises) in
        add_lemma kb lemma
      end
    | A.Theory (s, args, premises) ->
      let args = str_to_terms args in
      let premises = List.map (convert_premise ~signature) premises in
      let l = gather_premises_terms premises in
      assert (List.for_all (fun t -> List.memq t l) args);  (* check safe *)
      (* map symbols to variables *)
      let perm = map_to_vars l in
      let new_premises = fmap_premises (TermBij.apply perm) premises in
      let new_args = TermBij.apply_list perm args in
      let theory = Theory (s, new_args, new_premises) in
      add_theory kb theory
    | A.Clause (head, body) ->
      let c = Clause (head, body) in
      add_clause kb c
    | A.Include _ -> failwith "KB.kb_of_statements: remaining includes"
    | A.Error _ -> failwith "KB.kb_of_statements: error in list"
  in
  Sequence.fold add_statement init statements

(** {2 IO} *)

(** Parse a theory file, and build a KB out of it *)
let parse_theory_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    let statements = Parse_theory.parse_statements Lex_theory.token lexbuf in
    begin try
      (* error, fail *)
      let e = List.find A.is_error statements in
      Util.debug 0 "error in parsed KB: %s" (A.error_to_string e);
      empty
    with Not_found ->
      (* no error, proceed *)
      let kb = kb_of_statements (Sequence.of_list statements) in
      kb
    end
  with
  | Unix.Unix_error (e, _, _) ->
    let msg = Unix.error_message e in
    Util.debug 0 "error reading theory file %s: %s" filename msg;
    empty
  
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
      Some kb
    with e ->
      close_in ic;
      raise e
    end
  with e ->
    Util.debug 1 "restoring KB from %s: error %s" filename (Printexc.to_string e);
    None

