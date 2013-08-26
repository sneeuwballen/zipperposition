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

module T = Term

(** {2 Basic knowledge} *)

type lemma =
  | Lemma of MetaPattern.t * Term.t list * premise list
and axiom =
  | Axiom of string * Term.t list * MetaPattern.t * Term.t list
and theory =
  | Theory of string * Term.t list * premise list
and premise =
  | IfAxiom of string * Term.t list
  | IfPattern of MetaPattern.t * Term.t list

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
  | IfAxiom _, IfPattern _ -> 1
  | IfPattern _, IfAxiom _ -> -1
  | IfAxiom (s1, l1), IfAxiom (s2, l2) ->
    Util.lexicograph_combine [String.compare s1 s2; Util.lexicograph T.compare l1 l2]
  | IfPattern (p1, args1), IfPattern (p2, args2) ->
    Util.lexicograph_combine [MetaPattern.compare p1 p2; Util.lexicograph T.compare args1 args2]
and compare_premises l1 l2 =
  Util.lexicograph compare_premise l1 l2

module StringMap = Map.Make(String)
module LemmaSet = Set.Make(struct type t = lemma let compare = compare_lemma end)

let name_of_axiom = function
  | Axiom (s, _, _, _) -> s

let name_of_theory = function
  | Theory (s, _, _) -> s

(** {2 Knowledge base} *)

type t = {
  lemmas : LemmaSet.t;            (* set of lemmas *)
  axioms : axiom StringMap.t;     (* axioms, by name *)
  theories : theory StringMap.t;  (* theories, by name *)
} (* KB *)

let eq t1 t2 =
  LemmaSet.equal t1.lemmas t2.lemmas &&
  StringMap.equal (fun a1 a2 -> compare_axiom a1 a2 = 0) t1.axioms t2.axioms &&
  StringMap.equal (fun t1 t2 -> compare_theory t1 t2 = 0) t1.theories t2.theories

let empty =
  { lemmas = LemmaSet.empty;
    axioms = StringMap.empty;
    theories = StringMap.empty;
  }

let add_lemma kb l =
  { kb with lemmas = LemmaSet.add l kb.lemmas }

let add_axiom kb a =
  let name = name_of_axiom a in
  assert (not (StringMap.mem name kb.axioms));
  { kb with axioms = StringMap.add name a kb.axioms }

let add_theory kb t =
  let name = name_of_theory t in
  assert (not (StringMap.mem name kb.theories));
  { kb with theories = StringMap.add name t kb.theories }

let get_axiom kb s =
  try Some (StringMap.find s kb.axioms)
  with Not_found -> None

let get_theory kb s =
  try Some (StringMap.find s kb.theories)
  with Not_found -> None

let all_patterns kb =
  let l = ref [] in
  let add_pattern p = l := p :: !l in
  let rec iter_premises = function
    | [] -> ()
    | (IfAxiom _) :: premises -> iter_premises premises
    | (IfPattern (p, _)) :: premises -> add_pattern p; iter_premises premises
  in
  StringMap.iter (fun _ (Axiom (_, _, p, _)) -> add_pattern p) kb.axioms;
  StringMap.iter (fun _ (Theory (_, _, premises)) -> iter_premises premises) kb.theories;
  LemmaSet.iter (fun (Lemma (_, _, premises)) -> iter_premises premises) kb.lemmas;
  !l

let union t1 t2 =
  let merge_axioms _ a1 a2 = match a1, a2 with
  | None, Some a
  | Some a, None -> Some a
  | None, None -> None
  | Some a1, Some a2 when compare_axiom a1 a2 = 0 -> Some a1
  | Some a1, Some a2 -> failwith ("two definitions for axiom " ^ (name_of_axiom a1))
  and merge_theories _ t1 t2 = match t1, t2 with
  | None, None -> None
  | Some t, None
  | None, Some t -> Some t
  | Some t1, Some t2 when compare_theory t1 t2 = 0 -> Some t1
  | Some t1, Some _ -> failwith ("two definitions for theory " ^ (name_of_theory t1))
  in
  { lemmas = LemmaSet.union t1.lemmas t2.lemmas;
    axioms = StringMap.merge merge_axioms t1.axioms t2.axioms;
    theories = StringMap.merge merge_theories t1.theories t2.theories;
  }

let diff t1 t2 =
  (* elements of [m1] that are not in [m2] *)
  let diff_map m1 m2 =
    StringMap.filter (fun name _ -> not (StringMap.mem name m2)) m1
  in
  { lemmas = LemmaSet.diff t1.lemmas t2.lemmas;
    axioms = diff_map t1.axioms t2.axioms;
    theories = diff_map t1.theories t2.theories;
  }

let pp buf kb =
  let pp_premise buf p = match p with
  | IfAxiom (s, []) -> Buffer.add_string buf s
  | IfAxiom (s, args) -> Printf.bprintf buf "%s(%a)" s (Util.pp_list T.pp) args
  | IfPattern (p, args) -> MetaPattern.pp_apply buf (p, args)
  in
  let pp_premises = Util.pp_list pp_premise in
  let pp_lemma buf = function
    | Lemma (p, args, premises) ->
      Printf.bprintf buf "lemma(%a, %a)" MetaPattern.pp_apply (p,args) pp_premises premises
  and pp_axiom buf = function
    | Axiom (s, left, p, right) ->
      Printf.bprintf buf "axiom(%s(%a), %a)" s
        (Util.pp_list T.pp) left MetaPattern.pp_apply (p,right)
  and pp_theory buf = function
    | Theory (s, args, premises) -> Printf.bprintf buf "theory(%s(%a), %a)"
      s (Util.pp_list T.pp) args pp_premises premises
  in
  Buffer.add_string buf "KB(";
  StringMap.iter (fun _ a -> pp_axiom buf a) kb.axioms;
  StringMap.iter (fun _ t -> pp_theory buf t) kb.theories;
  LemmaSet.iter (pp_lemma buf) kb.lemmas;
  Buffer.add_string buf ")";
  ()

let fmt fmt kb =
  let s = Util.sprintf "%a" pp kb in
  Format.pp_print_string fmt s

let bij =
  let open Bij in
  map
    ~inject:(fun _ -> assert false)
    ~extract:(fun _ -> assert false)
    unit_
(* TODO *)

(** {2 MetaReasoner} *)

type found_lemma =
  | NewLemma of Term.t
and found_theory =
  | NewTheory of string * Term.t list
and found_axiom =
  | NewAxiom of string * Term.t list

let mapping_lemma = MetaPattern.mapping
and mapping_theory =
  let open MetaReasoner.Translate in
  pair str (list_ term)
and mapping_axiom =
  let open MetaReasoner.Translate in
  pair str (list_ term)
  
let encode_premise p =
  let open MetaReasoner.Translate in
  match p with
  | IfAxiom (s, terms) ->
    let mapping = pair str (list_ term) in
    encode mapping "axiom" (s, terms)
  | IfPattern (p, args) -> encode MetaPattern.mapping "pattern" (p, args)

let add_axioms reasoner axioms =
  let open MetaReasoner.Translate in
  StringMap.iter
    (fun _ (Axiom (s, left, p, right)) ->
      let concl = encode mapping_axiom "axiom" (s, left) in
      let premises = [encode_premise (IfPattern (p, right))] in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
    axioms
and add_theories reasoner theories =
  let open MetaReasoner.Translate in
  StringMap.iter
    (fun _ (Theory (s, args, premises)) ->
      let concl = encode mapping_theory "theory" (s, args) in
      let premises = List.map encode_premise premises in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
    theories
and add_lemmas reasoner lemmas =
  let open MetaReasoner.Translate in
  LemmaSet.iter
    (fun (Lemma (p, args, premises)) ->
      (* add definition of lemma *)
      let concl = encode mapping_lemma "lemma" (p, args) in
      let premises = List.map encode_premise premises in
      let clause = MetaReasoner.Logic.mk_clause concl premises in
      MetaReasoner.add reasoner clause)
    lemmas

let add_reasoner reasoner kb =
  add_axioms reasoner kb.axioms;
  add_theories reasoner kb.theories;
  add_lemmas reasoner kb.lemmas;
  ()

(** {2 Backward Chaining} *)

(* match goal against lemmas' conclusions, yielding (lemma, args) list *)
let match_lemmas kb goal =
  let goal' = MetaPattern.encode goal in
  LemmaSet.fold
    (fun (Lemma (p, args, premises) as lemma) acc ->
      let substs = MetaPattern.matching p goal' in
      Sequence.fold
        (fun acc (_, args) ->
          (lemma, args) :: acc)
        acc substs)
    kb.lemmas []

type lemma_back_chain =
  | LBC_add_goal of Term.t
  | LBC_add_datalog_goal of MetaReasoner.Logic.literal
  | LBC_add_datalog_clause of MetaReasoner.Logic.clause

let term_to_lit t =
  MetaReasoner.Translate.encode MetaReasoner.Translate.term "istrue" t

let term_of_lit lit =
  MetaReasoner.Translate.decode_head MetaReasoner.Translate.term "istrue" lit

(* suggest actions to take in order to solve the given goal *)
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
        let datalog_goal = MetaReasoner.Translate.encode mapping_lemma "lemma" (pat, args) in
        LBC_add_goal goal' :: LBC_add_datalog_clause clause
        :: LBC_add_datalog_goal datalog_goal :: acc)
      [] matches
  in
  actions
  
(** {2 Conversion from Ast_theory} *)

module TermBij = Util.Bijection(struct
  type t = Term.t
  let equal = Term.eq
  let hash = Term.hash
end)

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
    | IfAxiom (_, l) -> l
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
      | IfPattern (p, l) -> IfPattern (p, List.map f l))
    premises

(** Map the terms to fresh variables, returning a permutation *)
let map_to_vars ?(offset=0) terms =
  let vars = List.mapi (fun i t -> T.mk_var (i+offset)) terms in
  TermBij.of_list terms vars

let str_to_terms l = List.map (fun s -> T.mk_const (Symbol.mk_symbol s)) l

(** Conversion of a list of Ast_theory.statement to a KB *)
let kb_of_statements ?(init=empty) statements =
  let module A = Ast_theory in
  let convert_premise = function
    | A.IfAxiom t ->
      let pat, args = MetaPattern.create t in
      IfPattern (pat, args)
    | A.IfFact (s, args) ->
      let args = str_to_terms args in
      IfAxiom (s, args)
  in
  let add_statement kb statement =
    match statement with
    | A.Axiom (s, args, t) ->
      (* convert axiom *)
      let left = str_to_terms args in
      let p, right = MetaPattern.create t in
      assert (List.length args = List.length right);
      (* map to variables *)
      let perm = map_to_vars left in
      (* replace by variables *)
      let new_left = TermBij.apply_list perm left in
      let new_right = TermBij.apply_list perm right in
      (* build axiom *)
      let axiom = Axiom (s, new_left, p, new_right) in
      add_axiom kb axiom
    | A.LemmaInline (t, premises) ->
      (* describe a lemma *)
      let pat, args = MetaPattern.create t in
      let premises = List.map convert_premise premises in
      let args' = gather_premises_terms premises in
      assert (List.for_all (fun t -> List.memq t args') args);  (* check safe *)
      (* map args' to fresh variables *)
      let perm = map_to_vars args' in
      let new_premises = fmap_premises (TermBij.apply perm) premises in
      let lemma = Lemma (pat, TermBij.apply_list perm args, new_premises) in
      add_lemma kb lemma
    | A.Lemma (s, args, premises) ->
      let args = str_to_terms args in
      let premises = List.map convert_premise premises in
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
      let premises = List.map convert_premise premises in
      let l = gather_premises_terms premises in
      assert (List.for_all (fun t -> List.memq t l) args);  (* check safe *)
      (* map symbols to variables *)
      let perm = map_to_vars l in
      let new_premises = fmap_premises (TermBij.apply perm) premises in
      let new_args = TermBij.apply_list perm args in
      let theory = Theory (s, new_args, new_premises) in
      add_theory kb theory
    | A.Include _ -> failwith "KB.kb_of_statements: remaining includes"
    | A.Error _ -> failwith "KB.kb_of_statements: error in list"
  in
  List.fold_left add_statement init statements

(** {2 IO} *)

(** Parse a theory file, and build a KB out of it *)
let parse_theory_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    let statements = Parse_theory.parse_statements Lex_theory.token lexbuf in
    let kb = kb_of_statements statements in
    kb
  with Unix.Unix_error (e, _, _) ->
    let msg = Unix.error_message e in
    Util.debug 1 "%% error reading theory file %s: %s" filename msg;
    empty
  
let save filename =
  failwith "KB.save: not implemented"
(* TODO *)

let restore filename =
  failwith "KB.restore: not implemented"
(* TODO *)

