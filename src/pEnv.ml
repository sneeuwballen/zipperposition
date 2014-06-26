
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

(** {1 Preprocessing Env} *)

open Logtk

module F = Formula.FO
module PF = PFormula

(** {2 Transformations} *)

type operation_result =
  | SimplifyInto of PFormula.t  (** replace by formula *)
  | Remove                      (** remove formula *)
  | Esa of PFormula.t list      (** replace by list of formulas *)
  | Add of PFormula.t list      (** add given formulas *)
  | AddOps of operation list    (** New operations to perform! *)

and operation = PFormula.Set.t -> PFormula.t -> operation_result list

exception ProcessNext

(* fixpoint of transformations *)
let fix ops set =
  let ops = ref ops in
  let ans = ref PF.Set.empty in
  let q = Queue.create () in
  (* also process the given formulas *)
  let add_forms l = List.iter (fun f -> Queue.push f q) l in
  (* a new operation appeared, so we must process again all formulas
      to be sure that they are irreducible *)
  let restart () =
    Util.debug 4 "restart fixpoint computation";
    PF.Set.iter (fun f -> Queue.push f q) !ans;
    ans := PF.Set.empty
  in
  (* initial queue to process *)
  PF.Set.iter (fun f -> Queue.push f q) set;
  while not (Queue.is_empty q) do
    let pf = Queue.pop q in
    if not (PF.Set.mem pf !ans)
    then try
      (* memoized simplifications *)
      let pf = PF.follow_simpl pf in
      List.iter
        (fun tr ->
          let keep = ref true in
          let results = tr !ans pf in
          List.iter
            (function
            | Remove ->
              Util.debug 5 "remove form %a" PF.pp pf;
              keep := false
            | Esa [pf'] when PF.eq_noproof pf pf' -> ()
            | Esa l ->
              (* get rid of [f], but process [l] instead *)
              Util.debug 5 "%a equisatisfiable with %a" PF.pp pf (Util.pp_list PF.pp) l;
              add_forms l;
              keep := false
            | Add l ->
              (* continue processing [pf], but also [l] *)
              Util.debug 5 "add forms %a" (Util.pp_list PF.pp) l;
              add_forms l
            | AddOps [] -> assert false
            | AddOps l ->
              (* add those operations to the list of ops to perform, and keep [pf] *)
              ops := List.rev_append l !ops;
              Util.debug 5 "restart after adding %d operations" (List.length l);
              restart ()
            | SimplifyInto f' when F.ac_eq (PF.form pf) (PF.form f') ->
              () (* not really simplified *)
            | SimplifyInto f' ->
              (* ignore [f], process [f'] instead, and remember the
                  simplification step *)
              PF.simpl_to ~from:pf ~into:f';
              Util.debug 5 "simplify %a into %a" PF.pp pf PF.pp f';
              Queue.push f' q;
              keep := false
            )
          results;
          if not !keep then raise ProcessNext)
        !ops;
      (* terminal node, keep it *)
      ans := PF.Set.add pf !ans
    with ProcessNext -> ()
  done;
  !ans

(* remove trivial formulas *)
let remove_trivial set pf =
  if F.is_trivial (PF.form pf)
    then [Remove]
    else []

(* reduce formulas to CNF *)
let cnf _set pf =
  Util.debug 3 "reduce %a to CNF..." PF.pp pf;
  (* reduce to CNF this formula *)
  let clauses = Cnf.cnf_of (PF.form pf) in
  (* now build "proper" clauses, with proof and all *)
  match clauses with
  | [[f]] when F.eq f (PF.form pf) -> []
  | _ ->
    let proof f' = Proof.mk_f_esa ~rule:"cnf" f' [PF.proof pf] in
    let clauses =
      List.map
        (fun c ->
          (* clause represented as formula *)
          let f = F.Base.or_ c in
          PF.create f (proof f))
        clauses
    in
    [Esa clauses]

(* TODO
let meta_prover ~meta =
  fun set pf ->
    (* scan formula *)
    let res  = MetaProverState.scan_formula meta pf in
    (* exploit result, adding lemmas to the set *)
    let lemmas = Util.list_fmap
      (function
        | MetaProverState.Deduced (pf', _) -> Some pf'
        | MetaProverState.Theory _ -> None)
      res
    in
    if lemmas = []
      then []
      else [Add lemmas]
*)

let rw_term ?(rule="rw") ~premises trs =
  fun set pf ->
    let f = PF.form pf in
    let f' = F.map (fun t -> Rewriting.TRS.rewrite trs t) f in
    if F.eq f f'
      then []
      else
        let premises = PF.Set.to_seq premises in
        let premises = Sequence.to_list (Sequence.map PF.proof premises) in
        let proof = Proof.mk_f_simp ~rule f' (PF.proof pf :: premises) in
        let pf' = PF.create f' proof in
        [SimplifyInto pf']

let rw_form ?(rule="rw") ~premises frs =
  fun set pf ->
    let f = PF.form pf  in
    Util.debug 5 "start rewriting %a" PF.pp pf;
    let f' = Rewriting.FormRW.rewrite frs f in
    Util.debug 5 "done rewriting %a" PF.pp pf;
    if F.eq f f'
      then []
      else
        let premises = PF.Set.to_seq premises in
        let premises = Sequence.to_list (Sequence.map PF.proof premises) in
        let proof = Proof.mk_f_simp ~rule f' (PF.proof pf::premises) in
        let pf' = PF.create f' proof in
        let _ = Util.debug 5 "rewritten %a in %a!" PF.pp pf PF.pp pf' in
        [SimplifyInto pf']

let fmap_term ~rule func =
  fun set pf ->
    let f = PF.form pf in
    let f' = F.map func f in
    if F.eq f f'
      then []
      else
        let proof = Proof.mk_f_simp ~rule f' [PF.proof pf] in
        let pf' = PF.create f' proof in
        [SimplifyInto pf']

(* expand definitions *)
let expand_def set pf =
  (* detect definitions in [pf] *)
  let transforms = FormulaShape.detect_def ~only:`Pred
    (Sequence.singleton (PF.form pf)) in
  (* make new operations on the set of formulas *)
  let premises = PF.Set.singleton pf in
  let ops = Util.list_fmap
    (function
      | Transform.RwForm frs ->
        Some (rw_form ~rule:"expand_pred_def" ~premises frs)
      | Transform.RwTerm trs ->
        Some (rw_term ~rule:"expand_term_def" ~premises trs)
      | Transform.Tr _ -> None)
    transforms
  in
  (* add those definitions, and remove the formula *)
  if ops = []
    then []
    else
      let _ = Util.debug 5 "detected def in %a" PF.pp pf in
      [Remove; AddOps ops]

(** {2 Preprocessing} *)

type t = {
  mutable axioms : PF.Set.t;
  mutable ops : (int * (PF.Set.t -> operation)) list;  (* int: priority *)
  mutable constrs : Precedence.Constr.t list;
  mutable constr_rules : (PF.Set.t -> Precedence.Constr.t) list;
  mutable status : (Symbol.t * Precedence.symbol_status) list;
  mutable base : Signature.t;
  params : Params.t;
}

let copy penv = { penv with ops = penv.ops; }

let get_params ~penv = penv.params

let signature ~penv = penv.base

let add_base_sig ~penv s =
  penv.base <- Signature.merge penv.base s

let add_axiom ~penv ax =
  penv.axioms <- PF.Set.add ax penv.axioms

let add_axioms ~penv axioms =
  Sequence.iter (add_axiom ~penv) axioms

let add_operation ~penv ~prio op =
  penv.ops <- (prio, (fun _ -> op)) :: penv.ops

let add_operation_rule ~penv ~prio rule =
  penv.ops <- (prio, rule) :: penv.ops

let create ?(base=Signature.TPTP.base) params =
  let penv = {
    axioms = PF.Set.empty;
    ops = [];
    constrs = [];
    constr_rules = [];
    status = [];
    base;
    params;
  } in
  penv

let process ~penv set =
  let compare (p1, _) (p2, _) = p1 - p2 in
  let rules = List.map snd (List.sort compare penv.ops) in
  let ops = List.map (fun rule -> rule set) rules in
  (* also add axioms *)
  let set = PF.Set.union set penv.axioms in
  fix ops set

let add_constr ~penv c =
  penv.constrs <- c::penv.constrs

let add_constrs ~penv l =
  List.iter (add_constr ~penv) l

let add_constr_rule ~penv r =
  penv.constr_rules <- r :: penv.constr_rules

let add_status ~penv l = penv.status <- List.rev_append l penv.status

let mk_precedence ~penv set =
  let constrs = penv.constrs @ List.map (fun rule -> rule set) penv.constr_rules in
  let signature = penv.base in
  let symbols = Signature.Seq.symbols signature
        |> Symbol.Seq.add_set Symbol.Set.empty
  in
  let symbols' = PFormula.Set.symbols set in
  let all_symbols = Symbol.Set.union symbols symbols' in
  let p = Precedence.create constrs (Symbol.Set.elements all_symbols) in
  let p = List.fold_left
    (fun p (s,status) -> Precedence.declare_status p s status)
    p penv.status
  in
  p
