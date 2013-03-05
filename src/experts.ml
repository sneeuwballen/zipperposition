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

(** Decision procedures for theories *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils

(** {2 General interface} *)

let stat_expert_redundant = mk_stat "experts.redundant"
let stat_expert_simplify = mk_stat "experts.simplify"

let prof_simplify_expert = Utils.mk_profiler "experts.simplify"
let prof_redundant_expert = Utils.mk_profiler "experts.redundant"
let prof_ground_pair = Utils.mk_profiler "experts.ground_pair"
let prof_normal_form = Utils.mk_profiler "experts.normal_form"

type t = {
  expert_name : string;                 (** Theory the expert works on *)
  expert_descr : string;                (** Description of the expert *)
  expert_equal : term -> term -> bool;  (** Check whether two terms are equal *)
  expert_sig : SSet.t;                  (** Symbols of the theory *)
  expert_clauses : hclause list;        (** Additional axioms *)
  expert_canonize : term -> term;       (** Get a canonical form of the term *)
  expert_ord : ordering -> bool;        (** Compatible with ord? *)
  expert_update_ctx : context -> t;     (** How to update the context *)
  expert_ctx : context;                 (** Context used by the expert *)
  expert_solve : ((term*term) list -> substitution list) option;
    (** The expert may be able to solve systems of equations, returning
        a list of substitutions. Example: the simplex. *)
} (** An expert for some theory *)

let compatible_ord e ~ord = e.expert_ord ord

let update_ctx e ~ctx = e.expert_update_ctx ctx

(** Simple syntactic criterion to decide whether two decision procedures
    are compatibles: check whether they have no symbol in common.
    TODO: more elaborate checks, for instance with ground-joinability of all
    critical pairs *)
let compatible e1 e2 =
  e1.expert_ctx == e2.expert_ctx &&
  SSet.is_empty (SSet.union e1.expert_sig e2.expert_sig)

(** Combine two decision procedures into a new one, that decides
    the combination of their theories, assuming they are compatible. *)
let rec combine e1 e2 =
  assert (compatible e1 e2);
  Utils.debug 3 "%% experts: @[<h>combine %s and %s@]"
    e1.expert_name e2.expert_name;
  (* compute normal form using both systems *)
  let rec nf t =
    let t' = e1.expert_canonize t in
    let t' = e2.expert_canonize t' in
    if t == t' then t' else nf t'
  in
  { expert_name = Utils.sprintf "(%s)_U_(%s)" e1.expert_name e2.expert_name;
    expert_descr =
      Utils.sprintf "@[<hov2>union of@ %s and@ %s@]" e1.expert_descr e2.expert_descr;
    expert_equal = (fun t1 t2 -> nf t1 == nf t2);
    expert_sig = SSet.union e1.expert_sig e2.expert_sig;
    expert_clauses = List.rev_append e1.expert_clauses e2.expert_clauses;
    expert_canonize = nf;
    expert_ord = (fun o -> e1.expert_ord o && e2.expert_ord o);
    expert_update_ctx = (fun ctx ->
      combine (e1.expert_update_ctx ctx) (e2.expert_update_ctx ctx));
    expert_ctx = e1.expert_ctx;
    expert_solve = None;
  }

(** [expert_more_specific e1 e2] returns true if [e1] decides a theory
    whose symbols are included in the theory of [e2]. Heuristically, that
    means that we can ignore [e1] and focus on [e2] *)
let more_specific e1 e2 =
  SSet.subset e1.expert_sig e2.expert_sig &&
  not (SSet.equal e1.expert_sig e2.expert_sig)

(** Get the normal form of the term *)
let canonize expert t = expert.expert_canonize t

let equal expert t1 t2 = expert.expert_equal t1 t2

let signature expert = expert.expert_sig

(** Decide whether this clause is redundant *)
let is_redundant expert hc =
  assert (hc.hcctx == expert.expert_ctx);
  Utils.enter_prof prof_redundant_expert;
  if C.get_flag C.flag_persistent hc then false else
  let ans = Utils.array_exists
    (fun lit -> match lit with
      | Equation (l, r, true, _) when l.sort != bool_ -> expert.expert_equal l r
      | _ -> false)
    hc.hclits
  in
  (if ans then begin
    incr_stat stat_expert_redundant;
    Utils.debug 2 "%% @[<h>%a redundant with %s@]"
      !C.pp_clause#pp_h hc expert.expert_name end);
  Utils.exit_prof prof_redundant_expert;
  ans

(** Simplify the clause *)
let simplify expert hc =
  Utils.enter_prof prof_simplify_expert;
  let ctx = expert.expert_ctx in
  let lits = Array.to_list hc.hclits in
  let lits = List.filter
    (fun lit -> match lit with
      | Equation (l, r, false, _) when expert.expert_equal l r -> false
      | _ -> true)
    lits in
  if List.length lits = Array.length hc.hclits
    then (Utils.exit_prof prof_simplify_expert; hc)  (* no simplification *)
    else begin
      let rule = "expert_" ^ expert.expert_name in
      let premises = List.map (fun hc -> hc.hcproof) expert.expert_clauses in
      let proof c' = Proof (c', rule, hc.hcproof :: premises) in
      let parents = hc :: hc.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx lits proof in
      incr_stat stat_expert_simplify;
      Utils.debug 2 "%% @[<h>theory-simplified %a into %a with %s@]"
                     !C.pp_clause#pp hc !C.pp_clause#pp_h new_hc expert.expert_name;
      (* return simplified clause *)
      Utils.exit_prof prof_simplify_expert;
      new_hc
    end

(** Get a list of clauses this DP needs to be present in the
    superposition prover for it to be complete *)
let clauses expert = 
  let clauses = expert.expert_clauses in
  List.iter (fun hc -> C.set_flag C.flag_persistent hc true) clauses;
  clauses

let pp_expert formatter expert =
  Format.pp_print_string formatter expert.expert_name

let pp_expert_detailed formatter expert =
  Format.fprintf formatter "[expert %s (%s)]"
    expert.expert_name expert.expert_descr

(** {2 Set of experts} *)

module Set = struct
  type expert = t (* alias *)

  type t = expert list
    (** A set of experts *)

  let empty = []

  let add experts e =
    (* traverse [experts], trying to find one that is compatible with e *)
    let rec add left right e =
      match right with  (* TODO if e is more specific than e' in list, drop e *)
      | [] -> e::left (* add the expert *)
      | e'::right' ->
        if compatible e e'
          then (* combine both, and add the combination *)
            add [] (left @ right) (combine e e')
          else (* go further *)
            add (e'::left) right' e
    in
    add [] experts e

  let update_ctx experts ~ctx =
    List.fold_left
      (fun experts' e -> add experts' (update_ctx e ~ctx))
      empty experts

  let is_redundant experts hc =
    List.exists
      (fun e -> compatible_ord e ~ord:e.expert_ctx.ctx_ord && is_redundant e hc)
      experts

  let simplify experts hc =
    List.fold_left
      (fun hc e ->
        if compatible_ord e ~ord:e.expert_ctx.ctx_ord
          then simplify e hc else hc)
      hc experts

  let pp formatter experts =
    Utils.pp_list pp_expert formatter experts
end

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;              (** name of the ordering *)
  gc_theory : string;           (** Theory that is decided *)
  gc_prec : symbol list;        (** Precedence *)
  gc_sig : SSet.t;              (** Symbols of the theory *)
  gc_eqns : hclause list;       (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

let mk_gc ~theory ~ord ~prec hclauses =
  let signature = C.signature hclauses in
  let signature = SMap.filter (fun s _ -> not (is_base_symbol s)) signature in
  (* check that every clause is a positive equation *)
  assert (List.for_all
    (fun hc -> match hc.hclits with 
     | [| Equation (_,_,true,_)|] -> true | _ -> false) hclauses);
  let set = symbols_of_signature signature in
  let set = SSetSeq.of_seq (Sequence.of_list set) in
  { gc_ord = ord;
    gc_theory = theory;
    gc_prec = prec;
    gc_sig = set;
    gc_eqns = hclauses;
  }

(** Instantiate variables in the pairs of terms with fresh constants *)
let ground_pair t1 t2 =
  (* build a grounding substitution *)
  let vars = T.vars_list [t1; t2] in
  let _, subst = List.fold_left
    (fun (i,subst) v ->
      (* bind [v] to a fresh constant *)
      let const = T.mk_const (mk_fresh_const i) v.sort in
      let subst' = S.bind subst (v,0) (const,0) in
      (i+1, subst'))
    (0, S.id_subst) vars in
  let t1' = S.apply_subst subst (t1,0) in
  let t2' = S.apply_subst subst (t2,0) in
  t1', t2'

(** check compatibility of ord with gc.gc_ord,gc.gc_prec! *)
let compatible_gc ~ord gc =
  (* Checks that the precedence of [ord] is compatible with
     the list of symbols [prec] being decreasing *)
  let rec compatible_prec ord prec =
    match prec with
    | [] | [_] -> true
    | x::((y::_) as prec') ->
      ord#precedence#compare x y > 0 && compatible_prec ord prec'
  in
  ord#name = gc.gc_ord && compatible_prec ord gc.gc_prec

(** From a set of ground convergent equations, create an expert for
    the associated theory. *)
let rec gc_expert ~ctx gc =
  (* name and printing stuff *)
  let expert_sig = gc.gc_sig in
  let theory = Utils.sprintf "@[<h>%s_%a@]" gc.gc_theory
    (Sequence.pp_seq ~sep:"_" pp_symbol) (SSetSeq.to_seq expert_sig) in
  let expert_name = Utils.sprintf "gc_%s" theory in
  (* update clauses with the context *)
  let expert_clauses = List.map (C.update_ctx ~ctx) gc.gc_eqns in
  List.iter (fun c -> C.set_flag C.flag_persistent c true) expert_clauses;
  (* make a rewriting system from the clauses *)
  let trs = Rewriting.OrderedTRS.create ~ord:ctx.ctx_ord in
  Rewriting.OrderedTRS.add_seq trs (Sequence.of_list expert_clauses);
  (* compute normal form using the rewriting system *)
  let nf t = Rewriting.OrderedTRS.rewrite trs t in
  (* equality is equality of grounded normal forms *)
  let expert_equal t1 t2 =
    Utils.enter_prof prof_ground_pair;
    let t1', t2' = ground_pair t1 t2 in
    Utils.exit_prof prof_ground_pair;
    Utils.enter_prof prof_normal_form;
    let t1' = nf t1' in
    let t2' = nf t2' in
    Utils.exit_prof prof_normal_form;
    Utils.debug 3 "%% %s: check equal @[<h>%a,%a@]"
      expert_name !T.pp_term#pp t1 !T.pp_term#pp t2;
    t1' == t2' in
  let expert_canonize t = nf t in
  { expert_name;
    expert_descr=("ground convergent system of equations for the theory " ^ theory);
    expert_equal;
    expert_sig;
    expert_clauses;
    expert_canonize;
    expert_ord = (fun o -> compatible_gc ~ord:o gc);
    expert_update_ctx = (fun ctx' -> gc_expert ~ctx:ctx' gc);
    expert_ctx = ctx;
    expert_solve=None;
  }

(** Pretty-print the system of ground convergent equations *)
let pp_gc formatter gc =
  Format.fprintf formatter "@[<h>%s(%d equations, ord %s(%a))@]"
    gc.gc_theory (List.length gc.gc_eqns) gc.gc_ord
    (Utils.pp_list ~sep:">" pp_symbol) gc.gc_prec

(** {3 JSON encoding} *)

let gc_to_json gc = failwith "TODO: Experts.gc_to_json" (* TODO *)

let gc_of_json ~ctx json = failwith "TODO: Experts.gc_of_json" (* TODO *)

(** {2 Some builtin theories} *)

(** Theory of Associative-Commutative symbols, for the given symbol *)
let ac ~ctx f = 
  (* function that computes the AC(f)-normal form of the term *)
  let is_ac s = s == f in
  let expert_canonize t = T.ac_normal_form ~is_ac t in
  let expert_equal t1 t2 = T.ac_eq ~is_ac t1 t2 in
  let rec expert ctx = {
    expert_name = Utils.sprintf "AC_%s" (name_symbol f);
    expert_descr = Utils.sprintf "AC for symbol %s" (name_symbol f);
    expert_equal;
    expert_sig = SSet.singleton f;
    expert_clauses = []; (* TODO *)
    expert_canonize;
    expert_ord = (fun _ -> true);
    expert_update_ctx = (fun ctx' -> expert ctx');
    expert_ctx = ctx;
    expert_solve = None;
  } in
  expert ctx
