
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

(** Decision procedures for theories *)

open Logtk

module T = Term
module C = Clause
module S = Substs

(** {2 General interface} *)

let stat_expert_redundant = Util.mk_stat "experts.redundant"
let stat_expert_simplify = Util.mk_stat "experts.simplify"

let prof_simplify_expert = Util.mk_profiler "experts.simplify"
let prof_redundant_expert = Util.mk_profiler "experts.redundant"
let prof_ground_pair = Util.mk_profiler "experts.ground_pair"
let prof_normal_form = Util.mk_profiler "experts.normal_form"

type t = {
  expert_name : string;                 (** Theory the expert works on *)
  expert_descr : string;                (** Description of the expert *)
  expert_equal : Term.t -> Term.t -> bool;  (** Check whether two terms are equal *)
  expert_sig : Symbol.SSet.t;           (** Symbols of the theory *)
  expert_clauses : Clause.t list;        (** Additional axioms *)
  expert_canonize : Term.t -> Term.t;       (** Get a canonical form of the term *)
  expert_ord : Ordering.t -> bool;        (** Compatible with ord? *)
  expert_update_ctx : Ctx.t -> t list;(** How to update the context *)
  expert_ctx : Ctx.t;                 (** Context used by the expert *)
  expert_solve : ((Term.t*Term.t) list -> Substs.t list) option;
    (** The expert may be able to solve systems of equations, returning
        a list of substitutions. Example: the simplex. *)
} (** An expert for some theory *)

let compatible_ord e ~ord = e.expert_ord ord

(** Copy of the expert, that uses the new context. The expert
    can be broken into several experts (in case it was a combination
    that is no longer possible with the new ordering) *)
let update_ctx e ~ctx = e.expert_update_ctx ctx

(* TODO also check that ordering constraint of e1 and e2 are compatible *)
(* TODO: more elaborate checks, for instance with ground-joinability of all
    critical pairs *)
(** Simple syntactic criterion to decide whether two decision procedures
    are compatibles: check whether they have no symbol in common. *)
let compatible e1 e2 =
  e1.expert_ctx == e2.expert_ctx &&
  compatible_ord e1 ~ord:(Ctx.ord e1.expert_ctx) =
    compatible_ord e2 ~ord:(Ctx.ord e2.expert_ctx) &&
  Symbol.SSet.is_empty (Symbol.SSet.inter e1.expert_sig e2.expert_sig)

(** Combine two decision procedures into a new one, that decides
    the combination of their theories, assuming they are compatible. *)
let rec combine e1 e2 =
  assert (compatible e1 e2);
  Util.debug 1 "%% @[<h>experts: combine %s and %s@]"
    e1.expert_name e2.expert_name;
  (* compute normal form using both systems *)
  let rec nf t =
    let t' = e1.expert_canonize t in
    let t' = e2.expert_canonize t' in
    if t == t' then t' else nf t'
  and expert_equal t1 t2 =
    let t1' = nf t1 and t2' = nf t2 in
    t1' == t2' || e1.expert_equal t1' t2' || e2.expert_equal t1' t2'
  in
  { expert_name = Utils.sprintf "(%s)_U_(%s)" e1.expert_name e2.expert_name;
    expert_descr =
      Utils.sprintf "@[<hov2>union of@ %s and@ %s@]" e1.expert_descr e2.expert_descr;
    expert_equal;
    expert_sig = Symbol.SSet.union e1.expert_sig e2.expert_sig;
    expert_clauses = List.rev_append e1.expert_clauses e2.expert_clauses;
    expert_canonize = nf;
    expert_ord = (fun o -> e1.expert_ord o && e2.expert_ord o);
    expert_update_ctx = (fun ctx ->
      e1.expert_update_ctx ctx @ e2.expert_update_ctx ctx);
    expert_ctx = e1.expert_ctx;
    expert_solve = None;
  }

(** Get the normal form of the term *)
let canonize expert t = expert.expert_canonize t

let equal expert t1 t2 =
  let t1' = canonize expert t1
  and t2' = canonize expert t2 in
  (* expert.expert_equal may be more than just == of canonized forms *)
  t1' == t2' || expert.expert_equal t1' t2' 

let signature expert = expert.expert_sig

(** Decide whether this clause is redundant *)
let is_redundant expert c =
  assert (c.C.hcctx == expert.expert_ctx);
  Util.enter_prof prof_redundant_expert;
  if C.get_flag C.flag_persistent c then false else
  let ans = Util.array_exists
    (fun lit -> match lit with
      | Literal.Equation (l, r, true, _) (* FIXME when l.sort != bool_ *) -> equal expert l r
      | _ -> false)
    c.C.hclits
  in
  (if ans then begin
    Util.incr_stat stat_expert_redundant;
    Util.debug 2 "%a redundant with %s" C.pp c expert.expert_name
    end);
  Util.exit_prof prof_redundant_expert;
  ans

(** Simplify the clause *)
let simplify expert c =
  Util.enter_prof prof_simplify_expert;
  let ctx = expert.expert_ctx in
  let lits = Array.to_list c.C.hclits in
  let lits = List.filter
    (fun lit -> match lit with
      | Literal.Equation (l, r, false, _) when equal expert l r -> false
      | _ -> true)
    lits in
  if List.length lits = Array.length c.C.hclits
    then (Util.exit_prof prof_simplify_expert; c)  (* no simplification *)
    else begin
      let rule = "expert_" ^ expert.expert_name in
      let premises = List.map (fun c -> c.C.hcproof) expert.expert_clauses in
      let proof c' = Proof.mk_c_step c' rule (c.C.hcproof :: premises) in
      let parents = c :: c.C.hcparents in
      let new_hc = C.create ~parents ~ctx lits proof in
      Util.incr_stat stat_expert_simplify;
      Util.debug 2 "theory-simplified %a into %a with %s"
        C.pp c C.pp new_hc expert.expert_name;
      (* return simplified clause *)
      Util.exit_prof prof_simplify_expert;
      new_hc
    end

(** Get a list of clauses this DP needs to be present in the
    superposition prover for it to be complete *)
let clauses expert = 
  let clauses = expert.expert_clauses in
  List.iter (fun c -> C.set_flag C.flag_persistent c true) clauses;
  clauses

let pp buf expert =
  Buffer.add_string buf expert.expert_name

let to_string e =
  Util.on_buffer pp e

let fmt fmt e =
  Format.pp_print_string fmt (to_string e)

let pp_expert_detailed formatter expert =
  Format.fprintf formatter "[expert %s (%s)]"
    expert.expert_name expert.expert_descr

(** {2 Set of experts} *)

module Set = struct
  type expert = t (* alias *)

  type t = {
    active : expert list;
    inactive : expert list;
    ctx : Ctx.t;
  }

    (** A set of experts *)

  let empty ~ctx = {
    active = [];
    inactive = [];
    ctx;
  }

  (* TODO investigate possible bug: some experts are disabled? *)

  let add_list set experts =
    (* traverse [right], trying to find one that is compatible with [e].
      [left] contains experts that have been already traversed. *)
    let rec add left right e =
      match right with
      | [] -> e::left (* add the expert *)
      | e'::right' ->
        if compatible e e'
          then (* combine both, and add the combination *)
            add [] (left @ right') (combine e e')
          else (* go further *)
            add (e'::left) right' e
    in
    (* update context of experts *)
    let experts = Util.list_flatmap
      (fun e -> update_ctx ~ctx:set.ctx e) experts in
    List.fold_left
      (fun set e ->
        if not (compatible_ord e ~ord:(Ctx.ord set.ctx))
          then (* [e] is disabled, not compatible with [ctx] *)
            let _ = Util.debug 2 "expert %a disabled" pp e in
            {set with inactive = e :: set.inactive; }
          else
            (* add [e] to the active experts *)
            let _ = Util.debug 2 "expert %a enabled" pp e in
            let active = add [] set.active e in
            {set with active; })
      set experts

  let add set e = add_list set [e]

  let iter e f =
    List.iter f e.active;
    List.iter f e.inactive

  let to_seq e = Sequence.append
    (Sequence.of_list e.active)
    (Sequence.of_list e.inactive)

  let of_seq e seq =
    Sequence.fold add e seq

  let size e = Sequence.length (to_seq e)

  let update_ctx set ~ctx =
    let set' = {ctx; active=[]; inactive=[]; } in
    let set' = add_list set' set.active in
    let set' = add_list set' set.inactive in
    set'

  let is_redundant set c =
    List.exists
      (fun e ->
        assert (compatible_ord e ~ord:(Ctx.ord e.expert_ctx));
        is_redundant e c)
      set.active

  let simplify set c =
    List.fold_left
      (fun c e ->
        assert (compatible_ord e ~ord:(Ctx.ord e.expert_ctx));
        simplify e c)
      c set.active

  let pp buf set =
    Printf.bprintf buf "{active: %a, inactive: %a}"
      (Util.pp_list pp) set.active (Util.pp_list pp) set.inactive

  let fmt fmt set =
    Format.pp_print_string fmt (Util.on_buffer pp set)
end

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;              (** name of the ordering *)
  gc_theory : string;           (** Theory that is decided *)
  gc_prec : Symbol.t list;        (** Precedence *)
  gc_sig : Symbol.SSet.t;              (** Symbols of the theory *)
  gc_eqns : Clause.t list;       (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

let mk_gc ~theory ~ord ~prec clauses =
  let signature = C.signature (Sequence.of_list clauses) in
  let signature = Symbol.SMap.filter
    (fun s _ -> not (Symbol.is_connective s)) signature in
  (* check that every clause is a positive equation *)
  assert (List.for_all
    (fun c -> match c.C.hclits with 
     | [| Literal.Equation (_,_,true,_)|] -> true | _ -> false) clauses);
  let set = Signature.to_symbols signature in
  let set = Symbol.SSet.of_seq (Sequence.of_list set) in
  { gc_ord = ord;
    gc_theory = theory;
    gc_prec = prec;
    gc_sig = set;
    gc_eqns = clauses;
  }

(** Instantiate variables in the pairs of terms with fresh constants *)
let ground_pair t1 t2 =
  (* build a grounding substitution *)
  let vars = T.vars_list [t1; t2] in
  let _, subst = List.fold_left
    (fun (i,subst) v ->
      (* bind [v] to a fresh constant *)
      let const = T.mk_const (Symbol.mk_fresh_const i) in
      let subst' = S.bind subst v 0 const 0 in
      (i+1, subst'))
    (0, S.empty) vars in
  let t1' = S.apply subst t1 0 in
  let t2' = S.apply subst t2 0 in
  t1', t2'

(** Same as [ground_pair], but with a cache *)
let cached_ground_pair =
  let cache = T.T2Cache.create 256 in
  fun t1 t2 ->
    T.T2Cache.with_cache cache ground_pair t1 t2

(** check compatibility of ord with gc.gc_ord,gc.gc_prec! *)
let compatible_gc ~ord gc =
  (* Checks that the precedence of [ord] is compatible with
     the list of symbols [prec] being decreasing *)
  let rec compatible_prec ord prec =
    match prec with
    | [] | [_] -> true
    | x::((y::_) as prec') ->
      Precedence.compare ord.Ordering.ord_precedence x y > 0 &&
      compatible_prec ord prec'
  in
  Ordering.name ord = gc.gc_ord && compatible_prec ord gc.gc_prec

module OrderedTRS = Rewriting.MakeOrdered(struct
  type t = Clause.t
  let equal = C.eq
  let extract c = match c.C.hclits with
    | [| Literal.Equation (l, r, sign, _) |] -> l, r, sign
    | _ -> assert false
  let priority _ = 1
end)

(** From a set of ground convergent equations, create an expert for
    the associated theory. *)
let rec gc_expert ~ctx gc =
  (* name and printing stuff *)
  let expert_sig = gc.gc_sig in
  let theory = Util.sprintf "%s_%a" gc.gc_theory
    (Util.pp_seq ~sep:"_" Symbol.pp) (Symbol.SSet.to_seq expert_sig) in
  let expert_name = Utils.sprintf "gc_%s" theory in
  (* update clauses with the context *)
  let expert_clauses = List.map (C.update_ctx ~ctx) gc.gc_eqns in
  List.iter (fun c -> C.set_flag C.flag_persistent c true) expert_clauses;
  (* make a rewriting system from the clauses *)
  let trs = OrderedTRS.empty ~ord:(Ctx.ord ctx) in
  let trs = OrderedTRS.add_seq trs (Sequence.of_list expert_clauses) in
  (* compute normal form using the rewriting system *)
  let nf = OrderedTRS.mk_rewrite trs ~size:2048 in
  (* equality is equality of grounded normal forms *)
  let expert_equal t1 t2 =
    Util.enter_prof prof_ground_pair;
    let t1', t2' = cached_ground_pair t1 t2 in
    Util.exit_prof prof_ground_pair;
    Util.enter_prof prof_normal_form;
    let t1' = nf t1' in
    let t2' = nf t2' in
    Util.exit_prof prof_normal_form;
    Util.debug 3 "%s: check equal %a,%a" expert_name T.pp t1 T.pp t2;
    t1' == t2' in
  let expert_canonize t = nf t in
  { expert_name;
    expert_descr=("ground convergent system of equations for the theory " ^ theory);
    expert_equal;
    expert_sig;
    expert_clauses;
    expert_canonize;
    expert_ord = (fun o -> compatible_gc ~ord:o gc);
    expert_update_ctx = (fun ctx' -> [gc_expert ~ctx:ctx' gc]);
    expert_ctx = ctx;
    expert_solve=None;
  }

(** Pretty-print the system of ground convergent equations *)
let pp_gc buf gc =
  Printf.bprintf buf "%s(%d equations, ord %s(%a))"
    gc.gc_theory (List.length gc.gc_eqns) gc.gc_ord
    (Util.pp_list ~sep:">" Symbol.pp) gc.gc_prec

let fmt_gc fmt gc =
  Format.pp_print_string fmt (Util.on_buffer pp_gc gc)

(** {2 Some builtin theories} *)

(** Theory of Associative-Commutative symbols, for the given symbol *)
let ac ~ctx f = 
  (* function that computes the AC(f)-normal form of the term *)
  let is_ac s = s == f in
  let expert_canonize t = T.ac_normal_form ~is_ac t in
  let expert_equal t1 t2 = T.ac_eq ~is_ac t1 t2 in
  let rec expert ctx = {
    expert_name = Util.sprintf "AC_%s" (Symbol.to_string f);
    expert_descr = Util.sprintf "AC for symbol %s" (Symbol.to_string f);
    expert_equal;
    expert_sig = Symbol.SSet.singleton f;
    expert_clauses = []; (* TODO *)
    expert_canonize;
    expert_ord = (fun _ -> true);
    expert_update_ctx = (fun ctx' -> [expert ctx']);
    expert_ctx = ctx;
    expert_solve = None;
  } in
  expert ctx
