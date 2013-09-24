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

(** {1 Term rewriting} *)


module T = Term
module F = Formula
module S = Substs

let prof_ordered_rewriting = Util.mk_profiler "rewriting.ordered"
let stat_ordered_rewriting = Util.mk_stat "rewriting.ordered.steps"

(** {2 Ordered rewriting} *)

module TermHASH = struct
  type t = Term.t
  let equal = (==)
  let hash t = t.T.tag
end

(** Memoization cache for rewriting *)
module TCache = Cache.Replacing(TermHASH)

module type ORDERED = sig
  type t

  module E : Index.EQUATION

  val empty : ord:Ordering.t -> t
  
  val add : t -> E.t -> t
  val add_seq : t -> E.t Sequence.t -> t
  val add_list : t -> E.t list -> t
  
  val to_seq : t -> E.t Sequence.t

  val size : t -> int
  
  val mk_rewrite : t -> size:int -> (Term.t -> Term.t)
    (** Given a TRS and a cache size, build a memoized function that
        performs term rewriting *)
end

module MakeOrdered(E : Index.EQUATION with type rhs = Term.t) = struct
  module E = E

  type rule = {
    rule_left : Term.t;     (** Pattern *)
    rule_right : Term.t;    (** Result *)
    rule_oriented : bool;   (** Is the rule already oriented? *)
    rule_equation: E.t;     (** User-defined equation *)
  } (** A rule, oriented or not *)

  let rule_priority rule =
    (* better priority for oriented rules *)
    if rule.rule_oriented then 1 else 2

  let eq_rule r1 r2 =
    r1.rule_oriented = r2.rule_oriented &&
    r1.rule_left == r2.rule_left &&
    r1.rule_right == r2.rule_right &&
    E.equal r1.rule_equation r2.rule_equation

  module DT = Dtree.Make(struct
    type t = rule
    type rhs = Term.t
    let equal = eq_rule
    let extract r = r.rule_left, r.rule_right, true
    let priority = rule_priority
  end)

  type t = {
    ord : Ordering.t;
    mutable rules : DT.t;
  } (** Ordered rewriting system *)

  let empty ~ord = {
    ord;
    rules = DT.empty;
  }

  let mk_rule eqn l r oriented =
    { rule_equation=eqn; rule_left=l; rule_right=r; rule_oriented=oriented; }

  (** Extract a list of rules from the clause *)
  let rules_of_eqn trs eqn =
    let l, r, sign = E.extract eqn in
    if sign
      then
        match Ordering.compare trs.ord l r with
        | Comparison.Gt -> [ mk_rule eqn l r true ]
        | Comparison.Lt -> [ mk_rule eqn r l true ]
        | Comparison.Eq -> []
        | Comparison.Incomparable -> [ mk_rule eqn l r false; mk_rule eqn r l false ]
      else []   (* negative equation *)

  let add trs eqn =
    let l = rules_of_eqn trs eqn in
    { trs with rules = List.fold_left
      (fun rules rule ->
        (* add the rule to the list of rules *)
        let rules = DT.add rules rule in
        rules)
      trs.rules l;
    }

  let add_seq trs seq =
    Sequence.fold add trs seq

  let add_list trs l =
    List.fold_left add trs l

  let to_seq trs =
    let rules = trs.rules in
    Sequence.from_iter
      (fun k ->
        DT.iter rules (fun _ rule -> k rule.rule_equation))

  let size trs = DT.size trs.rules
  
  exception RewrittenInto of Term.t

  (** Given a TRS and a cache size, build a memoized function that
      performs term rewriting *)
  let mk_rewrite trs ~size =
    (* reduce to normal form. [reduce'] is the memoized version of reduce. *)
    let rec reduce reduce' t =
      match t.T.term with
      | T.Var _ | T.BoundVar _ -> t
      | T.Bind (s, t') ->
        let t' = reduce' t' in
        T.mk_bind s t'
      | T.Node (s, l) ->
        let l' = List.map reduce' l in
        let t' = if List.for_all2 (==) l l'
          then t
          else T.mk_node s l' in
        (* now rewrite the term itself *)
        rewrite_here reduce' t'
      | T.At (t1, t2) ->
        let t1' = reduce' t1 in
        let t2' = reduce' t2 in
        let t' = T.mk_at t1' t2' in
        rewrite_here reduce' t'
    (* rewrite once at this position. If it succeeds,
       yields back to [reduce]. *)
    and rewrite_here reduce' t =
      try
        DT.retrieve ~sign:true (trs.rules,1) (t,0) ()
          (fun () _ _ rule subst ->
            (* right-hand part *)
            let r = rule.rule_right in
            let r' = S.apply subst r 1 in
            if rule.rule_oriented
              then raise (RewrittenInto r')  (* we know that t > r' *)
              else (
                assert (t == S.apply subst rule.rule_left 1);
                if Ordering.compare trs.ord t r' = Comparison.Gt
                  then raise (RewrittenInto r')
                  else ()));
        t (* could not rewrite t *)
      with RewrittenInto t' ->
        Util.debug 3 "rewrite %a into %a" T.pp t T.pp t';
        Util.incr_stat stat_ordered_rewriting;
        assert (Ordering.compare trs.ord t t' = Comparison.Gt);
        reduce reduce' t'  (* term is rewritten, reduce it again *)
    in
    let cache = TCache.create size in
    let reduce = TCache.with_cache_rec cache reduce in
    (* The main rewriting function *)
    let rewrite t =
      Util.enter_prof prof_ordered_rewriting;
      let t' = reduce t in
      Util.exit_prof prof_ordered_rewriting;
      t'
    in
    rewrite
end

(** {2 Regular rewriting} *)

module TRS = struct
  type rule = Term.t * Term.t

  (** Instance of discrimination tree indexing} *)
  module DT = Dtree.Make(struct
    type t = rule
    type rhs = Term.t
    let equal (l1,r1) (l2,r2) = l1 == l2 && r1 == r2
    let extract (l,r) = (l,r,true)
    let priority _ = 1
  end)

  type t = DT.t
    (** Term Rewriting System *)

  let empty = DT.empty

  let add trs (l, r) =
    (* check that the rule does not introduce variables *)
    assert (List.for_all (fun v -> T.subterm ~sub:v l) (T.vars r));
    assert (not (T.is_var l));
    (* add rule to the discrimination tree *)
    let trs = DT.add trs (l, r) in
    trs

  let add_seq trs seq =
    Sequence.fold add trs seq

  let add_list trs l =
    List.fold_left add trs l

  let size trs = DT.size trs

  let iter trs k =
    DT.iter trs (fun _ rule -> k rule)

  let to_seq trs =
    Sequence.from_iter
      (fun k -> iter trs k)

  let of_seq seq =
    add_seq empty seq

  let of_list l =
    add_list empty l

  (** {2 Computation of normal forms} *)

  exception RewrittenIn of Term.t * Substs.t

  (** Compute normal form of the term, and set its binding to the normal form *)
  let rewrite ?(depth=0) trs t = 
    (* compute normal form of [subst(t, offset)] *)
    let rec compute_nf ~depth subst t offset =
      match t.T.term with
      | T.Bind (s, t') ->
        let t'' = compute_nf ~depth:(depth+1) subst t' offset in
        let new_t = T.mk_bind s t'' in
        reduce_at_root ~depth new_t
      | T.Node (hd, l) ->
        (* rewrite subterms first *)
        let l' = List.map (fun t' -> compute_nf ~depth subst t' offset) l in
        let t' = T.mk_node hd l' in
        (* rewrite at root *)
        reduce_at_root ~depth t'
      | T.Var _ -> S.apply ~depth subst t offset  (* normal form in subst *)
      | T.BoundVar _ -> t
      | T.At (t1, t2) ->
        let t1' = compute_nf ~depth subst t1 offset in
        let t2' = compute_nf ~depth subst t2 offset in
        let t' = T.mk_at t1' t2' in
        reduce_at_root ~depth t'
    (* assuming subterms of [t] are in normal form, reduce the term *)
    and reduce_at_root ~depth t =
      try
        DT.retrieve ~sign:true (trs,1) (t,0) () rewrite_handler;
        t  (* normal form *)
      with (RewrittenIn (t', subst)) ->
        Util.debug 3 "rewrite %a into %a (with %a)" T.pp t T.pp t' Substs.pp subst;
        compute_nf ~depth subst t' 1  (* rewritten into subst(t',1), continue *)
    (* attempt to use one of the rules to rewrite t *)
    and rewrite_handler () l r rule subst =
      let t' = r in
      raise (RewrittenIn (t', subst))
    in
    let t' = compute_nf ~depth S.empty t 0 in
    t'
end

(** {2 Formula Rewriting } *)

module FormRW = struct
  type rule = Term.t * Formula.t

  (** Instance of discrimination tree indexing} *)
  module DT = Dtree.Make(struct
    type t = rule
    type rhs = Formula.t
    let equal (l1,r1) (l2,r2) = T.eq l1 l2 && F.eq r1 r2
    let extract (l,r) = (l,r,true)
    let priority (l,r) = F.weight r
  end)

  type t = DT.t
    (** Term Rewriting System *)

  let empty = DT.empty

  let add trs (l, r) =
    (* check that the rule does not introduce variables *)
    assert (List.for_all (fun v -> T.subterm ~sub:v l) (F.free_variables r));
    assert (not (T.is_var l));
    (* add rule to the discrimination tree *)
    let trs = DT.add trs (l, r) in
    trs

  let add_seq trs seq =
    Sequence.fold add trs seq

  let add_list trs l =
    List.fold_left add trs l

  let add_term_rule trs (l,r) =
    let rule = l, F.mk_atom r in
    add trs rule

  let add_term_rules trs seq =
    List.fold_left add_term_rule trs seq

  let size trs = DT.size trs

  let iter trs k =
    DT.iter trs (fun _ rule -> k rule)

  let to_seq trs =
    Sequence.from_iter
      (fun k -> iter trs k)

  let of_seq seq =
    add_seq empty seq

  let of_list l =
    add_list empty l

  (** {2 Computation of normal forms} *)

  exception RewrittenIn of Formula.t * Substs.t

  (** Compute normal form of the formula *)
  let rewrite ?(depth=0) frs f =
    let rec compute_nf ~depth subst f scope =
      F.map_leaf_depth ~depth
        (fun depth leaf -> match leaf.F.form with
          | F.Atom p ->
            (* rewrite atoms *)
            let p' = S.apply ~depth subst p scope in
            reduce_at_root ~depth p'
          | F.Equal (l,r) ->
            F.mk_eq (S.apply ~depth subst l scope) (S.apply ~depth subst r scope)
          | _ -> leaf)
        f
    (* try to rewrite this term at root *)
    and reduce_at_root ~depth t =
      try
        DT.retrieve ~sign:true (frs,1) (t,0) () rewrite_handler;
        F.mk_atom t  (* normal form is itself *)
      with (RewrittenIn (f, subst)) ->
        Util.debug 3 "rewrite %a into %a (with %a)" T.pp t F.pp f Substs.pp subst;
        compute_nf ~depth subst f 1  (* rewritten into subst(t',1), continue *)
    (* attempt to use one of the rules to rewrite t *)
    and rewrite_handler () l r rule subst =
      raise (RewrittenIn (r, subst))
    in
    let f = compute_nf ~depth S.empty f 0 in
    Util.debug 3 "normal form: %a" F.pp f;
    f
end
