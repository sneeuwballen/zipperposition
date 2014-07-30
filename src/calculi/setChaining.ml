
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

(** {1 Chaining on Sets} *)

open Logtk

module Lit = Literal

(** {2 Inference Rules} *)
module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  val idx_left : unit -> PS.TermIndex.t     (* terms at LHS of subseteq *)
  val idx_right : unit -> PS.TermIndex.t    (* terms at RHS of subseteq *)

  val preprocess : Formula.FO.t -> Formula.FO.t
  (** Preprocessing of formula, during CNF, to remove most set operators,
      keeping only those of the form
      a \cap b \cap ...  \subseteq a' \cup b' \cup ... *)

  val positive_chaining: Env.binary_inf_rule
    (* positive chaining *)

  val negative_chaining_left: Env.binary_inf_rule
    (* negative chaining where the left side of literals are treated *)

  val negative_chaining_right: Env.binary_inf_rule
    (* negative chaining where the right side of literals are treated *)

  val reflexivity_res: Env.unary_inf_rule
    (* reflexivity resolution *)

  val factoring_left: Env.unary_inf_rule
    (* factoring terms that are on the left side of literals *)

  val rewrite_set_eq: Env.multi_simpl_rule
    (* rewrite A=B into A subseteq B and B subseteq A *)

  val rewrite_set_neq: Env.multi_simpl_rule
    (* rewrite A!=B into A notsubseteq B or B notsubseteq A *)

  val singleton_pos: Env.rw_simplify_rule
    (* choice of a witness for all terms appearing in a singleton on the left
     * side of a subseteq *)

  val singleton_neg: Env.multi_simpl_rule
    (* choice of a witness for all terms appearing in a singleton on the left
     * side of a notsubseteq *)

  val singleton_elim: Env.multi_simpl_rule
    (* eliminates a variable that appears in singletons only on the left side
     * of set literals and in equalities *)

  val var_elim: Env.multi_simpl_rule
    (* eliminates a variable that appears only on the left side of set
     * literals *)

  val reflexivity: Env.is_trivial_rule
    (* reflexivity tautology : when the same term appears in each side of a
     * positive literal*)

  val is_tautology: Env.is_trivial_rule
    (* finds tautologies of the form : A \subseteq B or A \not\subseteq B
     * the positive literal can have stronger constraints *)

  val is_absurd: Env.lit_rewrite_rule
    (* eliminates negative literals that have the same term appearing in each
     * side *)

  val setup : unit -> unit
end

(* global theory currently in use *)
let _theory = ref Theories.Sets.default

module Make(E : Env.S) = struct
  module Env = E
  module C = Env.C
  module PS = Env.ProofState
  module Ctx = Env.Ctx
  module F = Formula.FO
  module TS = Theories.Sets
  module S = Substs
  module Lits = Literals
  module Lit = Literal
  module I = PS.TermIndex

  let sets = !_theory

  type scope = S.scope

  let _idx_left = ref (PS.TermIndex.empty ())
  let _idx_right = ref (PS.TermIndex.empty ())

  let idx_left () = !_idx_left
  let idx_right () = !_idx_right

  let _update_idx f c =
    let ord = Ctx.ord () in
    (* terms appearing under a subseteq *)
    let left, right = Lits.fold_subseteq
      ~eligible:(C.Eligible.(filter Lit.is_subseteq)) (C.lits c) (idx_left (), idx_right ())
      (fun (left,right) lit position ->
        Lit.fold_terms ~position ~which:`All ~ord ~subterms:false lit (left,right)
        (fun (left,right) term pos ->
          match pos with
          | Position.Arg(_,Position.Left _) ->
            let with_pos = C.WithPos.( {term; pos; clause=c} ) in
            (f left term with_pos,right)
          | Position.Arg(_,Position.Right _) ->
            let with_pos = C.WithPos.( {term; pos; clause=c} ) in
            (left,f right term with_pos)
          | _ -> left,right)
      )
    in
    _idx_left := left;
    _idx_right := right;
    ()

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
        _update_idx PS.TermIndex.add c;
        Signal.ContinueListening);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
        _update_idx PS.TermIndex.remove c;
        Signal.ContinueListening);
    ()

  type sets_list = {
    sets : F.term list;
    comp : F.term list;
    empty : bool;
    ty : Type.ty
  }

  (** Preprocessing of set terms in both sides of a \subseteq
      and returns a list of list of sets.
      If ~left, the result is considered as a list of intersections of sets;
      otherwise, it is a list of unions of sets
    *)
  let rec preprocess_subseteq ~sets ~left s_list acc =
    match s_list with
      | s::l ->
        let vs = TS.view ~sets s in
        begin match vs with
          | TS.Union s_list' ->
            if left then begin
              Util.debug 3 " --- A inter (B union C) subseteq D --> %s"
                "(A inter B subseteq D) and (A inter C subseteq D)";
              let rec aux l_aux acc_aux =
                match l_aux with
                  | h::t -> aux t ((preprocess_subseteq ~sets ~left (h::l) acc)@acc_aux)
                  | [] -> acc_aux
              in aux s_list' []
            end else begin
              Util.debug 3 " --- A union (B union C) --> A union B union C";
              preprocess_subseteq ~sets ~left s_list' (preprocess_subseteq ~sets ~left l acc)
            end
          | TS.Inter s_list' ->
            if left then begin
              Util.debug 3 " --- A inter (B inter C) --> A inter B inter C";
              preprocess_subseteq ~sets ~left s_list' (preprocess_subseteq ~sets ~left l acc)
            end else begin
              Util.debug 3 " --- A subseteq B union (C inter D) --> %s"
                "(A subseteq B union C) and (A subseteq B union D)";
              let rec aux l_aux acc_aux =
                match l_aux with
                  | h::t -> aux t ((preprocess_subseteq ~sets ~left (h::l) acc)@acc_aux)
                  | [] -> acc_aux
              in aux s_list' []
            end
          | TS.Diff (s1,s2) ->
            Util.debug 3 " --- A diff B --> A inter comp(B)";
            preprocess_subseteq ~sets ~left
              ((TS.mk_inter ~sets [s1;(TS.mk_complement ~sets s2)])::l)
              acc
          | TS.Singleton x ->
            Util.debug 3 " --- {x} --> {x}";
            preprocess_subseteq ~sets ~left l
              (List.map
                (fun sr -> {sets = (s::sr.sets);comp = sr.comp;empty = false; ty = sr.ty})
                acc)
         | TS.Emptyset ty ->
            if left then begin
              Util.debug 3 " --- A inter empty --> empty";
              [{sets = []; comp = []; empty = true; ty = ty}]
            end else begin
              Util.debug 3 " --- A union empty --> A";
              preprocess_subseteq ~sets ~left l acc
            end
          | TS.Complement s' ->
            let vs' = TS.view ~sets s' in
            begin match vs' with
              | TS.Union s_list' ->
                Util.debug 3 " --- comp(A union B) --> comp(A) inter comp(B)";
                preprocess_subseteq ~sets ~left
                  (TS.mk_inter ~sets (List.map (fun x -> TS.mk_complement ~sets x) s_list')::l)
                  acc
              | TS.Inter s_list' ->
                Util.debug 3 " --- comp(A inter B) --> comp(A) union comp(B)";
                preprocess_subseteq ~sets ~left
                  (TS.mk_union ~sets (List.map (fun x -> TS.mk_complement ~sets x) s_list')::l)
                  acc
              | TS.Diff (s1,s2) ->
                Util.debug 3 " --- comp(A diff B) --> comp(A) union B";
                preprocess_subseteq ~sets ~left
                  ((TS.mk_union ~sets [(TS.mk_complement ~sets s1);s2])::l)
                  acc
              | TS.Singleton x ->
                Util.debug 3 " --- comp({x}) --> comp({x})";
                preprocess_subseteq ~sets ~left l
                  (List.map
                    (fun sr -> {sets = sr.sets; comp = s'::(sr.comp); empty = false; ty = sr.ty})
                    acc)
              | TS.Emptyset ty ->
                if left then begin
                  Util.debug 3 " --- A inter comp(empty) --> A";
                  preprocess_subseteq ~sets ~left l acc
                end else begin
                  Util.debug 3 " --- A union comp(empty) --> comp(empty)";
                  [{sets = []; comp = []; empty = true; ty = ty}]
                end
              | TS.Complement s'' ->
                Util.debug 3 " --- comp(comp(A)) --> A";
                preprocess_subseteq ~sets ~left (s''::l) acc
              | TS.Other _ ->
                Util.debug 3 " --- comp(A) --> comp(A)";
                preprocess_subseteq ~sets ~left l
                  (List.map
                    (fun sr -> {sets = sr.sets; comp = s'::(sr.comp); empty = false; ty = sr.ty})
                    acc)
              | _ -> assert false
            end
          | TS.Other _ ->
            Util.debug 3 " --- A --> A";
            preprocess_subseteq ~sets ~left l
              (List.map
                (fun sr -> {sets = s::(sr.sets); comp = sr.comp; empty = false; ty = sr.ty})
                acc)
          | _ -> assert false
        end
      | [] -> acc

  (** reconstructs the set terms
      returns a list of terms of the form
      A \cap B \cap ... \subseteq A' \cup B' \cup ...
      constructed by doing the cartesian product of left side terms and right side terms
    *)
  let reform_subseteq ~sets left right =
    Util.debug 3 "Reconstruction...";
    let rec aux l r acc = match l,r with
      | [],_ -> acc
      | h::t,[] -> aux t right acc
      | h::t,h'::t' ->
        let h_inter =
          if h.sets = [] && h'.comp = [] then
            TS.mk_empty ~sets h.ty
          else if h.empty || h'.empty then
            TS.mk_empty ~sets h.ty
          else
            TS.mk_inter ~sets (h.sets@h'.comp)
        in let h_union =
          if h.comp = [] && h'.sets = [] then
            TS.mk_empty ~sets h.ty
          else
            TS.mk_union ~sets (h'.sets@h.comp)
        in aux l t' (F.Base.atom ((TS.mk_subseteq ~sets h_inter h_union))::acc)
    in aux left right []

  let rec preprocess f =
    let vf = F.view f in
    match vf with
      | F.True
      | F.False -> f
      | F.Atom t ->
        let vt = TS.view ~sets t in
        begin match vt with
          | TS.Member (x,s) ->
            Util.debug 3 "Found a set of type member -- %s"
              "applying: x in A --> {x} subseteq A";
            preprocess (F.Base.atom (TS.mk_subseteq ~sets (TS.mk_singleton ~sets x) s))
          | TS.Subset (s1,s2) ->
            Util.debug 3 "Found a set of type subset -- %s"
              "applying: A subset B --> A subseteq B and not(B subseteq A)";
            preprocess (F.Base.and_
                [(F.Base.atom (TS.mk_subseteq ~sets s1 s2));
                 (F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets s2 s1)))
                ])
          | TS.Subseteq (s1,s2) ->
            Util.debug 3 "Found a set of type subseteq -- %s"
              "beginning transformation into a conjonction of subseteq clauses";
            let preproc_left =
              preprocess_subseteq ~sets ~left:true [s1]
              [{sets = []; comp = []; empty = false; ty = TS.get_set_type_exn ~sets s1}]
            and preproc_right =
              preprocess_subseteq ~sets ~left:false [s2]
              [{sets = []; comp = []; empty = false; ty = TS.get_set_type_exn ~sets s2}] in
              F.Base.and_ (reform_subseteq ~sets preproc_left preproc_right)
          | TS.Other _ -> f
          | _ -> f
        end
      | F.And f_list -> F.Base.and_ (List.map preprocess f_list)
      | F.Or f_list -> F.Base.or_ (List.map preprocess f_list)
      | F.Not f' ->
        begin match F.view f' with
          | F.Atom t ->
            let vt = TS.view ~sets t in
            begin match vt with
              | TS.Member (x,s) ->
                Util.debug 3 "Found a set of type not member -- %s"
                  "applying x not in A --> not({x} subseteq A)";
                let subseteq_new = TS.mk_subseteq ~sets (TS.mk_singleton ~sets x) s in
                  preprocess (F.Base.not_ (F.Base.atom subseteq_new))
              | TS.Subset (s1,s2) ->
                Util.debug 3 "Found a set of type not subset -- %s"
                  "applying not(A subset B) --> (B subseteq A) or not(A subseteq B)";
                preprocess (F.Base.or_
                  [(F.Base.atom (TS.mk_subseteq ~sets s2 s1));
                   (F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets s1 s2)))
                  ])
              | TS.Subseteq (s1,s2) ->
                Util.debug 3 "Found a set of type not subseteq -- %s"
                  "beginning transformation into a disjonction of not subseteq clauses";
                let preproc_left =
                  preprocess_subseteq ~sets ~left:true [s1]
                    [{sets = []; comp = []; empty = false; ty = TS.get_set_type_exn ~sets s1}]
                and preproc_right =
                  preprocess_subseteq ~sets ~left:false [s2]
                    [{sets = []; comp = []; empty = false; ty = TS.get_set_type_exn ~sets s1}] in
                  F.Base.or_ (List.map (fun x -> F.Base.not_ x)
                    (reform_subseteq ~sets preproc_left preproc_right))
              | TS.Other _ -> f
              | _ -> f
            end
          (* this preprocessing is post-nnf *)
          | _ -> assert false
        end
      | F.Imply (f1,f2) -> F.Base.imply (preprocess f1) (preprocess f2)
      | F.Equiv (f1,f2) -> F.Base.equiv (preprocess f1) (preprocess f2)
      | F.Xor (f1,f2) -> F.Base.xor (preprocess f1) (preprocess f2)
      | F.Eq (t1,t2) ->
        if TS.is_set ~sets t1
        then begin
        Util.debug 3 "Found a set of type equals -- %s"
          "applying A = B --> (A subseteq B) and (B subseteq A)";
          preprocess (F.Base.and_
            [(F.Base.atom (TS.mk_subseteq ~sets t1 t2));
             (F.Base.atom (TS.mk_subseteq ~sets t2 t1))]
          )
        end else f
      | F.Neq (t1,t2) ->
        if TS.is_set ~sets t1
        then begin
          Util.debug 3 "Found a set of type not equals -- %s"
            "applying A <> B --> not(A subseteq B) or not(B subseteq A)";
          preprocess (F.Base.or_
            [(F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets t1 t2)));
             (F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets t2 t1)))]
          )
        end else f
      | F.Forall (t,f') -> F.Base.__mk_forall t (preprocess f')
      | F.Exists (t,f') -> F.Base.__mk_exists t (preprocess f')
      | F.ForallTy f' -> F.Base.__mk_forall_ty (preprocess f')

  (** remove the term at the given position of a subseteq literal *)
  let remove_term_at lit pos =
    let module P = Position in
    match lit,pos with
      | Lit.Subseteq(sets,l,r,sign), P.Left (P.Arg(i,P.Stop)) ->
        let seq = Sequence.of_list l in
        Lit.mk_subseteq ~sign ~sets (Sequence.foldi
          (fun l j elt -> if (i = j) then l else elt::l) [] seq) r
      | Lit.Subseteq(sets,l,r,sign),P.Right (P.Arg(i,P.Stop)) ->
        let seq = Sequence.of_list r in
        Lit.mk_subseteq ~sign ~sets l (Sequence.foldi
          (fun r j elt -> if (i = j) then r else elt::r) [] seq)
      | _,_ -> assert false

  let mk_eq ~sets left right =
    if TS.is_set ~sets left then
      [Lit.mk_subseteq ~sets [left] [right];Lit.mk_subseteq ~sets [right] [left]]
    else
      [Lit.mk_eq left right]

  let mk_neq ~sets left right =
    if TS.is_set ~sets left then
      [Lit.mk_notsubseteq ~sets [left] [right];Lit.mk_notsubseteq ~sets [right] [left]]
    else
      [Lit.mk_neq left right]

  module SetPositiveInfo = struct
    type t = {
      left: C.t;
      left_pos: Position.t;
      left_lit: Literal.t;
      left_scope: scope;
      right: C.t;
      right_pos: Position.t;
      right_lit: Literal.t;
      right_scope: scope;
      subst: Substs.t;
    }
  end

  (** perform positive chaining *)
  let do_positive_chaining info acc =
    let open SetPositiveInfo in
    let module P = Position in
    let renaming = Ctx.renaming_clear () in
    let subst = info.subst in
    let sc_l = info.left_scope and sc_r = info.right_scope in
    (* get the index of the literals, and the position of the chaining terms *)
    let left_idx,left_pos = Lits.Pos.cut info.left_pos in
    let right_idx,right_pos = Lits.Pos.cut info.right_pos in
    (* apply the substitution on all the other literals *)
    let lits_left = Util.array_except_idx (C.lits info.left) left_idx in
    let lits_left = Lit.apply_subst_list ~renaming subst lits_left sc_l in
    let lits_right = Util.array_except_idx (C.lits info.right) right_idx in
    let lits_right = Lit.apply_subst_list ~renaming subst lits_right sc_r in
    (* new literal :
     * given lit_l = [A inter t subseteq B] and
     *       lit_r = [A' subseteq B' union t],
     * we remove t in those literals, and then we merge them, obtaining
     * [A inter A' subseteq B inter B'] *)
    let new_lit =
      let new_lit_left = Lit.apply_subst ~renaming subst
        (remove_term_at info.left_lit left_pos) sc_l in
      let new_lit_right = Lit.apply_subst ~renaming subst
        (remove_term_at info.right_lit right_pos) sc_r in
      let sets,l1,r1,_ = Lit.View.get_subseteq_exn new_lit_left in
      let _,l2,r2,_ = Lit.View.get_subseteq_exn new_lit_right in
      Lit.mk_subseteq ~sets (l1 @ l2) (r1 @ r2) in
    (* construct the new clause *)
    let new_lits = new_lit :: (lits_left @ lits_right) in
    let proof cc = Proof.mk_c_inference ~theories:["sets"]
      ~info:[Substs.to_string subst]
      ~rule:"positive_chaining" cc [C.proof info.left;C.proof info.right] in
    let new_c = C.create ~parents:[info.left;info.right] new_lits proof in
    Util.debug 2 "chaining %a with %a using %a" C.pp info.left C.pp info.right
    Substs.pp subst;
    Util.debug 2 "new clause: %a" C.pp new_c;
    new_c :: acc

  (** positive chaining on both sides of each literal of the given clause *)
  let positive_chaining c =
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~sign:true ~eligible (C.lits c) []
    (fun acc lit position ->
      Util.debug 2 "try positive chaining in %a" Lit.pp lit;
      Lit.fold_terms ~position ~which:`All ~ord ~subterms:false
      lit acc
      (fun acc term pos -> match pos with
        | Position.Arg(_,Position.Left _) ->
          (* search for all terms in the right (opposite) side of subseteq
           * literals that unify with the current term *)
          Util.debug 54 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_right 0 term 1 acc
          (fun acc _ with_pos subst ->
            (* if a term is found, perform the chaining *)
            let right = with_pos.C.WithPos.clause in
            let right_pos = with_pos.C.WithPos.pos in
            let right_lit,_ = Lits.Pos.lit_at (C.lits right) right_pos in
            if Lit.sign right_lit then (
              Util.debug 4 "... candidate: %a" Lit.pp right_lit;
              Util.debug 5 "position: %a" Position.pp right_pos;
              let info = SetPositiveInfo.( {
                left=c; left_pos=pos; left_lit=lit; left_scope=0;
                right; right_pos; right_lit; right_scope=1;
                subst} )
              in
              do_positive_chaining info acc)
            else acc)
        | Position.Arg (_,Position.Right _) ->
          (* search for all terms in the left (opposite) side of subseteq
           * literals that unify with the current term *)
          Util.debug 5 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_left 0 term 1 acc
          (fun acc _ with_pos subst ->
            (* if a term is found, perform the chaining *)
            let left = with_pos.C.WithPos.clause in
            let left_pos = with_pos.C.WithPos.pos in
            let left_lit,_ = Lits.Pos.lit_at (C.lits left) left_pos in
            if Lit.sign left_lit then (
              Util.debug 4 "... candidate: %a" Lit.pp left_lit;
              Util.debug 5 "position: %a" Position.pp left_pos;
              let info = SetPositiveInfo.( {
                left; left_pos; left_lit; left_scope=1;
                right=c; right_pos=pos; right_lit=lit; right_scope=0;
                subst} )
              in
              do_positive_chaining info acc)
            else acc)
        | _ -> acc
      )
    )
    in new_clauses

  module SetNegativeInfo = struct
    type t = {
      positive: C.t;
      positive_pos: Position.t;
      positive_lit: Lit.t;
      positive_scope: scope;
      negative: C.t;
      negative_pos: Position.t;
      negative_lit: Lit.t;
      negative_scope: scope;
      subst: Substs.t;
    }
  end

  (** perform negative chaining - side doesn't matter *)
  let do_negative_chaining ~side info acc =
    let open SetNegativeInfo in
    let module P = Position in
    let renaming = Ctx.renaming_clear () in
    let subst = info.subst in
    let sc_p = info.positive_scope and sc_n = info.negative_scope in
    (* get the index and position of literals *)
    let positive_idx,positive_pos = Lits.Pos.cut info.positive_pos in
    let negative_idx,negative_pos = Lits.Pos.cut info.negative_pos in
    (* apply the substitution to all other literals *)
    let lits_positive = Util.array_except_idx (C.lits info.positive) positive_idx in
    let lits_positive = Lit.apply_subst_list ~renaming subst lits_positive sc_p in
    let lits_negative = Util.array_except_idx (C.lits info.negative) negative_idx in
    let lits_negative = Lit.apply_subst_list ~renaming subst lits_negative sc_n in
    (* create the new literals *)
    let new_lits_list =
      let new_lit_positive = remove_term_at info.positive_lit positive_pos in
      let new_lit_positive = Lit.apply_subst ~renaming subst new_lit_positive sc_p in
      let new_lit_negative = remove_term_at info.negative_lit negative_pos in
      let new_lit_negative = Lit.apply_subst ~renaming subst new_lit_negative sc_n in
      let sets,l_p,r_p,_ = Lit.View.get_subseteq_exn new_lit_positive in
      let _,l_n,r_n,_ = Lit.View.get_subseteq_exn new_lit_negative in
      match l_p,r_p with
        (* if the positive literal is [t subseteq emptyset] or [universe subseteq t],
         * we just remove t it in the negative literal *)
        | [],[] -> [(Lit.mk_subseteq ~sign:false ~sets l_n r_n)]
        (* else we perform the chaining and do the normalization on the fly *)
        | _,_ ->
          let f side_of_term acc' term =
            if side_of_term then (Lit.mk_subseteq ~sign:false ~sets (term::l_n) r_n)::acc'
            else (Lit.mk_subseteq ~sign:false ~sets l_n (term::r_n))::acc'
          in
          List.fold_left (f true)  (List.fold_left (f false) [] l_p) r_p
    in
    (* and finally we generate a new clause *)
    let new_lits = new_lits_list @ lits_positive @ lits_negative in
    let rule = match side with
      | `Left -> "negative_chaining_left"
      | `Right -> "negative_chaining_right"
    in
    let proof cc = Proof.mk_c_inference ~theories:["sets"]
      ~info:[Substs.to_string subst]
      ~rule cc [C.proof info.negative;C.proof info.positive] in
    let new_c = C.create ~parents:[info.negative;info.positive] new_lits proof in
    Util.debug 2 "chaining %a with %a using %a" C.pp info.negative C.pp info.positive
    Substs.pp subst;
    Util.debug 2 "new clause: %a" C.pp new_c;
    new_c :: acc

  (** negative chaining on left side of each literal of the given clause *)
  let negative_chaining_left c =
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~eligible (C.lits c) []
    (fun acc lit position ->
      Util.debug 2 "try negative chaining left in %a" Lit.pp lit;
      Lit.fold_terms ~position ~which:`All ~ord ~subterms:false
      lit acc
      (fun acc term pos -> match pos with
        | Position.Arg(_,Position.Left _) ->
          (* search for all terms in the left (same) side of subseteq literals
           * that unify with the current term *)
          Util.debug 5 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_left 0 term 1 acc
          (fun acc _ with_pos subst ->
            let act = with_pos.C.WithPos.clause in
            let act_pos = with_pos.C.WithPos.pos in
            let act_lit,_ = Lits.Pos.lit_at (C.lits act) act_pos in
            (* if a term is found, and the polarity of the two literals is
             * different, perform the chaining *)
            if (Lit.sign lit) <> (Lit.sign act_lit) then (
              Util.debug 4 "... candidate: %a" Lit.pp act_lit;
              Util.debug 5 "position: %a" Position.pp act_pos;
              let info =
                if Lit.sign act_lit
                (* case whare the positive literal is in the active clause *)
                then SetNegativeInfo.( {
                  positive=act; positive_pos=act_pos; positive_lit=act_lit; positive_scope=0;
                  negative=c; negative_pos=pos; negative_lit=lit; negative_scope=1;
                  subst} )
                (* case where the positive literal is in the passive clause *)
                else SetNegativeInfo.( {
                  positive=c; positive_pos=pos; positive_lit=lit; positive_scope=1;
                  negative=act; negative_pos=act_pos; negative_lit=act_lit; negative_scope=0;
                  subst} )
              in
              do_negative_chaining ~side:`Left info acc)
            else acc)
        | _ -> acc
      )
    )
    in new_clauses

  (** negative chaining on right side of each literal of the given clause *)
  let negative_chaining_right c =
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~eligible (C.lits c) []
    (fun acc lit position ->
      Util.debug 2 "try negative chaining right in %a" Lit.pp lit;
      Lit.fold_terms ~position ~which:`All ~ord ~subterms:false
      lit acc
      (fun acc term pos -> match pos with
      (* search for all terms in the right (same) side of subseteq literals
       * that unify with the current term *)
        | Position.Arg(_,Position.Right _) ->
          Util.debug 5 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_right 0 term 1 acc
          (fun acc _ with_pos subst ->
            let act = with_pos.C.WithPos.clause in
            let act_pos = with_pos.C.WithPos.pos in
            let act_lit,_ = Lits.Pos.lit_at (C.lits act) act_pos in
            (* if a term is found, and the polarity of the two literals is
             * different, perform the chaining *)
            if (Lit.sign lit) <> (Lit.sign act_lit) then (
              Util.debug 4 "... candidate: %a" Lit.pp act_lit;
              Util.debug 5 "position: %a" Position.pp act_pos;
              let info =
                if Lit.sign act_lit
                (* case where the positive literal is in the active clause *)
                then SetNegativeInfo.( {
                  positive=act; positive_pos=act_pos; positive_lit=act_lit; positive_scope=0;
                  negative=c; negative_pos=pos; negative_lit=lit; negative_scope=1;
                  subst} )
                (* case where the positive literal is in the passive clause *)
                else SetNegativeInfo.( {
                  positive=c; positive_pos=pos; positive_lit=lit; positive_scope=1;
                  negative=act; negative_pos=act_pos; negative_lit=act_lit; negative_scope=0;
                  subst} )
              in
              do_negative_chaining ~side:`Right info acc)
            else acc)
        | _ -> acc
      )
    )
    in new_clauses

  (** reflexivity resolution *)
  let reflexivity_res c =
    let module P = Position in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~sign:false ~eligible (C.lits c) []
    (fun acc lit pos ->
      match lit with
        | Lit.Subseteq(sets,l,r,_) ->
          Util.debug 2 "try reflexivity res in %a" Lit.pp lit;
          let seq_l = Sequence.of_list l and seq_r = Sequence.of_list r in
          let seq = Sequence.product seq_l seq_r in
          let f acc (term_l,term_r) =
            try
              (* try to unify a term in the left side with another in the right
               * side of a negative subseteq literal *)
              let subst = Unif.FO.unification term_l 0 term_r 0 in
              let i = Lits.Pos.idx pos in
              let renaming = Ctx.renaming_clear () in
              (* if a substitution is found, apply it and remove the literal,
               * as it is rendered useless *)
              let lits = Util.array_except_idx (C.lits c) i in
              let lits = Lit.apply_subst_list ~renaming subst lits 0 in
              (* generate a new clause *)
              let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"reflexivity_res" cc [C.proof c]
              ~info:[Substs.to_string subst; Util.sprintf "reflexivity resolution"] in
              let new_c = C.create ~parents:[c] lits proof in
              Util.debug 2 "reflexivity res of %a gives %a (subst: %a)" C.pp c
              C.pp new_c Substs.pp subst;
              new_c :: acc
            with Unif.Fail -> acc
          in Sequence.fold f acc seq
        | _ -> acc)
    in new_clauses

  (** factoring on left side of each couple of literals of the given clause *)
  let factoring_left c =
    let module P = Position in
    let module Seq = Sequence in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let lits = C.lits c in
    let new_clauses = Lits.fold_subseteq ~sign:true ~eligible lits []
    (fun acc lit pos ->
      let idx = Lits.Pos.idx pos in
      let lits = Util.array_except_idx lits idx in
      Lits.fold_subseteq ~sign:true ~eligible (Array.of_list lits) []
      (fun acc lit' _ ->
        (* try the inference for each couple of positive subseteq literals *)
        match lit,lit' with
        | Lit.Subseteq(sets,l,r,_),Lit.Subseteq(_,l',r',_)  ->
          Util.debug 2 "try factoring left in %a and %a" Lit.pp lit Lit.pp lit';
          let seq_l = Sequence.of_list l and seq_r = Sequence.of_list l' in
          let seq = Sequence.product seq_l seq_r in
          let f acc (term_l,term_l') =
            try
              (* try to unify two terms appearing of the left side of the two
               * current literals *)
              let subst = Unif.FO.unification term_l 0 term_l' 0 in
              let renaming = Ctx.renaming_clear () in
              let l = List.filter (fun t -> not (FOTerm.eq term_l t)) l in
              let l' = List.filter (fun t -> not (FOTerm.eq term_l' t)) l' in
              let lits = Lit.apply_subst_list ~renaming subst lits 0 in
              (* if a subsitution is found, apply it and perform the factoring *)
              let new_lits =
                match l,r with
                  (* thoses cases are for when one of se sides of the considered
                   * literal is empty, to avoid not creating any literal *)
                  | [],[] ->
                    let lit = Lit.mk_subseteq ~sign:false ~sets l' r' in
                      (Lit.apply_subst ~renaming subst lit 0)::lits
                  | [],_ ->
                    let seq = Seq.of_list r in
                    let f acc t_A =
                      let lit = Lit.mk_subseteq ~sign:false ~sets l' (t_A::r') in
                        (Lit.apply_subst ~renaming subst lit 0)::acc
                    in Seq.fold f lits seq
                  | _,[] ->
                    let seq = Seq.of_list l in
                    let f acc t_B =
                      let lit = Lit.mk_subseteq ~sign:false ~sets (t_B::l') r' in
                        (Lit.apply_subst ~renaming subst lit 0)::acc
                    in Seq.fold f lits seq
                  (* default case *)
                  | _,_ ->
                    let seq_l = Seq.of_list l and seq_r = Seq.of_list r in
                    let seq =  Seq.product seq_l seq_r in
                    let f acc (t_A,t_B) =
                      let lit = Lit.mk_subseteq ~sign:false ~sets (t_B::l') (t_A::r') in
                      (Lit.apply_subst ~renaming subst lit 0)::acc
                    in Seq.fold f lits seq
              in
              (* generate the new clause *)
              let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"factoring_left" cc [C.proof c]
              ~info:[Substs.to_string subst] in
              let new_c = C.create ~parents:[c] new_lits proof in
              Util.debug 2 "factoring left of %a gives %a (subst: %a)" C.pp c
              C.pp new_c Substs.pp subst;
              new_c :: acc
            with Unif.Fail -> acc
          in Sequence.fold f acc seq
        | _ -> acc))
    in new_clauses

  (** rewrite A=B where A and B are sets into (A subseteq B) and (B subseteq A) *)
  let rewrite_set_eq c =
    let lits = C.lits c in
    let rec aux = fun lit_list idx -> match lit_list with
      | [] -> None
      | lit::t -> begin match lit with
        (* search a literal A=B *)
        | Lit.Equation(a,b,sign) when sign ->
          (* if A (and B) are sets, rewrite the clause *)
          if TS.is_set ~sets a then begin
            Util.debug 2 "rewrite %a into:" C.pp c;
            let new_lits =
              [Lit.mk_subseteq ~sets [a] [b];Lit.mk_subseteq ~sets [b] [a]]
            in
            let context = Util.array_except_idx lits idx in
            let f lit =
              let proof cc = Proof.mk_c_inference ~theories:["sets"]
                ~rule:"simplify_eq" cc [C.proof c] in
              let new_c = C.create ~parents:[c] (lit::context) proof in
              Util.debug 2 "    %a" C.pp new_c;
              new_c
            (* generate two clauses, as the rewriting gives a conjunction of
             * literals *)
            in Some (List.map f new_lits)
          end else aux t (idx+1)
        | _ -> aux t (idx+1)
      end
    in aux (Array.to_list lits) 0

  (** rewrite A!=B where A and B are sets into (A notsubseteq B) or (B notsubseteq A) *)
  let rewrite_set_neq c =
    let lits = C.lits c in
    let rec aux = fun lit_list idx -> match lit_list with
      | [] -> None
      | lit::t -> begin match lit with
        (* search a literal A!=B *)
        | Lit.Equation(a,b,sign) when not(sign) ->
          (* if A (and B) are sets, rewrite the clause *)
          if TS.is_set ~sets a then
            let context = Util.array_except_idx lits idx in
            let new_lits =
              (Lit.mk_notsubseteq ~sets [a] [b])::(Lit.mk_notsubseteq ~sets [b] [a])::context
            in
            let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"simplify_neq" cc [C.proof c] in
            let new_c = C.create ~parents:[c] new_lits proof in
            Util.debug 2 "rewrite %a into %a" C.pp c C.pp new_c;
            (* generate one clause, as the rewriting gives a disjunction of
             * literals *)
            Some ([new_c])
          else aux t (idx+1)
        | _ -> aux t (idx+1)
      end
    in aux (Array.to_list lits) 0

  exception Found of Lit.t list

  (** partition of a list of terms into those which are singletons (stocking only
      the term in it) and other terms *)
  let sing_partition ~sets terms =
    List.fold_left
    (fun (acc_s,acc_o) term ->
       match TS.view ~sets term with
         | TS.Singleton t -> t::acc_s,acc_o
         | TS.Other _ -> acc_s,term::acc_o
         | _ -> assert false)
    ([],[]) terms

  (** positive singleton rewriting rule *)
  let singleton_pos c =
    let lits = C.lits c in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let module P = Position in
    try Lits.fold_subseteq ~sign:true ~eligible lits c
    (fun acc lit pos ->
      Util.debug 2 "try positive singleton simplification in %a" Lit.pp lit;
      let sets,a,b,_ = Lit.View.get_subseteq_exn lit in
      let idx = Lits.Pos.idx pos in
      let context = Util.array_except_idx lits idx in
      let sing,other = sing_partition ~sets a in
      match sing,other,b with
        (* don't rewrite the clause either if there is no singleton in it...*)
        | [],_,_
        (* ...or if the rewriting would give the same literal *)
        | [_],[],[_] -> acc
        | h::t,_,_ ->
          let t_1 = TS.mk_singleton ~sets h in
          (* construct the literals {t1} subseteq b, b in Bu
           * and {t1} notsubseteq a, a in An *)
          let create_lit_set ~sign acc term =
            (Lit.mk_subseteq ~sets ~sign [t_1] [term])::acc
          in
          (* construct the literals t1 != tk *)
          let create_lit_term acc term = (mk_neq ~sets h term)@acc
          in
          let new_lits = List.fold_left (create_lit_set ~sign:false) context other in
          let new_lits = List.fold_left (create_lit_set ~sign:true) new_lits b in
          let new_lits = List.fold_left create_lit_term new_lits t in
            (* stops when the clause has been rewritten once *)
            raise (Found new_lits))
    with Found (new_lits) ->
      (* generate the new clause *)
      let proof cc = Proof.mk_c_inference ~theories:["sets"]
      ~rule:"singleton_pos" cc [C.proof c] in
      let new_c = C.create ~parents:[c] new_lits proof in
      Util.debug 2 "rewriting %a into %a" C.pp c C.pp new_c;
      new_c

  (** negative singleton rewriting rule *)
  let singleton_neg c =
    let lits = C.lits c in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let module P = Position in
    let new_clauses = Lits.fold_subseteq ~sign:false ~eligible lits []
    (fun acc lit pos ->
      Util.debug 2 "try negative singleton simplification in %a" Lit.pp lit;
      let sets,a,b,_ = Lit.View.get_subseteq_exn lit in
      let idx = Lits.Pos.idx pos in
      let context = Util.array_except_idx lits idx in
      let sing,other = sing_partition ~sets a in
      match sing,other,b with
        (* don't rewrite the clause either if there is no singleton in it...*)
        | [],_,_
        (* ...or if the rewriting would give the same literal *)
        | [_],[],[_] -> acc
        | h::t,_,_ ->
          Util.debug 2 "rewriting %a into :" C.pp c;
          let t_1 = TS.mk_singleton ~sets h in
          (* construct the literals{t1} subseteq a, a in An
           * and {t1} notsubseteq b, b in Bu
           * and generate a clause for each literal *)
          let create_clause_set ~sign term =
            let new_lits =
              if sign then (Lit.mk_subseteq ~sets ~sign [t_1] [term])::context
              else (Lit.mk_subseteq ~sets ~sign [t_1] [term])::context
            in
            let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"singleton_neg" cc [C.proof c] in
            let new_c = C.create ~parents:[c] new_lits proof in
            Util.debug 2 "    %a" C.pp new_c;
            new_c
          in
          (* construct the literals t1 != tk, and generate a clause for each
           * literal *)
          let create_clause_term term =
            let new_lits = (mk_eq ~sets h term) in
            let f lit =
              let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"singleton_neg" cc [C.proof c] in
              let new_c = C.create ~parents:[c] (lit::context) proof in
              Util.debug 2 "    %a" C.pp new_c;
              new_c
            in
            List.map f new_lits
          in
          let new_clauses_A = List.map (create_clause_set ~sign:true) other in
          let new_clauses_B = List.map (create_clause_set ~sign:false) b in
          let new_clauses_t = Util.list_flatmap create_clause_term t in
            new_clauses_A @ new_clauses_B @ new_clauses_t @ acc)
    in match new_clauses with
      | [] -> None
      | _ -> Some new_clauses

  (** choose one variable in the clause *)
  let choose_var lits =
    let module Seq = Sequence in
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    (* list all variables that are not in a singleton *)
    let filter = (fun term -> match TS.view ~sets term with
      | TS.Singleton _ -> None
      | _ -> Some term)
    in
    let vars = Lits.Seq.terms lits |>
      Seq.fmap filter
      |> Seq.flatMap FOTerm.Seq.vars
    in
    (* sequence of vars that appear in only one side of subseteq and
     * notsubseteq *)
    let f (vars_l,vars_r) term pos =
      if FOTerm.is_var term then
        let neq t = not (FOTerm.eq term t) in
        match pos with
          | Position.Arg (_,Position.Left _) ->
              vars_l,Seq.filter neq vars_r
          | Position.Arg (_,Position.Right _) ->
              Seq.filter neq vars_l,vars_r
          | _ -> vars_l,vars_r
      else vars_l,vars_r
    in
    let vars_left,vars_right =
      Lits.fold_subseteq_terms ~eligible ~ord lits (vars,vars) f
    in
    (* choose one var among the remaning ones *)
    let vars' = Seq.to_set (module FOTerm.Set) vars_left
    in
      try Some (FOTerm.Set.choose vars',`Left)
      with Not_found ->
        let vars' = Seq.to_set (module FOTerm.Set) vars_right in
        try Some (FOTerm.Set.choose vars',`Right)
        with Not_found -> None

  (** choose one variable in the clause that appears under a singleton *)
  let choose_var_sing lits =
    let module Seq = Sequence in
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    (* list all variables that are in a singleton *)
    let filter = (fun term -> match TS.view ~sets term with
      | TS.Singleton t -> Some t
      | _ -> None)
    in
    let vars = Lits.Seq.terms lits |>
      Seq.fmap filter
      |> Seq.flatMap FOTerm.Seq.vars
    in
    (* sequence of vars that appear in left side of subseteq and not subseteq
     * (and not in right side) *)
    let f acc term pos =
      let module P = Position in
      let neq t = not (FOTerm.eq t term) in
      match pos,TS.view ~sets term with
        | P.Arg (_,P.Right _), TS.Singleton t when FOTerm.is_var t ->
          Seq.filter neq acc
        | _ -> acc
    in
    let vars = Lits.fold_subseteq_terms ~eligible ~ord lits vars f
    in
    (* choose one var among the remaning ones *)
    let vars' = Seq.to_set (module FOTerm.Set) vars
    in
      try Some (FOTerm.Set.choose vars')
      with Not_found -> None

  (** generates a sequence of all paths through a list of lists *)
  let rec mk_seq l acc k =
    match l with
      | [] -> k acc
      | h::t -> List.iter (fun x -> mk_seq t (x::acc) k) h

  (** version of mk_seq to be used in singleton_elim *)
  let rec mk_seq_sing l (accl,accr) k =
    match l with
      | [] -> k (accl,accr)
      | h::t -> List.iter
        (fun x -> match x with
          | term,`Left -> mk_seq_sing t (term::accl,accr) k
          | term,`Right -> mk_seq_sing t (accl,term::accr) k
          | _ -> assert false) h

  (** eliminate variables occuring in singletons on the left side of subseteq *)
  let singleton_elim c =
    Util.debug 2 "try singleton elimination in %a" C.pp c;
    let lits = C.lits c in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let ord = Ctx.ord () in
    (* choose a variable that appear in a singleton, named x *)
    match choose_var_sing lits with
      | None -> None
      | Some x ->
        Util.debug 3 "variable to eliminate: %a" FOTerm.pp x;
        (* search all the positive subseteq literals that contain {x} *)
        let (context,posl,posr) = Lits.fold_subseteq ~sign:true ~eligible lits
        (Array.to_list lits,[],[])
        (fun (cont,accl,accr) lit pos -> match lit with
          | Lit.Subseteq(sets,a,b,_) ->
            let sing_x = TS.mk_singleton ~sets x in
            if List.exists (FOTerm.eq sing_x) a
            then
              (* remove {x} from the left side literal, and update the
               * context *)
              let a = List.filter (fun t -> not (FOTerm.eq sing_x t)) a in
              let idx = Lits.Pos.idx pos in
              let cont = Util.array_except_idx (Array.of_list cont) idx in
              cont,a@accl,b@accr
            else cont,accl,accr
          | _ -> assert false)
        in
        (* search all the negative subseteq literals that contain {x} *)
        let context,terms_neg = Lits.fold_subseteq ~sign:false ~eligible lits
        (context,[])
        (fun (cont,acc) lit pos -> match lit with
          | Lit.Subseteq(sets,a,b,_) ->
            let sing_x = TS.mk_singleton ~sets x in
            if List.exists (FOTerm.eq sing_x) a
            then
              (* remove {x} from the left side of the literal, and update the
               * context *)
              let a = Util.list_fmap
              (fun t -> if FOTerm.eq sing_x t
                then None
                else Some (t,`Right)) a
              in
              let idx = Lits.Pos.idx pos in
              let cont = Util.array_except_idx (Array.of_list cont) idx in
              cont,(List.fold_left (fun acc t -> (t,`Left)::acc ) a b)::acc
            else cont,acc
          | _ -> assert false)
        in
        (* search all the equality literals that contain x *)
        let context,terms_sing =
          Lits.fold_eqn ~sign:true ~ord ~eligible:C.Eligible.(filter Lit.is_eq)
          lits (context,[])
          (fun (cont,acc) t1 t2 _ pos ->
            (* if the equation is t=x or x=t, add {t} to the right side of the
             * new literal, and update the context *)
            if FOTerm.eq t1 x then
              (* case x=t2 *)
              let idx = Lits.Pos.idx pos in
              let cont = Util.array_except_idx (Array.of_list cont) idx in
              cont,(TS.mk_singleton ~sets t2)::acc
            else if FOTerm.eq t2 x then
              (* case t1=x*)
              let idx = Lits.Pos.idx pos in
              let cont = Util.array_except_idx (Array.of_list cont) idx in
              cont,(TS.mk_singleton ~sets t1)::acc
            else cont,acc)
        in
        Util.debug 3 "rewriting %a into:" C.pp c;
        (* normalization:
         *    we distribute the intersection over the union in the left side of
         *    each literal, giving a conjunction of literals *)
        let seq = Sequence.from_iter (mk_seq_sing terms_neg (posl,posr@terms_sing)) in
        (* generate the new clauses *)
        let new_clauses = Sequence.fold
        (fun new_clauses (left,right) ->
          let new_lit = Lit.mk_subseteq ~sets left right in
          let proof cc = Proof.mk_c_inference ~theories:["sets"]
            ~rule:"singleton_elim" cc [C.proof c] in
          let new_c = C.create ~parents:[c] (new_lit::context) proof in
          Util.debug 3 "    %a" C.pp new_c;
          new_c::new_clauses) [] seq
        in
        match new_clauses with
          | [] -> None
          | _ -> Some new_clauses

  (** perform the variable elimination, with the variable on the left side *)
  let do_var_elim x lits c =
    let module P = Position in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    (* function to treat the negative literals that contain x *)
    let do_neg_lits (ctx,acc') lit pos =
      let _,a',b',_ = Lit.View.get_subseteq_exn lit in
      if List.exists (FOTerm.eq x) a' then
        (* remove x from the literal, and update the context *)
        let a' = Util.list_fmap
          (fun t ->
            if (FOTerm.eq t x) then None
            else Some (t,`Left)) a'
        in
        let idx = Lits.Pos.idx pos in
        let ctx = Util.array_except_idx (Array.of_list ctx) idx in
        let neg_terms = a'@(List.map (fun t -> t,`Right) b') in
        ctx,neg_terms::acc'
      else ctx,acc'
    in
    (* search all the positive set literals that contain x *)
    let context,new_lits_list = Lits.fold_subseteq ~sign:true ~eligible lits
    (Array.to_list lits,[])
    (fun (ctx,acc) lit pos ->
      let sets,a,b,_ = Lit.View.get_subseteq_exn lit in
      if List.exists (FOTerm.eq x) a then
        (* remove x from the literal, and update the context *)
        let a = List.filter (fun t -> not (FOTerm.eq t x)) a in
        let idx = Lits.Pos.idx pos in
        let ctx = Util.array_except_idx (Array.of_list ctx) idx in
        (* serach all the negative set literals that containt x *)
        let ctx,terms = Lits.fold_subseteq ~sign:false ~eligible
          (Array.of_list ctx) (ctx,[]) do_neg_lits
        in
        (* first step of normalization:
         *    we transform each literal under the 'or' into a conjunction
         *    of literals by distributing the intersection over the union
         *    in the left side of the literal *)
        let seq = Sequence.from_iter (mk_seq_sing terms (a,b)) in
        let new_lits = Sequence.fold
          (fun acc' (left,right) ->
            (Lit.mk_subseteq ~sets ~sign:true left right)::acc')
          [] seq
        in
        ctx,new_lits::acc
      else ctx,acc)
    in
    let context,new_lits_list = match new_lits_list with
      | [] ->
        let ctx,terms = Lits.fold_subseteq ~sign:false ~eligible
          (lits) (Array.to_list lits,[]) do_neg_lits
        in
        let seq = Sequence.from_iter (mk_seq_sing terms ([],[])) in
        let new_lits = Sequence.fold
          (fun acc' (left,right) ->
            (Lit.mk_subseteq ~sets ~sign:true left right)::acc')
          [] seq
        in
        ctx,[new_lits]
      | _ -> context,new_lits_list
    in
    (* second step of normalization:
     *    we redistribute the 'and' over the 'or' *)
    let lits_seq = Sequence.from_iter (mk_seq new_lits_list context) in
    (* generate the new clauses *)
    Util.debug 3 "rewriting %a into:" C.pp c;
    let new_clauses = Sequence.fold
    (fun acc lits ->
      let proof cc = Proof.mk_c_inference ~theories:["sets"]
        ~rule:"var_elimi_right" cc [C.proof c] in
      let new_c = C.create ~parents:[c] lits proof in
      Util.debug 3 "    %a" C.pp new_c;
      new_c::acc) [] lits_seq
    in match new_clauses with
      | [] -> None
      | _ -> Some new_clauses

  (** eliminate variables occuring in subseteq;
      the variable must appear in the same side of each literal, and not in the
      other *)
  let var_elim c =
    Util.debug 2 "try variable elimination right in %a" C.pp c;
    let module P = Position in
    let lits = C.lits c in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    (* choose a variable that appears in a subseteq, named x *)
    match choose_var lits with
      | None -> None
      | Some (x,`Left) ->
        (* if the variable is in the left side of the literals, apply the
         * rewriting rule *)
        Util.debug 3 "variable to eliminate : %a" FOTerm.pp x;
        do_var_elim x lits c
      | Some (x,`Right) ->
        (* if the variable is in the right side of the literals,
         * rename it by x' = comp(x), so that x' is in the left side of the
         * literals; then apply the same rewriting rule as above *)
        Util.debug 3 "variabla to eliminate : %a" FOTerm.pp x;
        let lits = Lits.fold_subseteq ~eligible lits lits
        (fun acc lit pos ->
          let sets,a,b,sign = Lit.View.get_subseteq_exn lit in
          if List.mem x b then
            (* renaming *)
            let a = x::a in
            let b = List.filter (fun t -> not(FOTerm.eq t x)) b in
            let lit = Lit.mk_subseteq ~sets ~sign a b in
            let idx = Lits.Pos.idx pos in
            let lits = Util.array_except_idx acc idx in
            Array.of_list (lit::lits)
          else acc)
        in do_var_elim x lits c

  (** reflexivity tautology *)
  let reflexivity c =
    let module Seq = Sequence in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    Lits.fold_subseteq ~sign:true ~eligible (C.lits c) false
    (fun res lit _ ->
      res ||
      (match lit with
        | Lit.Subseteq(_,l,r,_) ->
          let seq = Seq.product (Seq.of_list l) (Seq.of_list r) in
          Seq.exists (fun (a,b) -> FOTerm.eq a b) seq
        | _ -> false))

  (** find the A subseteq B or A notsubseteq B tautologies *)
  let is_tautology c =
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let lits = C.lits c in
    let eq = FOTerm.eq in
    Lits.fold_subseteq ~sign:false ~eligible lits false
    (fun res lit _ -> Lits.fold_subseteq ~sign:true ~eligible lits res
      (fun res' lit' _ -> match lit,lit' with
        | Lit.Subseteq(_,l,r,_),Lit.Subseteq(_,l',r',_) ->
          res' ||
          (Util.list_subset eq l l' && Util.list_subset eq r r')
        | _ -> res'))

  (** reflexivity contradiction *)
  let is_absurd lit =
    let module Seq = Sequence in
    match lit with
      | Lit.Subseteq(_,l,r,sign) ->
      if sign then lit
      else let seq = Seq.product (Seq.of_list l) (Seq.of_list r) in
        Seq.fold (fun lit (a,b) -> if FOTerm.eq a b then Lit.mk_absurd else lit)
        lit seq
      | _ -> lit

  let setup () =
    Util.debug 1 "setup set chaining";
    Env.add_cnf_option (Cnf.PostNNF preprocess);
    Ctx.Lit.add_from_hook (Lit.Conv.set_hook_from ~sets);
    Ctx.Lit.add_to_hook (Lit.Conv.set_hook_to);
    Env.add_binary_inf "positive_chaining" positive_chaining;
    Env.add_binary_inf "negative_chaining_left" negative_chaining_left;
    Env.add_binary_inf "negative_chaining_right" negative_chaining_right;
    Env.add_unary_inf "reflexivity_res" reflexivity_res;
    Env.add_unary_inf "factoring_left" factoring_left;
    Env.add_multi_simpl_rule rewrite_set_eq;
    Env.add_multi_simpl_rule rewrite_set_neq;
    Env.add_rw_simplify singleton_pos;
    Env.add_multi_simpl_rule singleton_neg;
    Env.add_multi_simpl_rule singleton_elim;
    Env.add_multi_simpl_rule var_elim;
    Env.add_is_trivial is_tautology;
    Env.add_is_trivial reflexivity;
    Env.add_lit_rule "remove_absurd_set" is_absurd;
    (* maybe change the set signature? FIXME
    Signal.on Ctx.Theories.Sets.on_add
      (fun theory' -> _theory := theory'; Signal.ContinueListening);
    *)
    ()
end

let _initial_setup () =
  (* declare types for set operators *)
  Util.debug 3 "declaring set types...";
  let set_signature = Theories.Sets.signature !_theory in
  Params.signature := Signature.merge !Params.signature set_signature;
  (* add hooks (printing, lit conversion) *)
  FOTerm.add_hook (Theories.Sets.print_hook ~sets:!_theory);
  ()

let extension =
  let module DOIT(Env : Env.S) = struct
    include Extensions.MakeAction(Env)
    let actions =
      let module Set = Make(Env) in
      [Ext_general Set.setup]
  end
  in
  { Extensions.default with
    Extensions.name="set";
    Extensions.init_actions = [Extensions.Init_do _initial_setup];
    Extensions.make=(module DOIT : Extensions.ENV_TO_S);
  }

let () =
  Params.add_opts
    [ "-set"
      , Arg.Unit (fun () -> Extensions.register extension)
      , "enable set chaining"
    ];
  ()

