
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
    (* negative chaining where the clause is on the left *)

  val negative_chaining_right: Env.binary_inf_rule
    (* negative chaining where the clause is on the right *)

  val reflexivity_res: Env.unary_inf_rule
    (* reflexivity resolution *)

  val singleton_witness: Env.unary_inf_rule
    (* choice of a witness for all terms appearing in a singleton on the left
     * side of a \not\subseteq *)

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
  module PS = Env.ProofState
  module C = Env.C
  module Ctx = Env.Ctx
  module F = Formula.FO
  module TS = Theories.Sets
  module S = Substs
  module Lits = Literals
  module Lit = Literal
  module I = PS.TermIndex

  let _theory = ref TS.default

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
    let rec aux l r acc =
      match l with
        | h::t ->
          begin match r with
            | h'::t' ->
                let h_inter =
                  if h.sets = [] && h'.comp = [] then
                    TS.mk_empty ~sets h.ty
                else if h.empty || h'.empty then
                  TS.mk_empty ~sets h.ty
                else
                  TS.mk_inter ~sets (h.sets@h'.comp)
                and h_union =
                  if h.comp = [] && h'.sets = [] then
                    TS.mk_empty ~sets h.ty
                  else
                    TS.mk_union ~sets (h'.sets@h.comp) in
                  aux l t' (F.Base.atom ((TS.mk_subseteq ~sets h_inter h_union))::acc)
            | [] -> aux t right acc
          end
        | [] -> acc
    in aux left right []

  let rec preprocess f =
    let sets = !_theory in
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
              [{sets = []; comp = []; empty = false; ty = TS._get_set_type ~sets s1}]
            and preproc_right =
              preprocess_subseteq ~sets ~left:false [s2]
              [{sets = []; comp = []; empty = false; ty = TS._get_set_type ~sets s2}] in
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
                    [{sets = []; comp = []; empty = false; ty = TS._get_set_type ~sets s1}]
                and preproc_right =
                  preprocess_subseteq ~sets ~left:false [s2]
                    [{sets = []; comp = []; empty = false; ty = TS._get_set_type ~sets s1}] in
                  F.Base.or_ (List.map (fun x -> F.Base.not_ x)
                    (reform_subseteq ~sets preproc_left preproc_right))
              | TS.Other _ -> f
              | _ -> f
            end
          | _ -> assert false
          (*| F.True
          | F.False -> f
          | F.And f_list -> F.Base.not_ (F.Base.and_ (List.map preprocess f_list))
          | F.Or f_list -> F.Base.not_ (F.Base.or_ (List.map preprocess f_list))
          | F.Not f'' -> assert false
          | F.Imply (f1,f2) -> F.Base.not_ (F.Base.imply (preprocess f1) (preprocess f2))
          | F.Equiv (f1,f2) -> F.Base.not_ (F.Base.equiv (preprocess f1) (preprocess f2))
          | F.Xor (f1,f2) -> F.Base.not_ (F.Base.xor (preprocess f1) (preprocess f2))
          | F.Eq (t1,t2) ->
            if TS.is_set ~sets t1
            then begin
              Util.debug 3 "Found a set of type not equals -- %s"
                "applying not(A = B) --> not(A subseteq B) or not(B subseteq A)";
              preprocess (F.Base.or_
                [(F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets t1 t2)));
                (F.Base.not_ (F.Base.atom (TS.mk_subseteq ~sets t2 t1)))]
            )
            end else f
          | F.Neq (t1,t2) ->
            if TS.is_set ~sets t1
            then begin
              Util.debug 3 "Found a set of type equals -- %s"
                "applying not(A <> B) --> (A subseteq B) and (B subseteq A)";
              preprocess (F.Base.and_
                [(F.Base.atom (TS.mk_subseteq ~sets t1 t2));
                (F.Base.atom (TS.mk_subseteq ~sets t2 t1))]
            )
            end else f
          | F.Forall (t,f'') -> F.Base.not_ (F.Base.__mk_forall t (preprocess f''))
          | F.Exists (t,f'') -> F.Base.not_ (F.Base.__mk_exists t (preprocess f''))
          | F.ForallTy f'' -> F.Base.not_ (F.Base.__mk_forall_ty (preprocess f''))*)
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

  (* remove the term at the given position of a subseteq literal *)
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

  (* perform positive chaining *)
  let do_positive_chaining info acc =
    let open SetPositiveInfo in
    let module P = Position in
    let renaming = Ctx.renaming_clear () in
    let subst = info.subst in
    let sc_l = info.left_scope and sc_r = info.right_scope in
    (* get the index of the literals, and the position of the chaining terms *)
    let left_idx,left_pos = Lits.Pos.cut info.left_pos in
    let right_idx,right_pos = Lits.Pos.cut info.right_pos in
    (* applying the substitution on all the other literals *)
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
      ~info:[Substs.to_string subst; Util.sprintf "positive chaining"]
      ~rule:"positive_chaining" cc [C.proof info.left;C.proof info.right] in
    let new_c = C.create ~parents:[info.left;info.right] new_lits proof in
    Util.debug 2 "chaining %a with %a using %a" C.pp info.left C.pp info.right
    Substs.pp subst;
    Util.debug 2 "new literal: %a" Lit.pp new_lit;
    new_c :: acc

  (* positive chaining on both sides of each literal of the given clause *)
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
          Util.debug 4 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_right 0 term 1 acc
          (fun acc _ with_pos subst ->
            let right = with_pos.C.WithPos.clause in
            let right_pos = with_pos.C.WithPos.pos in
            let right_lit,_ = Lits.Pos.lit_at (C.lits right) right_pos in
            if Lit.sign right_lit then (
              Util.debug 3 "... candidate: %a" Lit.pp right_lit;
              Util.debug 4 "position: %a" Position.pp right_pos;
              let info = SetPositiveInfo.( {
                left=c; left_pos=pos; left_lit=lit; left_scope=0;
                right; right_pos; right_lit; right_scope=1;
                subst} )
              in
              do_positive_chaining info acc)
            else acc)
        | Position.Arg (_,Position.Right _) ->
          Util.debug 4 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_left 0 term 1 acc
          (fun acc _ with_pos subst ->
            let left = with_pos.C.WithPos.clause in
            let left_pos = with_pos.C.WithPos.pos in
            let left_lit,_ = Lits.Pos.lit_at (C.lits left) left_pos in
            if Lit.sign left_lit then (
              Util.debug 3 "... candidate: %a" Lit.pp left_lit;
              Util.debug 4 "position: %a" Position.pp left_pos;
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

  (* perform negative chaining - side doesn't matter *)
  let do_negative_chaining ~side info acc =
    let open SetNegativeInfo in
    let module P = Position in
    let renaming = Ctx.renaming_clear () in
    let subst = info.subst in
    let sc_p = info.positive_scope and sc_n = info.negative_scope in
    let positive_idx,positive_pos = Lits.Pos.cut info.positive_pos in
    let negative_idx,negative_pos = Lits.Pos.cut info.negative_pos in
    let lits_positive = Util.array_except_idx (C.lits info.positive) positive_idx in
    let lits_positive = Lit.apply_subst_list ~renaming subst lits_positive sc_p in
    let lits_negative = Util.array_except_idx (C.lits info.negative) negative_idx in
    let lits_negative = Lit.apply_subst_list ~renaming subst lits_negative sc_n in
    let new_lits_list =
      let new_lit_positive = remove_term_at info.positive_lit positive_pos in
      let new_lit_positive = Lit.apply_subst ~renaming subst new_lit_positive sc_p in
      let new_lit_negative = remove_term_at info.negative_lit negative_pos in
      let new_lit_negative = Lit.apply_subst ~renaming subst new_lit_negative sc_n in
      let sets,l_p,r_p,_ = Lit.View.get_subseteq_exn new_lit_positive in
      let _,l_n,r_n,_ = Lit.View.get_subseteq_exn new_lit_negative in
      let f side_of_term acc' term =
        if side_of_term then (Lit.mk_subseteq ~sign:false ~sets (term::l_n) r_n)::acc'
        else (Lit.mk_subseteq ~sign:false ~sets l_n (term::r_n))::acc'
      in
      List.fold_left (f true)  (List.fold_left (f false) [] l_p) r_p
    in
    let new_lits = new_lits_list @ lits_positive @ lits_negative in
    let name,rule = match side with
      | `Left -> "negative chaining left", "negative_chaining_left"
      | `Right -> "negative chaining right", "negative_chaining_right"
    in
    let proof cc = Proof.mk_c_inference ~theories:["sets"]
      ~info:[Substs.to_string subst; name]
      ~rule cc [C.proof info.negative;C.proof info.positive] in
    let new_c = C.create ~parents:[info.negative;info.positive] new_lits proof in
    Util.debug 2 "chaining %a with %a using %a" C.pp info.negative C.pp info.positive
    Substs.pp subst;
    Util.debug 2 "new literals: %a" Lits.pp (Array.of_list new_lits_list);
    new_c :: acc

  (*negative chaining on left side of each literal of the given clause *)
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
          Util.debug 4 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_left 0 term 1 acc
          (fun acc _ with_pos subst ->
            let act = with_pos.C.WithPos.clause in
            let act_pos = with_pos.C.WithPos.pos in
            let act_lit,_ = Lits.Pos.lit_at (C.lits act) act_pos in
            if (Lit.sign lit) <> (Lit.sign act_lit) then (
              Util.debug 3 "... candidate: %a" Lit.pp act_lit;
              Util.debug 4 "position: %a" Position.pp act_pos;
              let info =
                if Lit.sign act_lit
                then SetNegativeInfo.( {
                  positive=act; positive_pos=act_pos; positive_lit=act_lit; positive_scope=0;
                  negative=c; negative_pos=pos; negative_lit=lit; negative_scope=1;
                  subst} )
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

  (* negative chaining on right side of each literal of the given clause *)
  let negative_chaining_right c =
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~eligible (C.lits c) []
    (fun acc lit position ->
      Util.debug 2 "try negative chaining right in %a" Lit.pp lit;
      Lit.fold_terms ~position ~which:`All ~ord ~subterms:false
      lit acc
      (fun acc term pos -> match pos with
        | Position.Arg(_,Position.Right _) ->
          Util.debug 4 "current position: %a" Position.pp pos;
          I.retrieve_unifiables !_idx_right 0 term 1 acc
          (fun acc _ with_pos subst ->
            let act = with_pos.C.WithPos.clause in
            let act_pos = with_pos.C.WithPos.pos in
            let act_lit,_ = Lits.Pos.lit_at (C.lits act) act_pos in
            if (Lit.sign lit) <> (Lit.sign act_lit) then (
              Util.debug 3 "... candidate: %a" Lit.pp act_lit;
              Util.debug 4 "position: %a" Position.pp act_pos;
              let info =
                if Lit.sign act_lit
                then SetNegativeInfo.( {
                  positive=act; positive_pos=act_pos; positive_lit=act_lit; positive_scope=0;
                  negative=c; negative_pos=pos; negative_lit=lit; negative_scope=1;
                  subst} )
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

  (* reflexivity resolution *)
  let reflexivity_res c =
    let module P = Position in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let new_clauses = Lits.fold_subseteq ~sign:false ~eligible (C.lits c) []
    (fun acc lit pos ->
      match lit with
        | Lit.Subseteq(sets,l,r,_) ->
          Util.debug 2 "try reflexifvity res in %a" Lit.pp lit;
          let seq_l = Sequence.of_list l and seq_r = Sequence.of_list r in
          let seq = Sequence.product seq_l seq_r in
          let f acc (term_l,term_r) =
            try
              let subst = Unif.FO.unification term_l 0 term_r 0 in
              let i = Lits.Pos.idx pos in
              let renaming = Ctx.renaming_clear () in
              let lits = Util.array_except_idx (C.lits c) i in
              let lits = Lit.apply_subst_list ~renaming subst lits 0 in
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

  (* singleton witness inference rule *)
  let singleton_witness c =
    let lits = C.lits c in
    let eligible = C.Eligible.(filter Lit.is_subseteq) in
    let module P = Position in
    let new_clauses = Lits.fold_subseteq ~sign:false ~eligible lits []
    (fun acc lit pos ->
      Util.debug 2 "try singleton witness inference in %a" Lit.pp lit;
      let sets,a,b,_ = Lit.View.get_subseteq_exn lit in
      let idx = Lits.Pos.idx pos in
      let context = Util.array_except_idx lits idx in
      let sing,other = List.fold_left
        (fun (acc_s,acc_o) term ->
            match TS.view ~sets term with
              | TS.Singleton t -> t::acc_s,acc_o
              | TS.Other _ -> acc_s,term::acc_o
              | _ -> assert false)
        ([],[]) a
      in
      match sing with
        | [] -> acc
        | h::t ->
            Util.debug 2 "singleton witness inference of %a gives :" C.pp c;
          let t_1 = TS.mk_singleton ~sets h in
          let create_clause_set ~sign term =
            let new_lits = match TS.view ~sets term with
              | TS.Emptyset _ -> (Lit.mk_subseteq ~sets ~sign:false [t_1] [])::context
              | _ ->
                if sign then (Lit.mk_subseteq ~sets ~sign:true [t_1] [term])::context
                else (Lit.mk_subseteq ~sets ~sign:true [t_1;term] [])::context
            in
            let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"singleton_witness" cc [C.proof c] in
            let new_c = C.create ~parents:[c] new_lits proof in
            Util.debug 2 "    %a" C.pp new_c;
            new_c
          in
          let create_clause_term term =
            let new_lits =
              (*match Type.view (TS._get_set_type ~sets term) with
                | Type.App(s,_) when Symbol.eq sets.set_type s ->
                  [Lit.mk_subseteq ~sets [t_1] [term];Lit.mk_subseteq ~sets [term] [t_1]]
                | _ ->*) [Lit.mk_eq h term]
            in
            let f lit =
              let proof cc = Proof.mk_c_inference ~theories:["sets"]
              ~rule:"singleton_witness" cc [C.proof c] in
              let new_c = C.create ~parents:[c] [lit] proof in
              Util.debug 2 "    %a" C.pp new_c;
              new_c
            in
            List.map f new_lits
          in
          let new_clauses_A = List.map (create_clause_set ~sign:true) other in
          let new_clauses_B = List.map (create_clause_set ~sign:false) b in
          let new_clauses_t = Util.list_flatmap create_clause_term t in
          new_clauses_A @ new_clauses_B @ new_clauses_t @ acc)
    in new_clauses

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
    Ctx.Lit.add_from_hook (Lit.Conv.set_hook_from ~sets:!_theory);
    Ctx.Lit.add_to_hook (Lit.Conv.set_hook_to);
    Env.add_binary_inf "positive_chaining" positive_chaining;
    Env.add_binary_inf "negative_chaining_left" negative_chaining_left;
    Env.add_binary_inf "negative_chaining_right" negative_chaining_right;
    Env.add_unary_inf "sinlgleton_witness" singleton_witness;
    Env.add_unary_inf "reflexivity_res" reflexivity_res;
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

