
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

type ll_subst = LLProof.subst

let conv_subst ~ctx (p:Subst.Projection.t) : ll_subst =
  List.fold_left
    (fun lsubst (v,t) ->
       let v = Type.cast_var_unsafe v in
       let prefix = if Type.is_tType (HVar.ty v) then "A" else "X" in
       let v = Term.Conv.var_to_simple_var ~prefix ctx v in
       let t = Term.Conv.to_simple_term ctx (Term.of_term_unsafe t) in
       assert (not (Var.Subst.mem lsubst v));
       Var.Subst.add lsubst v t)
    Var.Subst.empty
    (Subst.Projection.bindings p)

let conv (p:Proof.t) : LLProof.t =
  let tbl = Proof.S.Tbl.create 32 in
  let rec conv ?ctx p: LLProof.t =
    begin match Proof.S.Tbl.get tbl p with
      | Some r -> r
      | None ->
        let res = conv_step ?ctx p in
        Proof.S.Tbl.add tbl p res;
        res
    end
  and conv_step ?(ctx=Term.Conv.create()) p =
    let res = Proof.Result.to_form ~ctx (Proof.S.result p) in
    let parents =
      List.map conv_parent (Proof.Step.parents @@ Proof.S.step p)
    and parent_as_proof_exn = function
      | LLProof.P_of c -> c
      | LLProof.P_instantiate _ -> assert false
    in
    begin match Proof.Step.kind @@ Proof.S.step p with
      | Proof.Inference (rule,c)
      | Proof.Simplification (rule,c) ->
        LLProof.inference c res (Proof.Rule.name rule) parents
      | Proof.Esa (rule,c) ->
        let l = List.map parent_as_proof_exn parents in
        LLProof.esa c res (Proof.Rule.name rule) l
      | Proof.Trivial -> LLProof.trivial res
      | Proof.By_def id -> LLProof.by_def id res
      | Proof.Define (id,_) -> LLProof.define id res
      | Proof.Intro (_,Proof.R_assert) -> LLProof.assert_ res
      | Proof.Intro (_,Proof.R_goal) -> LLProof.assert_ res
      | Proof.Intro (_,(Proof.R_lemma|Proof.R_def|Proof.R_decl)) ->
        LLProof.trivial res
    end
  and conv_parent (p:Proof.Parent.t): LLProof.parent = match p with
    | Proof.P_of p -> LLProof.p_of (conv p)
    | Proof.P_subst (p,subst) ->
      let ctx = Term.Conv.create() in
      let p = conv ~ctx p in
      let subst = conv_subst ~ctx subst in
      LLProof.p_instantiate p subst
  in
  conv p
