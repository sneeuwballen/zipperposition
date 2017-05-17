
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module BV = CCBV
module T = Term

let section = Util.Section.make ~parent:Const.section "ho"

let stat_eq_res = Util.mk_stat "ho.eq_res.steps"
let stat_ext_neg = Util.mk_stat "ho.extensionality-.steps"
let stat_complete_eq = Util.mk_stat "ho.complete_eq.steps"

let prof_eq_res = Util.mk_profiler "ho.eq_res"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let lit_is_unshielded_ho_unif (c:C.t) (lit:Literal.t): bool = match lit with
    | Literal.Equation (t, u, false) ->
      let lits = C.lits c in
      begin match T.as_var (T.head_term t), T.as_var (T.head_term u) with
        | Some v, _ when not (Purify.is_shielded v lits) -> true
        | _, Some v when not (Purify.is_shielded v lits) -> true
        | _ -> false
      end
    | _ -> false

  (* HO unif rule, applies to literals [F t != u] *)
  let eq_res_ (c:C.t) : C.t list =
    (* try HO unif with [l != r] *)
    let try_unif_ l r l_pos =
      let pos = Literals.Pos.idx l_pos in
      if BV.get (C.eligible_res_no_subst c) pos then (
        HO_unif.unif_step (Ctx.combinators (), 1) ((l,r),0)
        |> List.rev_map
          (fun (subst,subst_penalty) ->
             Util.incr_stat stat_eq_res;
             let renaming = Ctx.renaming_clear () in
             let rule = Proof.Rule.mk "ho_eq_res" in
             let proof = Proof.Step.inference ~rule
                 [C.proof_parent_subst (c,0) subst] in
             let new_lits = Literals.apply_subst ~renaming subst (C.lits c,0) in
             let trail = C.trail c in
             let penalty = C.penalty c + subst_penalty in
             let new_c = C.create_a ~trail ~penalty new_lits proof in
             Util.debugf ~section 3
               "(@[<hv2>ho_eq_res@ :on @[%a@]@ :yields @[%a@]@])"
               (fun k->k C.pp c C.pp new_c);
             new_c)
      ) else []
    in
    (* try negative HO unif lits that are also eligible for resolution *)
    let eligible = C.Eligible.(filter (lit_is_unshielded_ho_unif c) ** res c) in
    let new_clauses =
      Literals.fold_eqn ~sign:false ~ord:(Ctx.ord ())
        ~both:false ~eligible (C.lits c)
      |> Sequence.flat_map_l
        (fun (l, r, sign, l_pos) ->
           assert (not sign);
           try_unif_ l r l_pos)
      |> Sequence.to_rev_list
    in
    new_clauses

  let eq_res c = Util.with_prof prof_eq_res eq_res_ c

  let mk_parameter =
    let n = ref 0 in
    fun ty ->
      let i = CCRef.incr_then_get n in
      let id = ID.makef "k#%d" i in
      ID.set_payload id (ID.Attr_parameter i);
      T.const id ~ty

  (* negative extensionality rule:
     [f != g] where [f : a -> b] becomes [f k != g k] for a fresh parameter [k] *)
  let ext_neg (lit:Literal.t): Literal.t option = match lit with
    | Literal.Equation (f, g, false)
      when Type.is_fun (T.ty f) && not (T.is_var f) && not (T.is_var g) ->
      let n_ty_params, ty_args, _ = Type.open_poly_fun (T.ty f) in
      assert (n_ty_params=0);
      let params = List.map mk_parameter ty_args in
      let new_lit =
        Literal.mk_neq
          (T.app f params)
          (T.app g params)
      in
      Util.incr_stat stat_ext_neg;
      Util.debugf ~section 4
        "(@[ho_ext_neg@ :old `%a`@ :new `%a`@])"
        (fun k->k Literal.pp lit Literal.pp new_lit);
      Some new_lit
    | _ -> None

  (* complete [f = g] into [f x1…xn = g x1…xn] *)
  let complete_eq_args (c:C.t) : C.t list =
    let var_offset = C.Seq.vars c |> Type.Seq.max_var |> succ in
    let new_c =
      C.lits c
      |> Sequence.of_array |> Sequence.zip_i |> Sequence.zip
      |> Sequence.filter_map
        (fun (lit_idx,lit) -> match lit with
          | Literal.Equation (t, u, true) when Type.is_fun (T.ty t) ->
            let n_ty_args, ty_args, _ = Type.open_poly_fun (T.ty t) in
            assert (n_ty_args = 0);
            let vars =
              List.mapi
                (fun i ty -> HVar.make ~ty (i+var_offset) |> T.var)
                ty_args
            in
            let new_lit = Literal.mk_eq (T.app t vars) (T.app u vars) in
            let new_lits = new_lit :: CCArray.except_idx (C.lits c) lit_idx in
            let proof =
              Proof.Step.inference [C.proof_parent c]
                ~rule:(Proof.Rule.mk "ho_complete_eq")
            in
            let new_c =
              C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
            in
            Some new_c
          | _ -> None)
      |> Sequence.to_rev_list
    in
    if new_c<>[] then (
      Util.add_stat stat_complete_eq (List.length new_c);
      Util.debugf ~section 4
        "(@[complete-eq@ :clause %a@ :yields (@[<hv>%a@])@])"
        (fun k->k C.pp c (Util.pp_list ~sep:" " C.pp) new_c);
    );
    new_c


  (* TODO: predicate elimination *)

  (* ensure that combinators are defined functions *)
  let declare_combinators() =
    let module RW = Rewrite in
    let c = Ctx.combinators () in
    List.iter
      (fun (id,ty) -> Ctx.declare id ty)
      (HO_unif.Combinators.decls c);
    List.iter
      (fun (r,_) ->
         let id = RW.Term.Rule.head_id r in
         RW.Defined_cst.declare_or_add id (Rewrite.T_rule r))
      (HO_unif.Combinators.rules c);
    ()

  let setup () =
    Util.debug ~section 1 "setup HO rules";
    Env.Ctx.lost_completeness();
    (* force rules *)
    let () = ignore (HO_unif.Combinators.rules @@ Ctx.combinators ()) in
    declare_combinators ();
    Env.add_unary_inf "ho_eq_res" eq_res;
    Env.add_unary_inf "ho_complete_eq" complete_eq_args;
    Env.add_lit_rule "ho_ext_neg" ext_neg;
    ()
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()

let st_contains_ho (st:(_,_,_) Statement.t): bool =
  (* is there a HO variable? *)
  let has_ho_var () =
    Statement.Seq.terms st
    |> Sequence.flat_map T.Seq.vars
    |> Sequence.exists
      (fun v ->
         let n_ty_vars, args, _ = Type.open_poly_fun (HVar.ty v) in
         n_ty_vars > 0 || args<>[])
  (* is there a HO symbol? *)
  and has_ho_sym () =
    Statement.Seq.ty_decls st
    |> Sequence.exists (fun (_,ty) -> Type.order ty > 1)
  in
  has_ho_sym () || has_ho_var ()

let extension =
  let register env =
    let module E = (val env : Env.S) in
    if E.flex_get k_some_ho then (
      let module ET = Make(E) in
      ET.setup ()
    )
  (* check if there are HO variables *)
  and check_ho vec state =
    let is_ho =
      CCVector.to_seq vec
      |> Sequence.exists st_contains_ho
    in
    if is_ho then (
      Util.debug ~section 2 "problem is HO"
    );
    Flex_state.add k_some_ho is_ho state
  in
  { Extensions.default with
      Extensions.name = "ho";
      post_cnf_actions=[check_ho];
      env_actions=[register];
  }

let () = Extensions.register extension
