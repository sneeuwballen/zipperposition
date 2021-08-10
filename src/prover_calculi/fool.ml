
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

type term = Term.t
let section = Util.Section.make ~parent:Const.section "fool"

let stat_fool_param = Util.mk_stat "fool.param_step"
let stat_elim_var = Util.mk_stat "fool.elim_var"

let enabled_ = ref true

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit
  val rw_bool_lits : Env.multi_simpl_rule

  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  (* replace [sub] by [true/false] in [c], obtaining a new clause *)
  let fool_param_sign ~sub sign c =
    let lits =
      C.lits c
      |> Literals.map (T.replace ~old:sub ~by:(if sign then T.true_ else T.false_))
      |> Array.to_list
    in
    let new_lit = Literal.mk_eq sub (if sign then T.false_ else T.true_) in
    let proof =
      Proof.Step.inference [C.proof_parent c]
        ~rule:(Proof.Rule.mk"fool_param")
    in
    let new_c =
      C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
        (new_lit :: lits) proof
    in
    Util.debugf ~section 5 "... deduce `@[%a@]`" (fun k->k C.pp new_c);
    new_c

  (* TODO: do not perform this under a connective (¬, ∧) or boolean combinator *)
  let fool_param c : C.t list =
    let sub_terms =
      C.Seq.terms c
      |> Iter.flat_map
        (fun t ->
           T.Seq.subterms_depth t
           |> Iter.filter_map (fun (t,d) -> if d>0  then Some t else None))
      |> Iter.filter
        (fun t ->
           Type.is_prop (T.ty t) &&
           T.DB.is_closed t &&
           begin match T.view t with
             | T.Const _ | T.App _ -> true
             | T.AppBuiltin ((Builtin.True | Builtin.False), _) -> false
             | T.AppBuiltin (_, _) -> true
             | T.Var _ | T.DB _ -> false
             | T.Fun _ -> assert false (* by typing *)
           end)
      |> T.Set.of_iter
    in
    if not (T.Set.is_empty sub_terms) then (
      Util.debugf ~section 5
        "@[<2>in clause `@[%a@]`@ possible subterms are [@[<hv>%a@]]@]"
        (fun k->k C.pp c (T.Set.pp ~pp_sep:(CCFormat.return ",@,") T.pp) sub_terms);
    );
    begin
      T.Set.to_iter sub_terms
      |> Iter.flat_map_l
        (fun sub -> [fool_param_sign ~sub true c; fool_param_sign ~sub false c])
      |> Iter.to_rev_list
      |> CCFun.tap
        (fun l ->
           if l<>[] then (
             Util.add_stat stat_fool_param (List.length l);
             Util.debugf ~section 4
               "(@[<2>fool_param@ :clause %a@ :yields (@[<hv>%a@])@])"
               (fun k->k C.pp c (Util.pp_list C.pp) l);
           ))
    end

  (* eliminate [P ∨ C] into [C[P := ⊥]] (and same for [¬P]) *)
  let fool_elim_var (c:C.t) : C.t list =
    C.lits c
    |> Iter.of_array_i
    |> Iter.filter_map
      (fun (idx,lit) -> 
        assert(Literal.no_prop_invariant lit);
        match lit with
         (* NOTE: -- Relies on the representation of literals  -- *)
         | Literal.Equation (lhs, rhs, true) when T.is_true_or_false rhs ->
           begin match T.as_var lhs with
             | Some v -> 
               (* found var, replace it with [not sign] *)
               let t = if T.equal rhs T.true_ then T.false_ else T.true_ in
               let subst =
                 Subst.FO.of_list' [(v,0), (t,0)]
               in
               let new_lits = CCArray.except_idx (C.lits c) idx in
               let renaming = Subst.Renaming.create() in
               let new_lits =
                 Literal.apply_subst_list renaming subst (new_lits,0)
               in
               let proof =
                 Proof.Step.inference ~rule:(Proof.Rule.mk "fool.elim_var")
                   [ C.proof_parent_subst renaming (c,0) subst ]
               in
               let new_c =
                 C.create new_lits proof
                   ~penalty:(C.penalty c) ~trail:(C.trail c)
               in

               Util.incr_stat stat_elim_var;
               Util.debugf ~section 3
                 "(@[elim_pred_var@ :var %a :into %B@ :clause %a@ :yield %a@])"
                 (fun k->k T.pp_var v (T.equal rhs T.true_) C.pp c C.pp new_c);
               Some new_c
             | _ -> None
           end
         | _ -> None)
    |> Iter.to_rev_list

  (* rewrite some boolean literals:
     - [a ≠_o b --> a || b && (¬ a || ¬b)], i.e. exclusive or
     - [(∧ a b) --> a ∧ b]
     - [(∨ a b) --> a ∨ b]
  *)
  let rw_bool_lits : E.multi_simpl_rule = fun c ->
    let is_bool_val t =
      T.equal t T.true_ || T.equal t T.false_ || T.is_var t || T.is_ho_app t
    in
    (* how to build a new clause *)
    let mk_c lits =
      let proof = Proof.Step.simp ~rule:(Proof.Rule.mk "cnf_fool")
          [Proof.Parent.from @@ C.proof c]
      in
      C.create lits proof
        ~penalty:(C.penalty c) ~trail:(C.trail c)
    in
    C.lits c
    |> CCArray.find_map_i
      (fun i lit -> match lit with
         | Literal.Equation (a, b, false)
           when Type.is_prop (T.ty a) &&
                not (is_bool_val a) &&
                not (is_bool_val b) ->
           let lits = CCArray.except_idx (C.lits c) i in
           let c_pos = Literal.mk_true a :: Literal.mk_true b :: lits |> mk_c in
           let c_neg = Literal.mk_false a :: Literal.mk_false b :: lits |> mk_c in
           Some [c_pos; c_neg]
         | Literal.Equation (lhs, rhs, true)
           when (T.equal rhs T.true_) || (T.equal rhs T.false_) ->
           (* NOTE: based on literal representation *)
           (* see if there is some CNF to do here *)
           let sign = T.equal rhs T.true_ in
           begin match T.view lhs, sign with
             | T.AppBuiltin (Builtin.And, l), true
             | T.AppBuiltin (Builtin.Or, l), false ->
               let lits = CCArray.except_idx (C.lits c) i in
               l
               |> List.map (fun t -> Literal.mk_prop t sign :: lits |> mk_c)
               |> CCOpt.return
             | T.AppBuiltin (Builtin.Or, l), true
             | T.AppBuiltin (Builtin.And, l), false ->
               let lits = CCArray.except_idx (C.lits c) i in
               (List.map (fun t -> Literal.mk_prop t sign) l @ lits)
               |> mk_c |> CCList.return |> CCOpt.return
             | T.AppBuiltin (Builtin.Eq, [_;t;u]), _ ->
               let lits = CCArray.except_idx (C.lits c) i in
               let lit = Literal.mk_lit t u sign in
               Some [mk_c (lit::lits)]
             | _ -> None
           end
         | Literal.Equation (a, b, true)
           when Type.is_prop (T.ty a) &&
                not (is_bool_val a) &&
                not (is_bool_val b) ->
           let lits = CCArray.except_idx (C.lits c) i in
           let c_a_imp_b = Literal.mk_false a :: Literal.mk_true b :: lits |> mk_c in
           let c_b_imp_a = Literal.mk_false b :: Literal.mk_true a :: lits |> mk_c in
           Some [c_a_imp_b; c_b_imp_a]
         | _ -> None)

  let setup () =
    Util.debug ~section 1 "setup fool rules";
    Env.add_unary_inf "fool_param" fool_param;
    Env.add_multi_simpl_rule ~priority:5 rw_bool_lits;
    Env.add_unary_inf "fool_elim_var" fool_elim_var;
    ()
end

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    if !enabled_
    then ET.setup ()
  in
  { Extensions.default with Extensions.
                         name = "fool";
                         env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--fool", Arg.Bool (fun v -> enabled_ := v), " enable/disable fool (first-class booleans)"  ];
  Params.add_to_modes 
    [ "ho-complete-basic"
    ; "ho-pragmatic"
    ; "ho-competitive"
    ; "fo-complete-basic"
    ; "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "ho-comb-complete"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] 
    (fun () ->
       enabled_ := false
    );
  Extensions.register extension
