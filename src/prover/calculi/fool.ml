
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module T = Term

type term = Term.t
let section = Util.Section.make ~parent:Const.section "fool"

let stat_fool_param = Util.mk_stat "fool.param_step"

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

  let fool_param c : C.t list =
    let sub_terms =
      C.Seq.terms c
      |> Sequence.flat_map
        (fun t ->
           T.Seq.subterms_depth t
           |> Sequence.filter_map (fun (t,d) -> if d>0  then Some t else None))
      |> Sequence.filter
        (fun t ->
           Type.is_prop (T.ty t)
           &&
           begin match T.view t with
             | T.Const _ | T.App _ -> true
             | T.AppBuiltin ((Builtin.True | Builtin.False), _) -> false
             | T.AppBuiltin (_, _) -> true
             | T.Var _ | T.DB _ -> false
           end)
      |> T.Set.of_seq
    in
    if not (T.Set.is_empty sub_terms) then (
      Util.debugf ~section 5
        "@[<2>in clause `@[%a@]`@ possible subterms are [@[<hv>%a@]]@]"
        (fun k->k C.pp c (T.Set.pp ~sep:"," T.pp) sub_terms);
    );
    begin
      T.Set.to_seq sub_terms
      |> Sequence.flat_map_l
        (fun sub -> [fool_param_sign ~sub true c; fool_param_sign ~sub false c])
      |> Sequence.to_rev_list
      |> CCFun.tap
        (fun l ->
           if l<>[] then (
             Util.add_stat stat_fool_param (List.length l);
             Util.debugf ~section 4
               "(@[<2>fool_param@ :clause %a@ :yields (@[<hv>%a@])@])"
               (fun k->k C.pp c (Util.pp_list C.pp) l);
           ))
    end

  (* rewrite some boolean literals:
     - [a ≠_o b --> a || b && (¬ a || ¬b)], i.e. exclusive or
     - [(∧ a b) --> a ∧ b]
     - [(∨ a b) --> a ∨ b]
  *)
  let rw_bool_lits : E.multi_simpl_rule = fun c ->
    let is_bool_val t = T.equal t T.true_ || T.equal t T.false_ in
    (* how to build a new clause *)
    let mk_c lits =
      let proof = Proof.Step.simp ~rule:(Proof.Rule.mk "fool_simp")
          [Proof.Parent.from @@ C.proof c]
      in
      C.create lits proof
        ~penalty:(C.penalty c) ~trail:(C.trail c)
    in
    C.lits c
    |> CCArray.findi
      (fun i lit -> match lit with
         | Literal.Equation (a, b, false)
           when Type.is_prop (T.ty a) &&
                not (is_bool_val a) &&
                not (is_bool_val b) ->
           let lits = CCArray.except_idx (C.lits c) i in
           let c_pos = Literal.mk_true a :: Literal.mk_true b :: lits |> mk_c in
           let c_neg = Literal.mk_false a :: Literal.mk_false b :: lits |> mk_c in
           Some [c_pos; c_neg]
         | Literal.Prop (t, sign) ->
           (* see if there is some CNF to do here *)
           begin match T.view t, sign with
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

             | _ -> None
           end
         | _ -> None)

  let setup () =
    Util.debug ~section 1 "setup fool rules";
    Env.add_unary_inf "fool_param" fool_param;
    Env.add_multi_simpl_rule rw_bool_lits;
    ()
end

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with Extensions.
                         name = "fool";
                         env_actions=[register];
  }

let () = Extensions.register extension
