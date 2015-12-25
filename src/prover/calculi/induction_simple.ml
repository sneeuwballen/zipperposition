
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition

module Lits = Literals
module T = FOTerm
module Su = Substs
module Ty = Type
module IH = Induction_helpers

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val register : unit -> unit
end

let section = Util.Section.make ~parent:Const.section "ind"

module Make(E : Env.S)(Solver : Sat_solver.S) = struct
  module Env = E
  module Ctx = E.Ctx
  module CI = Ctx.Induction
  module C = E.C
  module Avatar = Avatar.Make(Env)(Solver)  (* will use some inferences *)

  module BoolLit = Ctx.BoolLit

  module IH_ctx = IH.Make(Ctx)
  module IHA = IH.MakeAvatar(Avatar)

  (* scan clauses for ground terms of an inductive type,
     and declare those terms *)
  let scan seq : CI.cst list =
    Sequence.map C.lits seq
    |> Sequence.flat_map IH_ctx.find_inductive_cst
    |> Sequence.map
      (fun c ->
         CI.declare c;
         CCOpt.get_exn (CI.as_inductive c)
      )
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:CI.Cst.compare

  let is_eq_ (t1:CI.cst) (t2:CI.case) = BoolLit.inject_case t1 t2

  (* TODO (similar to Avatar.introduce_lemma, should factorize this)
     - gather vars of c
     - make a fresh constant for each variable
     - replace variables by constants
     - for each lit, negate it and add [not lit <- trail] *)

  (* [cst] is the minimal term for which [ctx] holds, returns clauses
     expressing that (prepended to [acc]), and a boolean literal. *)
  let assert_min acc c ctx (cst:CI.cst) =
    match CI.cover_set ~depth:(IH.cover_set_depth()) cst with
    | _, `Old -> acc  (* already declared constant *)
    | set, `New ->
        (* for each member [t] of the cover set:
           - add ctx[t] <- [cst=t]
           - for each [t' subterm t] of same type, add ~[ctx[t']] <- [cst=t]
        *)
        let acc, b_lits =
          Sequence.fold
            (fun (acc, b_lits) (case:CI.case) ->
               let b_lit = is_eq_ cst case in
               (* ctx[case] <- b_lit *)
               let c_case = C.create_a ~parents:[c]
                   ~trail:Trail.(singleton b_lit)
                   (ClauseContext.apply ctx (case:>T.t))
                   (fun cc -> Proof.mk_c_inference ~theories:["ind"]
                       ~rule:"split" cc [C.proof c]
                   )
               in
               (* ~ctx[t'] <- b_lit for each t' subterm case *)
               let c_sub =
                 Sequence.fold
                   (fun c_sub (sub:CI.sub_cst) ->
                      (* ~[ctx[sub]] <- b_lit *)
                      let clauses = assert false
                      (* FIXME
                         let lits = ClauseContext.apply ctx (sub:>T.t) in
                         let f =
                         lits
                         |> Literals.to_form
                         |> F.close_forall
                         |> F.Base.not_
                         in
                         let proof = Proof.mk_f_inference ~theories:["ind"]
                          ~rule:"min" f [C.proof c]
                         in
                         PFormula.create f proof
                         |> PFormula.Set.singleton
                         |> Env.cnf
                         |> C.CSet.to_list
                         |> List.map (C.update_trail (C.Trail.add b_lit))
                      *)
                      in
                      clauses @ c_sub
                   ) [] (CI.sub_constants_case case)
               in
               Util.debugf ~section 2
                 "@[<2>minimality of %a@ in case %a:@ @[<hv>%a@]@]"
                 (fun k->k ClauseContext.pp ctx CI.Case.pp case
                     (Util.pp_list C.pp) (c_case :: c_sub));
               (* return new clauses and b_lit *)
               c_case :: c_sub @ acc, b_lit :: b_lits
            ) (acc, []) (CI.cases set)
        in
        (* boolean constraint *)
        (* FIXME: generate boolean clause(s) instead
           let qform = (QF.imply
                       (qform_of_trail (C.get_trail c))
                       (QF.xor_l (List.map QF.atom b_lits))
                    ) in

           Util.debugf ~section 2 "@[<2>add boolean constr@ @[%a@]@]"
           (fun k->k (QF.print_with ~pp_lit:BoolLit.print) qform);
           Solver.add_form ~tag:(C.id c) qform;
        *)
        Avatar.save_clause ~tag:(C.id c) c;
        acc

  (* checks whether the trail of [c] is trivial, that is:
     - contains two literals [i = t1] and [i = t2] with [t1], [t2]
        distinct cover set members, or
     - two literals [loop(i) minimal by a] and [loop(i) minimal by b], or
     - two literals [C in loop(i)], [D in loop(j)] if i,j do not depend
        on one another *)
  let has_trivial_trail c =
    let trail = C.get_trail c |> Trail.to_seq in
    (* all i=t where i is inductive *)
    let relevant_cases =
      trail
      |> Sequence.filter_map
        (fun blit ->
           match BoolLit.extract (Bool_lit.abs blit) with
           | None -> None
           | Some (BoolLit.Case (l, r)) -> Some (`Case (l, r))
           | Some _ -> None
        )
    in
    (* is there i such that   i=t1 and i=t2 can be found in the trail? *)
    Sequence.product relevant_cases relevant_cases
    |> Sequence.exists
      (function
        | (`Case (i1, t1), `Case (i2, t2)) ->
            let res = not (CI.Cst.equal i1 i2)
                      || (CI.Cst.equal i1 i2 && not (CI.Case.equal t1 t2)) in
            if res
            then (
              Util.debugf ~section 4
                "@[<2>clause@ @[%a@]@ redundant because of @[%a={%a,%a}@] in trail@]"
                (fun k->k C.pp c CI.Cst.pp i1 CI.Case.pp t1 CI.Case.pp t2)
            );
            res
        | _ -> false
      )

  exception FoundInductiveLit of int * (T.t * T.t) list

  (* if c is  f(t1,...,tn) != f(t1',...,tn') or d, with f inductive symbol, then
      replace c with    t1 != t1' or ... or tn != tn' or d *)
  let injectivity_destruct c =
    try
      let eligible = C.Eligible.(filter Literal.is_neq) in
      Lits.fold_lits ~eligible (C.lits c)
      |> Sequence.iter
        (fun (lit, i) -> match lit with
           | Literal.Equation (l, r, false) ->
               begin match T.Classic.view l, T.Classic.view r with
                 | T.Classic.App (s1, l1), T.Classic.App (s2, l2)
                   when ID.equal s1 s2
                     && CI.is_constructor_sym s1
                   ->
                     (* destruct *)
                     assert (List.length l1 = List.length l2);
                     let pairs = List.combine l1 l2 in
                     raise (FoundInductiveLit (i, pairs))
                 | _ -> ()
               end
           | _ -> ()
        );
      c (* nothing happened *)
    with FoundInductiveLit (idx, pairs) ->
      let lits = CCArray.except_idx (C.lits c) idx in
      let new_lits = List.map (fun (t1,t2) -> Literal.mk_neq t1 t2) pairs in
      let proof cc = Proof.mk_c_inference ~theories:["induction"]
          ~rule:"injectivity_destruct" cc [C.proof c]
      in
      let c' = C.create ~trail:(C.get_trail c) ~parents:[c] (new_lits @ lits) proof in
      Util.debugf ~section 3 "@[<hv2>injectivity:@ simplify @[%a@]@ into @[%a@]@]"
        (fun k->k C.pp c C.pp c');
      c'

  (* when a clause contains new inductive constants, assert minimality
     of the clause for all those constants independently *)
  let inf_assert_minimal c =
    let consts = scan (Sequence.singleton c) in
    let clauses = List.fold_left
        (fun acc cst ->
           let ctx = ClauseContext.extract_exn (C.lits c) (cst:CI.cst:>T.t) in
           assert_min acc c ctx cst
        ) [] consts
    in
    clauses

  let register () =
    Util.debug ~section 2 "register induction_lemmas";
    IH_ctx.declare_types ();
    Avatar.register (); (* avatar inferences, too *)
    (* FIXME: move to Extension, probably, so it can be added
       to {!Compute_prec} before computing precedence
       Ctx.add_constr 20 IH_ctx.constr_sub_cst;  (* enforce new constraint *)
    *)
    Env.add_unary_inf "induction_lemmas.cut" IHA.inf_introduce_lemmas;
    Env.add_unary_inf "induction_lemmas.ind" inf_assert_minimal;
    Env.add_is_trivial has_trivial_trail;
    Env.add_simplify injectivity_destruct;
    ()
end

(* FIXME: do not duplicate Avatar *)

let extension =
  let action (module E : Env.S) =
    E.Ctx.lost_completeness ();
    let module Solver = Sat_solver.Make(struct end) in
    let module A = Make(E)(Solver) in
    A.register();
    (* add an ordering constraint: ensure that constructors are smaller
       than other terms *)
  and add_constr c = Compute_prec.add_constr c 15 IH.constr_cstors in
  Extensions.(
    {default with
     name="induction_simple";
     actions=[Do action];
     prec_actions=[Prec_do add_constr];
    })

let () =
  Signal.on IH.on_enable
    (fun () ->
      Extensions.register extension;
      Signal.ContinueListening)
