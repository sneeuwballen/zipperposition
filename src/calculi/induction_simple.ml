
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Induction through Cut} *)

module Sym = Logtk.Symbol
module Util = Logtk.Util
module Lits = Literals
module T = Logtk.FOTerm
module F = Logtk.Formula.FO
module Su = Logtk.Substs
module Ty = Logtk.Type
module IH = Induction_helpers

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val register : unit -> unit
end

let section_bool = BoolSolver.section

let section = Util.Section.make
  ~inheriting:[section_bool] ~parent:Const.section "ind"

module Make(E : Env.S)(Solver : BoolSolver.SAT) = struct
  module Env = E
  module Ctx = E.Ctx
  module CI = Ctx.Induction
  module C = E.C
  module Avatar = Avatar.Make(Env)(Solver)  (* will use some inferences *)

  module BoolLit = Ctx.BoolLit

  module IH_ctx = IH.Make(Ctx)
  module IHA = IH.MakeAvatar(Avatar)
  module QF = Qbf.Formula

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

  let qform_of_trail t = QF.and_map (C.Trail.to_seq t) ~f:QF.atom

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
        let acc, b_lits = Sequence.fold
          (fun (acc, b_lits) (case:CI.case) ->
            let b_lit = is_eq_ cst case in
            (* ctx[case] <- b_lit *)
            let c_case = C.create_a ~parents:[c]
              ~trail:C.Trail.(singleton b_lit)
              (ClauseContext.apply ctx (case:>T.t))
              (fun cc -> Proof.mk_c_inference ~theories:["ind"]
                ~rule:"split" cc [C.proof c]
              )
            in
            (* ~ctx[t'] <- b_lit for each t' subterm case *)
            let c_sub = Sequence.fold
              (fun c_sub (sub:CI.sub_cst) ->
                (* ~[ctx[sub]] <- b_lit *)
                let clauses =
                  let lits = ClauseContext.apply ctx (sub:>T.t) in
                  let f = lits
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
                in
                clauses @ c_sub
              ) [] (CI.sub_constants_case case)
            in
            Util.debugf ~section 2
              "@[<2>minimality of %a@ in case %a:@ @[<hv0>%a@]@]"
              ClauseContext.print ctx CI.Case.print case
              (CCList.print ~start:"" ~stop:"" C.fmt) (c_case :: c_sub);
            (* return new clauses and b_lit *)
            c_case :: c_sub @ acc, b_lit :: b_lits
          ) (acc, []) (CI.cases set)
        in
        (* boolean constraint *)
        let qform = (QF.imply
            (qform_of_trail (C.get_trail c))
            (QF.xor_l (List.map QF.atom b_lits))
        ) in
        Util.debugf ~section 2 "@[<2>add boolean constr@ @[%a@]@]"
          (QF.print_with ~pp_lit:BoolLit.print) qform;
        Avatar.save_clause ~tag:(C.id c) c;
        Solver.add_form ~tag:(C.id c) qform;
        acc

  (* when a clause contains new inductive constants, assert minimality
    of the clause for all those constants independently *)
  let inf_assert_minimal c =
    let consts = scan (Sequence.singleton c) in
    let clauses, bool_lits = List.fold_left
      (fun acc cst ->
        let ctx = ClauseContext.extract_exn (C.lits c) (cst:CI.cst:>T.t) in
        assert_min acc c ctx cst
      ) ([], []) consts
    in
    Solver.add_form ~tag:(C.id c) (QF.or_l consts)
    clauses

  let register () =
    Util.debug ~section 2 "register induction_lemmas";
    IH_ctx.declare_types ();
    Avatar.register (); (* avatar inferences, too *)
    Ctx.add_constr 20 IH_ctx.constr_sub_cst;  (* enforce new constraint *)
    Env.add_unary_inf "induction_lemmas.cut" IHA.inf_introduce_lemmas;
    Env.add_unary_inf "induction_lemmas.ind" inf_assert_minimal;
    (* no collisions with Skolemization please *)
    Signal.once Env.on_start IHA.clear_skolem_ctx;
    ()
end

let extension =
  let action (module E : Env.S) =
    E.Ctx.lost_completeness ();
    let module Solver = (val BoolSolver.get_sat() : BoolSolver.SAT) in
    Util.debug ~section:section_bool 2
      "created SAT solver \"%s\"" Solver.name;
    let module A = Make(E)(Solver) in
    A.register();
  (* add an ordering constraint: ensure that constructors are smaller
    than other terms *)
  and add_constr penv = PEnv.add_constr ~penv 15 IH.constr_cstors in
  Extensions.({default with
    name="induction_lemma";
    actions=[Do action];
    penv_actions=[Penv_do add_constr];
  })

let () =
  Signal.on IH.on_enable
    (function
      | `Simple ->
        Extensions.register extension;
        Signal.ContinueListening
      | `Full -> Signal.ContinueListening
    )
