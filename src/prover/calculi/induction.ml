
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition

module Lits = Literals
module T = FOTerm
module Su = Substs
module Ty = Type

module type S = Induction_intf.S

let section = Util.Section.make ~parent:Const.section "ind"

let cover_set_depth_ = ref 1
let show_lemmas_ = ref false

let cover_set_depth () = !cover_set_depth_

module Make
(E : Env.S)
(A : Avatar_intf.S with module E = E)
= struct
  module Env = E
  module Ctx = E.Ctx
  module C = E.C

  let lemmas_ = ref []

  (* annotate clauses that have been introduced by lemma *)
  let flag_cut_introduced = C.new_flag()

  let is_ind_conjecture_ c =
    match C.distance_to_conjecture c with
    | Some (0 | 1) -> true
    | Some _
    | None -> false

  let has_pos_lit_ c =
    CCArray.exists Literal.is_pos (C.lits c)

  let is_acceptable_lemma lits =
    (* not too deep *)
    Lits.Seq.terms lits
    |> Sequence.map T.depth
    |> Sequence.max
    |> CCOpt.maybe (fun d -> d < 5) true

  (* terms that are either inductive constants or sub-constants *)
  let constants_or_sub c =
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.symbols
    |> Sequence.filter
      (fun id -> Ind_types.is_cst id || Ind_types.is_sub_cst id)
    |> Sequence.sort_uniq ~cmp:ID.compare
    |> Sequence.to_rev_list

  (* sub-terms of an inductive type, that occur several times (candidate
     for "subterm generalization" *)
  let generalizable_subterms c =
    let count = T.Tbl.create 16 in
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t -> Ind_types.is_inductive_type (T.ty t) && not (T.is_const t))
    |> Sequence.iter
      (fun t ->
         let n = try T.Tbl.find count t with Not_found -> 0 in
         T.Tbl.replace count t (n+1)
      );
    (* terms that occur more than once *)
    T.Tbl.to_seq count
    |> Sequence.filter_map (fun (t,n) -> if n>1 then Some t else None)
    |> Sequence.to_rev_list

  (* apply the list of replacements [l] to the term [t] *)
  let replace_many l t =
    List.fold_left
      (fun t (old,by) -> T.replace t ~old ~by)
      t l

  (* when a unit clause has inductive constants, take its negation
      and add it as a lemma (some restrictions apply) *)
  let inf_introduce_lemmas c =
    let ind_csts = constants_or_sub c in
    let generalize ~on lit =
      (* fresh var generator *)
      let mk_fresh_var_ =
        let r = ref 0 in
        fun ty ->
          let v = T.var_of_int ~ty !r in
          incr r;
          v
      in
      (* abstract w.r.t all those constants (including the term
         being generalized). The latter must occur first, as it
         might contain constants being replaced. *)
      let replacements =
        List.map
          (fun t -> t, mk_fresh_var_ (T.ty t))
          (on @ ind_csts)
      in
      (* replace constants by variables in [lit], then
         let [c] be [forall... (not lit)] *)
      let lit =
        lit
        |> Literal.map (replace_many replacements)
        |> Literal.negate
      in
      let lits = [| lit |] in
      (* if [box lits] already exists or is too deep, no need to re-do inference *)
      if not (is_acceptable_lemma lits)
      then []
      else (
        (* introduce cut now *)
        let proof cc = Proof.mk_c_trivial ~theories:["ind"] ~info:["cut"] cc in
        let clauses, _ = Avatar.introduce_cut lits proof in
        List.iter (fun c -> C.set_flag flag_cut_introduced c true) clauses;
        Util.debugf ~section 2
          "@[<2>introduce cut@ from %a@ @[<hv0>%a@]@ generalizing on @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses
              (Util.pp_list T.pp) on);
        lemmas_ := List.rev_append clauses !lemmas_;
        clauses
      )
    in
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag flag_cut_introduced c)
    && C.is_unit_clause c
    && not (has_pos_lit_ c) (* only positive lemmas, therefore C negative *)
    && not (CCList.is_empty ind_csts) (* && not (T.Set.for_all CI.is_inductive set) *)
    then (
      assert (Array.length (C.lits c) = 1);
      let lit = (C.lits c).(0) in
      let terms = generalizable_subterms c in
      (* first, lemma without generalization;
          then, each possible way to generalize a subterm occurring multiple times *)
      List.rev_append
        (generalize ~on:[] lit)
        (CCList.flat_map (fun t -> generalize ~on:[t] lit) terms)
    ) else []

  let show_lemmas () =
    Util.debugf ~section 1 "@[<2>lemmas:@ [@[<hv>%a@]]@]"
      (fun k->k (Util.pp_list C.pp) !lemmas_)

  let () =
    Signal.on Signals.on_exit
      (fun _ ->
         if !show_lemmas_ then show_lemmas ();
         Signal.ContinueListening
      );

  (* scan clauses for ground terms of an inductive type,
     and declare those terms *)
  let scan seq : Ind_types.cst list =
    Sequence.map C.lits seq
    |> Sequence.flat_map find_inductive_cst
    |> Sequence.map
      (fun c ->
         declare c;
         CCOpt.get_exn (as_inductive c)
      )
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:Cst.compare

  (* TODO (similar to Avatar.introduce_lemma, should factorize this)
     - gather vars of c
     - make a fresh constant for each variable
     - replace variables by constants
     - for each lit, negate it and add [not lit <- trail] *)

  (* [cst] is the minimal term for which [ctx] holds, returns clauses
     expressing that (prepended to [acc]), and a boolean literal. *)
  let assert_min acc c ctx (cst:Cst.t) =
    match cover_set ~depth:(cover_set_depth()) cst with
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
    let trail = C.trail c |> Trail.to_seq in
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
            let res =
              not (Cst.equal i1 i2)
                  || (Cst.equal i1 i2 && not (Case.equal t1 t2)) in
            if res
            then (
              Util.debugf ~section 4
                "@[<2>clause@ @[%a@]@ redundant because of @[%a={%a,%a}@] in trail@]"
                (fun k->k C.pp c Cst.pp i1 Case.pp t1 Case.pp t2)
            );
            res
        | _ -> false)

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
           assert_min acc c ctx cst)
        [] consts
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


  (*
  module Meta = struct
    let declare_inductive p ity =
      let module CI = E.Ctx.Induction in
      let ity = Induction.make ity.CI.pattern ity.CI.constructors in
      Util.debugf ~section 2
        "@[<hv2>declare inductive type@ %a@]"
        (fun k->k Induction.print ity);
      let fact = Induction.t#to_fact ity in
      add_fact_ p fact

      (* declare inductive types *)
      E.Ctx.Induction.inductive_ty_seq
        (fun ity -> ignore (declare_inductive p ity));
      Signal.on E.Ctx.Induction.on_new_inductive_ty
        (fun ity ->
           ignore (declare_inductive p ity);
           Signal.ContinueListening
        );
  end
  *)
end

let extension =
  let action (module E : Env.S) =
    E.Ctx.lost_completeness ();
    E.Ctx.set_selection_fun Selection.no_select;
    let (module A) = Avatar.get_env (module E) in
    let module M = Make(E)(A) in
    M.register ();
  and add_constr c =
    (* add an ordering constraint: ensure that constructors are smaller
       than other terms *)
    Compute_prec.add_constr c 15 Ind_types.prec_constr in
  Extensions.(
    {default with
     name="induction_simple";
     actions=[Do action];
     prec_actions=[Prec_do add_constr];
    })

let init_from_decls pairs =
  let get_str = function
    | A.GNode (s, []) | A.GString s -> s
    | _ -> raise Exit
  in
  (* search for "inductive(c1, c2, ...)" *)
  let rec scan_for_constructors = function
    | A.GNode ("inductive", l) :: tail when List.length l >= 2 ->
        begin try
            let constructors = List.map get_str l in
            Some constructors
          with Exit ->
            scan_for_constructors tail
        end
    | _ :: tail -> scan_for_constructors tail
    | []  -> None
  in
  Sequence.iter
    (fun (ty, info) -> match scan_for_constructors info with
       | None -> ()
       | Some l ->
         assert false (* FIXME: should be done on Cnf.TyDecl? declare_ ty l *)
     )
    pairs

let on_enable = Signal.create()

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
    Util.debugf ~section 1
      "@[Induction: requires ord=rpo6; select=NoSelection@]" (fun k->k);
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.select := "NoSelection";
    Signal.send on_enable ();
  )

let () =
  Params.add_opts
    [ "--induction", Arg.Unit (Signal.send on_enable), " enable induction"
    ; "--induction-depth", Arg.Set_int cover_set_depth_,
        " set default induction depth"
    ; "--show-lemmas", Arg.Set show_lemmas_, " show inductive (candidate) lemmas"
    ]

let () =
  Signal.on IH.on_enable
    (fun () ->
      Extensions.register extension;
      Signal.ContinueListening)
