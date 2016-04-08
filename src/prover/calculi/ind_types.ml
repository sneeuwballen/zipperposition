
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module Lits = Literals
module T = FOTerm
module Su = Substs

let section = Ind_ty.section

(** {1 Deal with Inductive Types} *)
module Make(Env : Env_intf.S) = struct
  module C = Env.C

  let acyclicity lit =
    (* check if [sub] occurs in [t] under a constructor, recursively. Stop
        before entering non-constructor terms *)
    let rec occurs_in_ t ~sub =  match T.view t with
      | T.App (f, l) ->
        begin match T.view f with
          | T.Const id when Ind_ty.is_constructor id ->
            List.exists
              (fun t' -> T.equal sub t' || occurs_in_ t' ~sub)
              l
          | _ -> false
        end
      | T.Const _
      | T.Var _
      | T.DB _
      | T.AppBuiltin _ -> false
    in
    match lit with
    | Literal.Equation (l, r, b) ->
        if
          ( Ind_ty.is_inductive_type (T.ty l) && occurs_in_ ~sub:l r )
          ||
          ( Ind_ty.is_inductive_type (T.ty r) && occurs_in_ ~sub:r l )
        then if b then `Absurd else `Trivial else `Neither
    | _ -> `Neither

  let acyclicity_trivial c =
    let res = C.Seq.lits c
      |> Sequence.exists
        (fun lit -> match acyclicity lit with
          | `Neither
          | `Absurd -> false
          | `Trivial -> true
        )
    in
    if res
    then Util.debugf ~section 3 "@[<2>acyclicity:@ `@[%a@]` is trivial@]" (fun k->k C.pp c);
    res

  let acyclicity_simplify c =
    let lits' = C.Seq.lits c
      |> Sequence.filter
        (fun lit -> match acyclicity lit with
          | `Neither
          | `Trivial -> true
          | `Absurd -> false (* remove lit *)
        )
      |> Sequence.to_array
    in
    if Array.length lits' = Array.length (C.lits c)
    then SimplM.return_same c
    else (
      let proof =
        ProofStep.mk_inference ~rule:(ProofStep.mk_rule "acyclicity") [C.proof c] in
      let c' = C.create_a ~trail:(C.trail c) lits' proof in
      Util.debugf ~section 3
        "@[<2>acyclicity:@ simplify `@[%a@]`@ into `@[%a@]`@]" (fun k->k C.pp c C.pp c');
      SimplM.return_new c'
    )

  exception FoundInductiveLit of int * (T.t * T.t) list

  (* if c is  f(t1,...,tn) != f(t1',...,tn') or d, with f inductive cstor, then
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
                     && Ind_ty.is_constructor s1
                   ->
                     (* destruct *)
                     assert (List.length l1 = List.length l2);
                     let pairs = List.combine l1 l2 in
                     raise (FoundInductiveLit (i, pairs))
                 | _ -> ()
               end
           | _ -> ()
        );
      SimplM.return_same c (* nothing happened *)
    with FoundInductiveLit (idx, pairs) ->
      let lits = CCArray.except_idx (C.lits c) idx in
      let new_lits = List.map (fun (t1,t2) -> Literal.mk_neq t1 t2) pairs in
      let rule = ProofStep.mk_rule ~comment:["induction"] "injectivity_destruct" in
      let proof = ProofStep.mk_inference ~rule [C.proof c] in
      let c' = C.create ~trail:(C.trail c) (new_lits @ lits) proof in
      Util.debugf ~section 3 "@[<hv2>injectivity:@ simplify @[%a@]@ into @[%a@]@]"
        (fun k->k C.pp c C.pp c');
      SimplM.return_new c'

  let setup() =
    Util.debug ~section 2 "setup inductive types calculus";
    Env.add_is_trivial acyclicity_trivial;
    Env.add_simplify acyclicity_simplify;
    Env.add_simplify injectivity_destruct;
    ()
end

let env_act (module E : Env_intf.S) =
  let module M = Make(E) in
  M.setup ()

let extension =
  let open Extensions in
  { default with
    name="ind_types";
    env_actions=[env_act]
  }
