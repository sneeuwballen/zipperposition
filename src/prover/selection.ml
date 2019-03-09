
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = Term
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV

type t = Literal.t array -> CCBV.t

type parametrized = strict:bool -> ord:Ordering.t -> t

(* no need for classification here *)
let no_select _ : BV.t = BV.empty ()

let _ho_restriction = ref `None

(* May we select this literal? *)
let can_select_lit ~ord (lits:Lits.t) (i:int) : bool =
  if Lit.is_neg lits.(i)
  then (
    (* Search for the (maximal) terms with variable heads.
       Returns a list of (var_head, args) pairs. *)
    let var_headed_subterms which =
      let lits =
        if which = `Max
        then Lits.maxlits_l ~ord lits |> CCList.map (fun (l,_) -> l) |> Array.of_list
        else lits
      in
      lits |> CCArray.fold (
        fun vars lit ->
          let new_vars =
            Lit.fold_terms ~vars:true ~ty_args:false ~which ~ord ~subterms:false lit
            |> Iter.map (fun (t,_) -> T.as_app t)
            |> Iter.filter (fun (head,_) -> T.is_var head)
            |> Iter.to_list
          in
          CCList.append new_vars vars
      ) [] in
    (* Given a list of (var_head, args) pairs, check whether our lit contains
       one of those variables, but with different arguments. *)
    let occur_with_other_args vars_args =
        Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
        |> Iter.exists (fun (t,_) ->
            let t_head, t_args = T.as_app t in
            vars_args |> CCList.exists (fun (head, args) -> head = t_head && t_args != args)
          )
      in
    match !_ho_restriction with
      | `None -> true
      | `NoVarHeadingMaxTerm ->
        (* Don't select literals containing a variable that heads the maximal term
           of the clause but occurs with different arguments there. *)
        let vars_args = var_headed_subterms `Max in
        not (occur_with_other_args vars_args)
      | `NoVarDifferentArgs ->
        (* Don't selected if the literal contains a variable that is applied to
           different arguments in the clause *)
        let vars_args = var_headed_subterms `All in
        not (occur_with_other_args vars_args)
      | `NoUnappliedVarOccurringApplied ->
        (* Don't selected if the literal contains an unapplied variable that also
           occurs applied in the clause *)
        let vars_args = var_headed_subterms `All in
        Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
        |> Iter.exists (fun (t,_) ->
            vars_args |> CCList.exists (fun (head, args) -> head = t && not (CCList.is_empty args))
          )
      | `NoHigherOrderVariables ->
        (* We cannot select literals containing a HO variable: *)
        not (
          Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lits.(i)
          |> Iter.exists (fun (t,_) -> T.is_ho_var (fst (T.as_app t)))
        )
  )
  else false

(* checks that [bv] is an acceptable selection for [lits]. In case
   some literal is selected, at least one negative literal must be selected. *)
let validate_fun_ ~ord lits bv =
  if BV.is_empty bv then true
  else (
    Iter.of_array_i lits
    |> Iter.exists
      (fun (i,_) -> can_select_lit ~ord lits i && BV.get bv i)
  )

(* build a selection function in general, given the more specialized
   one there *)
let mk_ ~ord ~(f:Lits.t -> BV.t) (lits:Lits.t) : BV.t =
  if Array.length lits <= 1 then BV.empty ()
  else (
    (* should we select anything? *)
    let should_select = CCList.exists (can_select_lit ~ord lits) (CCList.range' 0 (CCArray.length lits)) in
    if should_select then (
      let bv = f lits in
      assert (validate_fun_ ~ord lits bv);
      bv
    ) else (
      (*Util.debugf ~section 5 "(@[should-not-select@ %a@])" (fun k->k Lits.pp lits);*)
      BV.empty ()
    )
  )

let bv_first_ bv = BV.iter_true bv |> Iter.head

let max_goal ~strict ~ord lits =
  mk_ ~ord lits ~f:(fun lits ->
    let bv = Lits.maxlits ~ord lits in
    (* only retain negative normal lits, or constraints
       that are unshielded *)
    BV.filter bv (fun i -> can_select_lit ~ord lits i);
    begin match bv_first_ bv with
      | Some i ->
        (* keep only first satisfying lit *)
        BV.clear bv;
        BV.set bv i;
        if not strict then (
          BV.union_into ~into:bv (Lits.pos lits);
        );
        bv
      | None ->
        BV.empty ()  (* empty one *)
    end)

let except_RR_horn (p:parametrized) ~strict ~ord lits =
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else p ~strict ~ord lits  (* delegate *)

(** {2 Global selection Functions} *)

let default = max_goal ~strict:true

let l =
  let basics =
    [ "NoSelection", (fun ~ord:_ -> no_select);
      "default", default
    ]
  and by_ord =
    CCList.flat_map
      (fun (name,p) ->
         [ name, (fun ~ord -> p ~strict:true ~ord);
           name ^ "NS", (fun ~ord -> p ~strict:false ~ord);
         ])
      [ "MaxGoal", max_goal;
        "MaxGoalExceptRRHorn", except_RR_horn max_goal;
      ]
  in
  basics @ by_ord

let from_string ~ord s =
  try (List.assoc s l) ~ord
  with Not_found ->
    failwith ("no such selection function: "^s)

let all () = List.map fst l

let ho_restriction_opt =
  let set_ n = _ho_restriction := n in
  let l = [
    "none", `None;
    "no-var-heading-max-term", `NoVarHeadingMaxTerm;
    "no-var-different-args", `NoVarDifferentArgs;
    "no-unapplied-var-occurring-applied", `NoUnappliedVarOccurringApplied;
    "no-ho-vars", `NoHigherOrderVariables] in
  Arg.Symbol (List.map fst l, fun s -> set_ (List.assoc s l))

let () =
  let set_select s = Params.select := s in
  Params.add_opts
    [ "--select", Arg.Symbol (all(), set_select), " set literal selection function";
      "--ho-selection-restriction", ho_restriction_opt, " selection restrictions for lambda-free higher-order terms (none/no-var-heading-max-term/no-var-different-args/no-unapplied-var-occurring-applied/no-ho-vars)"
    ]
