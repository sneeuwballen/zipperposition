
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

let _select_ho_var_lits = ref true

(* is it a good idea to select this kind of literal? *)
let can_select_lit (lit:Lit.t) : bool =
  Lit.is_neg lit
  && (
    !_select_ho_var_lits ||
    (* If this flag is false, we cannot select literals containing a HO variable: *)
    not (
      Lit.fold_terms ~vars:true ~ty_args:false ~which:`All ~subterms:true lit
      |> Sequence.exists (fun (t,_) -> T.is_ho_var t)
    )
  )

(* checks that [bv] is an acceptable selection for [lits]. In case
   some literal is selected, at least one negative literal must be selected. *)
let validate_fun_ lits bv =
  if BV.is_empty bv then true
  else (
    Sequence.of_array_i lits
    |> Sequence.exists
      (fun (i,_) -> can_select_lit lits.(i) && BV.get bv i)
  )

(* build a selection function in general, given the more specialized
   one there *)
let mk_ ~(f:Lits.t -> BV.t) (lits:Lits.t) : BV.t =
  if Array.length lits <= 1 then BV.empty ()
  else (
    (* should we select anything? *)
    let should_select = CCArray.exists can_select_lit lits in
    if should_select then (
      let bv = f lits in
      assert (validate_fun_ lits bv);
      bv
    ) else (
      (*Util.debugf ~section 5 "(@[should-not-select@ %a@])" (fun k->k Lits.pp lits);*)
      BV.empty ()
    )
  )

let bv_first_ bv = BV.iter_true bv |> Sequence.head

let max_goal ~strict ~ord lits =
  mk_ lits ~f:(fun lits ->
    let bv = Lits.maxlits ~ord lits in
    (* only retain negative normal lits, or constraints
       that are unshielded *)
    BV.filter bv (fun i -> can_select_lit lits.(i));
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

let default ~ord = max_goal ~strict:true ~ord

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

let () =
  let set_select s = Params.select := s in
  Params.add_opts
    [ "--select",
      Arg.Symbol (all(), set_select),
      " set literal selection function";
      "--dont-select-ho-var-lits",
      Arg.Clear _select_ho_var_lits,
      " prohibit to select only literals containing higher-order variables"
    ]
