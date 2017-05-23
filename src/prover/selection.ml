
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = Term
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV
module Classify = Classify_literals

type t = Classify_literals.t -> Literal.t array -> CCBV.t

type parametrized = strict:bool -> ord:Ordering.t -> t

(* no need for classification here *)
let no_select _ _ : BV.t = BV.empty ()

(* is it a good idea to select this kind of literal? *)
let can_select_cl_ (k:Classify.class_): bool = match k with
  | Classify.K_normal false
  | Classify.K_constr (_, `Unshielded) -> true
  | _ -> false

(* checks that [bv] is an acceptable selection for [lits]. In case
   some literal is selected, at least one negative literal must be selected. *)
let validate_fun_ cl lits bv =
  if BV.is_empty bv then true
  else (
    Sequence.of_array_i lits
    |> Sequence.exists
      (fun (i,_) -> can_select_cl_ cl.(i) && BV.get bv i)
  )

(* select one unshielded {HO,purify} constraint, if any *)
let find_max_constr_ cl =
  cl |> CCArray.findi
    (fun i k -> match k with
       | Classify.K_constr
           ((Classify.C_ho | Classify.C_purify), `Unshielded) ->
         let bv = BV.create ~size:(Array.length cl) false in
         BV.set bv i;
         Some bv
       | _ -> None)

(* build a selection function in general, given the more specialized
   one there *)
let mk_ ~(f:Lits.t -> BV.t)(cl:Classify.t) (lits:Lits.t) : BV.t =
  if Array.length lits <= 1 then BV.empty ()
  else (
    (* should we select anything? *)
    let should_select = CCArray.exists can_select_cl_ cl in
    if should_select then (
      (* select a literal (first try an unshielded constr, else call [f]) *)
      let bv = match find_max_constr_ cl with
        | Some bv ->  bv
        | None -> f lits
      in
      (*Util.debugf ~section 5
        "(@[select@ :lits %a@ :res %a@ :classify %a@])"
        (fun k->k Lits.pp lits BV.print bv Classify.pp cl);*)
      assert (validate_fun_ cl lits bv);
      bv
    ) else (
      (*Util.debugf ~section 5 "(@[should-not-select@ %a@])" (fun k->k Lits.pp lits);*)
      BV.empty ()
    )
  )

let bv_first_ bv = BV.iter_true bv |> Sequence.head

let max_goal ~strict ~ord cl lits =
  mk_ cl lits ~f:(fun lits ->
    let bv = Lits.maxlits ~ord lits in
    (* only retain negative normal lits, or constraints
       that are unshielded *)
    BV.filter bv (fun i -> can_select_cl_ cl.(i));
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

let except_RR_horn (p:parametrized) ~strict ~ord cl lits =
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else p ~strict ~ord cl lits  (* delegate *)

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
      " set literal selection function"
    ]
