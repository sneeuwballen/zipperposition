
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = Term
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV

let section = Util.Section.make "select"

type t = Literal.t array -> BV.t

module Classify : sig
  type shielded = [`Shielded | `Unshielded]
  type sign = bool

  type constraint_ =
    | C_arith (* arith negative equality, seen as constraint *)
    | C_ho (* F args != t *)
    | C_purify (* X != t, X of purifiable type *)

  type class_ =
    | K_normal of sign (* any "normal" kind of literal *)
    | K_constr of constraint_ * shielded (* constraint *)

  val is_unshielded_constr : class_ -> bool
  (** Constraint on unshielded variable? *)

  type t = class_ array

  val pp_class : class_ CCFormat.printer
  val pp : t CCFormat.printer

  val classify : Literals.t -> t
  (** [classify lits] gives a classification of the literals in this array,
      relative to other literals *)
end = struct
  type shielded = [`Shielded | `Unshielded]
  type sign = bool

  type constraint_ =
    | C_arith (* arith negative equality, seen as constraint *)
    | C_ho (* F args != t *)
    | C_purify (* X != t, X of purifiable type *)

  type class_ =
    | K_normal of sign (* any "normal" kind of literal *)
    | K_constr of constraint_ * shielded (* constraint *)

  type t = class_ array

  (* combine the "shielded" status *)
  let (+++) a b = match a, b with
    | `Unshielded, _
    | _, `Unshielded -> `Unshielded
    | `Shielded, `Shielded -> `Shielded

  let is_unshielded_constr = function
    | K_constr (_, `Unshielded) -> true
    | K_normal _ | K_constr _ -> false

  let pp_shield out = function
    | `Shielded -> CCFormat.string out "shielded"
    | `Unshielded -> CCFormat.string out "unshielded"

  let pp_constr out = function
    | C_arith -> CCFormat.string out "arith_constr"
    | C_ho -> CCFormat.string out "ho_constr"
    | C_purify -> CCFormat.string out "purify_constr"

  let pp_class out = function
    | K_normal s -> Format.fprintf out "normal :sign %B" s
    | K_constr (c,s) -> Format.fprintf out "%a %a" pp_constr c pp_shield s

  let pp out a =
    let pp_i out (i,cl) = Format.fprintf out "(%d: %a)" i pp_class cl in
    Format.fprintf out "(@[<hv>%a@])"
      (Util.pp_seq ~sep:" " pp_i) (Sequence.of_array_i a)

  let classify (lits:Literals.t): t =
    let shield_status v =
      if Purify.is_shielded v lits then `Shielded else `Unshielded
    in
    lits |> Array.map
      (fun lit -> match lit with
         | Literal.Equation (t, u, false) ->
           let hd_t, args_t = T.as_app t in
           let hd_u, args_u = T.as_app u in
           begin match T.view hd_t, T.view hd_u with
             | T.Var v1, T.Var v2 when args_t <> [] && args_u <> [] ->
               K_constr (C_ho, shield_status v1 +++ shield_status v2)
             | T.Var v, _ when args_t <> [] ->
               (* HO unif constraint *)
               K_constr (C_ho, shield_status v)
             | _, T.Var v when args_u <> [] ->
               K_constr (C_ho, shield_status v)
             | T.Var v, _ when Type.is_fun (T.ty t) ->
               K_constr (C_purify, shield_status v)
             | _, T.Var v when Type.is_fun (T.ty u) ->
               K_constr (C_purify, shield_status v)
             | _ -> K_normal (Lit.sign lit)
           end
         | Literal.Prop (t, sign) ->
           let hd_t, args_t = T.as_app t in
           begin match T.view hd_t, args_t with
             | T.Var v, [] -> K_constr (C_purify, shield_status v)
             | T.Var v, _::_ -> K_constr (C_ho, shield_status v)
             | _ -> K_normal sign
           end
         | Literal.Int (Int_lit.Binary (Int_lit.Different, m1, m2)) ->
           let vars =
             Sequence.append (Monome.Seq.terms m1) (Monome.Seq.terms m2)
             |> Sequence.filter_map T.as_var
             |> Sequence.to_rev_list
           in
           begin match vars with
             | [] -> K_normal (Lit.sign lit) (* no vars *)
             | _::_ ->
               let st =
                 List.fold_left (fun acc v -> acc +++ shield_status v) `Shielded vars
               in
               K_constr (C_arith, st)
           end
         | _ -> K_normal (Lit.sign lit))
end

type parametrized = strict:bool -> ord:Ordering.t -> t

(* no need for classification here *)
let no_select _ = BV.empty ()

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
let mk_ ~(f: Classify.t -> Lits.t -> BV.t) (lits:Lits.t) : BV.t =
  if Array.length lits <= 1 then BV.empty ()
  else (
    let cl = Classify.classify lits in
    (* should we select anything? *)
    let should_select = CCArray.exists can_select_cl_ cl in
    if should_select then (
      (* select a literal (first try an unshielded constr, else call [f]) *)
      let bv = match find_max_constr_ cl with
        | Some bv ->  bv
        | None -> f cl lits
      in
      Util.debugf ~section 5
        "(@[select@ :lits %a@ :res %a@ :classify %a@])"
        (fun k->k Lits.pp lits BV.print bv Classify.pp cl);
      assert (validate_fun_ cl lits bv);
      bv
    ) else (
      Util.debugf ~section 5 "(@[should-not-select@ %a@])" (fun k->k Lits.pp lits);
      BV.empty ()
    )
  )

let bv_first_ bv = BV.iter_true bv |> Sequence.head

let max_goal ~strict ~ord lits =
  mk_ lits ~f:(fun cl lits ->
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

let except_RR_horn (p:parametrized) ~strict ~ord lits =
  if Lits.is_RR_horn_clause lits
  then BV.empty () (* do not select (conditional rewrite rule) *)
  else p ~strict ~ord lits  (* delegate *)

(** {2 Global selection Functions} *)

let default ~ord = max_goal ~strict:true ~ord

let l =
  let basics =
    [ "NoSelection", (fun ~ord:_ c -> no_select c);
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
