
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat

type term = TypedSTerm.t
type ty = term
type form = TypedSTerm.Form.t
type subst = (term, ty) Var.Subst.t

type name = string

type t = {
  id: int; (* unique ID *)
  concl: form;
  step: step;
}
and step =
  | Goal
  | Assert
  | Negated_goal of t
  | Instantiate of subst * t
  | Esa of name * t list
  | Inference of name * t list
  | No_check of name * t list

let pp_step out (s:step): unit = match s with
  | Goal -> Fmt.string out "goal"
  | Assert -> Fmt.string out "assert"
  | Negated_goal _ -> Fmt.string out "negated_goal"
  | Instantiate (subst, _) ->
    Fmt.fprintf out "(@[instantiate %a@])" (Var.Subst.pp T.pp) subst
  | Esa (n,_) -> Fmt.fprintf out "(esa %s)" n
  | Inference (n,_) -> Fmt.fprintf out "(inf %s)" n
  | No_check (n,_) -> Fmt.fprintf out "(no_check %s)" n

let premises (p:t): t list = match p.step with
  | Goal | Assert -> []
  | Negated_goal p2
  | Instantiate (_,p2) -> [p2]
  | Esa (_,l)
  | Inference (_,l)
  | No_check (_,l) -> l

let equal a b = a.id = b.id
let compare a b = CCInt.compare a.id b.id
let hash a = Hash.int a.id

module Tbl = CCHashtbl.Make(struct
    type t_ = t
    type t = t_
    let hash = hash
    let equal = equal
  end)

let pp_id out (p:t): unit = Fmt.int out p.id

let pp out (p:t): unit =
  Fmt.fprintf out "(@[<2>%a@ :from [@[%a@]]@])@,"
    pp_step p.step (Util.pp_list pp_id) (premises p)

let pp_dag out (p:t): unit =
  let seen = Tbl.create 32 in
  let rec pp out (p:t) =
    if not @@ Tbl.mem seen p then (
      Tbl.add seen p ();
      pp out p;
      List.iter (pp out) (premises p);
    )
  in
  Fmt.fprintf out "(@[<hv2>proof@ %a@])" pp p

let mk_ : form -> step -> t =
  let n = ref 0 in
  fun concl step ->
    { id=CCRef.incr_then_get n; concl; step }

let goal f = mk_ f Goal
let negated_goal f p = mk_ f (Negated_goal p)
let assert_ f = mk_ f Assert
let instantiate f subst p = mk_ f (Instantiate (subst,p))
let esa f name ps = mk_ f (Esa (name,ps))
let inference f name ps = mk_ f (Inference (name,ps))
let no_check f name ps = mk_ f (No_check (name,ps))

