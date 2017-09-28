
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat

let section = Util.Section.make "llproof"

type term = TypedSTerm.t
type ty = term
type form = TypedSTerm.Form.t
type subst = (term, ty) Var.Subst.t

type name = string

type check_info =
  | C_check of form list (* additional inputs *)
  | C_no_check
  | C_other

type t = {
  id: int; (* unique ID *)
  concl: form;
  step: step;
}
and step =
  | Goal
  | Assert
  | Negated_goal of t
  | Trivial
  | By_def of ID.t
  | Define of ID.t
  | Instantiate of subst * t
  | Esa of name * t list * check_info
  | Inference of name * parent list * check_info

and parent =
  | P_of of t
  | P_instantiate of t * subst

let concl p = p.concl
let step p = p.step
let id p = p.id

let p_of p = P_of p
let p_instantiate p subst = P_instantiate (p,subst)

let pp_subst out (s:subst) : unit =
  Format.fprintf out "{@[<hv>%a@]}" (Var.Subst.pp T.pp) s

let pp_step out (s:step): unit = match s with
  | Goal -> Fmt.string out "goal"
  | Assert -> Fmt.string out "assert"
  | Negated_goal _ -> Fmt.string out "negated_goal"
  | Trivial -> Fmt.string out "trivial"
  | By_def id -> Fmt.fprintf out "(by_def :of %a)" ID.pp id
  | Define id -> Fmt.fprintf out "(@[define@ %a@])" ID.pp id
  | Instantiate (subst, _) ->
    Fmt.fprintf out "(@[instantiate %a@])" pp_subst subst
  | Esa (n,_,_) -> Fmt.fprintf out "(esa %s)" n
  | Inference (n,_,_) -> Fmt.fprintf out "(inf %s)" n

let parents (p:t): parent list = match p.step with
  | Goal | Assert | Trivial | By_def _ | Define _ -> []
  | Negated_goal p2 -> [p_of p2]
  | Instantiate (subst,p2) -> [p_instantiate p2 subst]
  | Esa (_,l,_) -> List.map p_of l
  | Inference (_,l,_) -> l

let premises (p:t): t list =
  let open_p = function
    | P_of x -> x
    | P_instantiate (p,_) -> p
  in
  List.rev_map open_p @@ parents p

let check_info (p:t): check_info = match p.step with
  | Goal | Assert | Trivial | Negated_goal _ | By_def _ | Define _ -> C_other
  | Instantiate (_,_) -> C_check []
  | Esa (_,_,c)
  | Inference (_,_,c) -> c

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
let pp_res out (p:t) = TypedSTerm.pp out (concl p)

let pp_parent out = function
  | P_of p -> pp_res out p
  | P_instantiate (p,subst) ->
    Format.fprintf out "@[(@[%a@])@,%a@]" pp_res p pp_subst subst

let pp out (p:t): unit =
  Fmt.fprintf out "(@[<hv2>%a@ :res `%a`@ :from [@[%a@]]@])"
    pp_step (step p)
    pp_res p
    (Util.pp_list pp_parent) (parents p)

let pp_dag out (p:t): unit =
  let seen = Tbl.create 32 in
  let rec pp out (p:t) =
    if not @@ Tbl.mem seen p then (
      Tbl.add seen p ();
      pp out p;
      Fmt.fprintf out "@,";
      List.iter (pp_parent out) (parents p);
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
let trivial f = mk_ f Trivial
let by_def id f = mk_ f (By_def id)
let define id f = mk_ f (Define id)
let instantiate f subst p = mk_ f (Instantiate (subst,p))

let conv_check_ = function
  | `No_check -> C_no_check
  | `Check -> C_check []
  | `Check_with l -> C_check l

let esa c f name ps = mk_ f (Esa (name,ps,conv_check_ c))
let inference c f name ps = mk_ f (Inference (name,ps,conv_check_ c))

