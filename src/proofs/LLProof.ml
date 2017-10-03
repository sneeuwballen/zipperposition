
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

open Logtk

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat

let section = Util.Section.make "llproof"

type term = TypedSTerm.t
type ty = term
type form = TypedSTerm.Form.t
type var = ty Var.t
type inst = term list (** Instantiate some binder with the following terms. Order matters. *)
type tag = Proof.tag

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
  | Instantiate of t * term list
  | Esa of name * t list * check_info
  | Inference of {
      intros: term list; (* local renaming, with fresh constants *)
      name: name;
      parents: parent list;
      check: check_info;
      tags: tag list;
    }

and parent = {
  p_proof: t;
  p_inst: inst; (* instantiate [forall] variables *)
}

let concl p = p.concl
let step p = p.step
let id p = p.id

let p_inst p inst = {p_proof=p; p_inst=inst}
let p_of p = p_inst p []

let pp_inst out (l:inst) : unit =
  Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:"," T.pp) l

let pp_step out (s:step): unit = match s with
  | Goal -> Fmt.string out "goal"
  | Assert -> Fmt.string out "assert"
  | Negated_goal _ -> Fmt.string out "negated_goal"
  | Trivial -> Fmt.string out "trivial"
  | By_def id -> Fmt.fprintf out "(by_def :of %a)" ID.pp id
  | Define id -> Fmt.fprintf out "(@[define@ %a@])" ID.pp id
  | Instantiate (_,inst) ->
    Fmt.fprintf out "(@[instantiate %a@])" pp_inst inst
  | Esa (n,_,_) -> Fmt.fprintf out "(esa %s)" n
  | Inference {name=n;_} -> Fmt.fprintf out "(inf %s)" n

let parents (p:t): parent list = match p.step with
  | Goal | Assert | Trivial | By_def _ | Define _ -> []
  | Negated_goal p2 -> [p_of p2]
  | Instantiate (p2,_) -> [p_of p2]
  | Esa (_,l,_) -> List.map p_of l
  | Inference {parents=l;_} -> l

let premises (p:t): t list =
  let open_p {p_proof;_} = p_proof in
  List.rev_map open_p @@ parents p

let inst (p:t): inst = match p.step with
  | Instantiate (_,inst) -> inst
  | _ -> []

let check_info (p:t): check_info = match p.step with
  | Goal | Assert | Trivial | Negated_goal _ | By_def _ | Define _ -> C_other
  | Instantiate (_,_) -> C_check []
  | Esa (_,_,c)
  | Inference {check=c;_} -> c

let tags (p:t) : tag list = match p.step with
  | Inference {tags;_} -> tags
  | _ -> []

let intros (p:t) : inst = match p.step with
  | Inference {intros;_} -> intros
  | _ -> []

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

let pp_parent out p = match p.p_inst with
  | [] -> pp_res out p.p_proof
  | _::_ ->
    Format.fprintf out "@[(@[%a@])@,%a@]" pp_res p.p_proof pp_inst p.p_inst

let pp_inst_some out = function [] -> () | l -> Fmt.fprintf out "@ :inst %a" pp_inst l
let pp_intro_some out = function [] -> () | l -> Fmt.fprintf out "@ :intro %a" pp_inst l

let pp out (p:t): unit =
  Fmt.fprintf out "(@[<hv2>%a%a@ :res `%a`@ :from [@[%a@]]%a%a@])"
    pp_step (step p) Proof.pp_tags (tags p)
    pp_res p
    (Util.pp_list pp_parent) (parents p)
    pp_inst_some (inst p)
    pp_intro_some (intros p)

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
let instantiate f p inst = mk_ f (Instantiate (p,inst))

let conv_check_ = function
  | `No_check -> C_no_check
  | `Check -> C_check []
  | `Check_with l -> C_check l

let esa c f name ps = mk_ f (Esa (name,ps,conv_check_ c))
let inference c ~intros ~tags f name ps : t =
  mk_ f (Inference {name;intros;parents=ps;check=conv_check_ c;tags})
