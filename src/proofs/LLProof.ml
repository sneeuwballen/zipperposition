
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs} *)

open Logtk

module T = TypedSTerm
module F = T.Form
module Fmt = CCFormat

let section = Util.Section.make "llproof"

type term = T.t
type ty = term
type form = term
type inst = term list (** Instantiate some binder with the following terms. Order matters. *)

type tag = Proof.tag

type name = string

type check_res =
  | R_ok
  | R_fail
  | R_skip

type t = {
  id: int; (* unique ID *)
  concl: form;
  step: step;
  mutable checked: check_res option; (* caching result of checking *)
}
and step =
  | Goal
  | Assert
  | Negated_goal of t
  | Trivial
  | Define of Proof.def
  | Instantiate of {
      form: t;
      inst: inst;
      tags: tag list;
    }
  | Inference of {
      intros: term list option; (* local renaming, with fresh constants *)
      name: name;
      parents: parent list;
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

let pp_tags = Proof.pp_tags
let pp_inst out (l:inst) : unit =
  Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:"," T.pp) l

let pp_step out (s:step): unit = match s with
  | Goal -> Fmt.string out "goal"
  | Assert -> Fmt.string out "assert"
  | Negated_goal _ -> Fmt.string out "negated_goal"
  | Trivial -> Fmt.string out "trivial"
  | Define id -> Fmt.fprintf out "(@[define@ %a@])" Proof.pp_def id
  | Instantiate {inst;tags;_} ->
    Fmt.fprintf out "(@[instantiate %a%a@])" pp_inst inst pp_tags tags
  | Inference {name=n;tags;_} -> Fmt.fprintf out "(inf %s%a)" n pp_tags tags

let parents (p:t): parent list = match p.step with
  | Goal | Assert | Trivial | Define _ -> []
  | Negated_goal p2 -> [p_of p2]
  | Instantiate {form=p2;_} -> [p_of p2]
  | Inference {parents=l;_} -> l

let premises (p:t): t list =
  let open_p {p_proof;_} = p_proof in
  List.rev_map open_p @@ parents p

let inst (p:t): inst = match p.step with
  | Instantiate {inst;_} -> inst
  | _ -> []

let tags (p:t) : tag list = match p.step with
  | Inference {tags;_} | Instantiate {tags;_} -> tags
  | _ -> []

let intros (p:t) : inst option = match p.step with
  | Inference {intros;_} -> intros
  | _ -> None

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
let pp_res out (p:t) = T.pp out (concl p)

let pp_parent out p = match p.p_inst with
  | [] -> pp_res out p.p_proof
  | _::_ ->
    Format.fprintf out "@[(@[%a@])@,%a@]" pp_res p.p_proof pp_inst p.p_inst

let pp_inst_some out = function [] -> () | l -> Fmt.fprintf out "@ :inst %a" pp_inst l
let pp_intro_some out = function None -> () | l -> Fmt.fprintf out "@ :intro %a" (CCOpt.pp pp_inst) l

let pp out (p:t): unit =
  Fmt.fprintf out "(@[<hv2>proof/%d %a%a@ :res `%a`@ :from [@[<hv>%a@]]%a%a@])"
    p.id pp_step (step p) Proof.pp_tags (tags p)
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
    { id=CCRef.incr_then_get n; concl; step; checked=None; }

let goal f = mk_ f Goal
let negated_goal f p = mk_ f (Negated_goal p)
let assert_ f = mk_ f Assert
let trivial f = mk_ f Trivial
let define def f = mk_ f (Define def)

let instantiate ?(tags=[]) f p inst =
  mk_ f (Instantiate {form=p;inst;tags})

let inference ~intros ~tags f name ps : t =
  mk_ f (Inference
      {name;intros;parents=ps;tags})

let get_check_res t = t.checked
let set_check_res t r = t.checked <- Some r

let pp_check_res out = function
  | R_ok -> Fmt.string out "ok"
  | R_fail -> Fmt.string out "fail"
  | R_skip -> Fmt.string out "skip"

module Dot = struct
  (** Get a graph of the proof *)
  let as_graph ~display_trivial : (t, string * inst) CCGraph.t =
    CCGraph.make
      (fun p ->
         let descr = match step p with
           | Goal -> "goal"
           | Assert -> "assert"
           | Negated_goal _ -> "negated_goal"
           | Trivial -> "trivial"
           | Define id -> Fmt.sprintf "define(%a)" Proof.pp_def id
           | Instantiate _ -> "instantiate"
           | Inference {name;_} -> name
         in
         let descr = Fmt.sprintf "@[<h>%s%a@]" descr pp_tags (tags p) in
         begin
           parents p
           |> Iter.of_list
           |> Iter.filter (fun p' -> match step p'.p_proof with 
              | Trivial -> display_trivial 
              | _ -> true)
           |> Iter.map
             (fun p' -> (descr,inst p), p'.p_proof)
         end)

  let _to_str_escape fmt =
    Util.ksprintf_noc ~f:Util.escape_dot fmt

  let color p : string option =
    let rec is_bool_atom t = match T.view t with
      | T.AppBuiltin (Builtin.Box_opaque,_) -> true
      | T.AppBuiltin (Builtin.Not, [t]) -> is_bool_atom t
      | _ -> false
    in
    begin match step p, F.view (concl p) with
      | _, F.False -> Some "red"
      | _ when is_bool_atom (concl p) -> Some "cyan"
      | _, F.Or l when List.for_all is_bool_atom l -> Some "cyan"
      | Goal, _ -> Some "green"
      | Assert, _ -> Some "yellow"
      | Trivial, _ -> Some "gold"
      | Define _, _ -> Some "navajowhite"
      | _ -> Some "grey"
    end

  let pp_dot_seq ~name ~display_trivial out seq =
    CCGraph.Dot.pp_seq
      ~tbl:(CCGraph.mk_table ~eq:equal ~hash:hash 64)
      ~eq:equal
      ~name
      ~graph:(as_graph ~display_trivial)
      ~attrs_v:(fun p ->
        let top, b_color = match get_check_res p with
          | None -> "[no-check]", []
          | Some R_ok -> "[check ✔]", [`Color "green"; `Other ("penwidth", "6")]
          | Some R_fail -> "[check ×]", [`Color "red"; `Other ("penwidth", "8")]
          | Some R_skip -> "[check ø]", [`Color "yellow"]
        in
        let label = _to_str_escape "@[<v>%s@,@[<2>%a@]@]@." top T.pp (concl p) in
        let attrs = [`Label label; `Style "filled"] in
        let shape = `Shape "box" in
        let color = match color p with None -> [] | Some c -> [`Other ("fillcolor", c)] in
        shape :: color @ b_color @ attrs
      )
      ~attrs_e:(fun (r,inst) ->
        let label = _to_str_escape "@[<v>%s%a@]@." r pp_inst_some inst in
        [`Label label; `Other ("dir", "back")])
      out
      seq;
    Format.pp_print_newline out ();
    ()

  let pp_dot ~name ~display_trivial out proof = pp_dot_seq ~name ~display_trivial out (Iter.singleton proof)

  let pp_dot_seq_file ?(name="llproof") ~display_trivial filename seq =
    (* print graph on file *)
    Util.debugf ~section 1 "print LLProof graph to@ `%s`" (fun k->k filename);
    CCIO.with_out filename
      (fun oc ->
         let out = Format.formatter_of_out_channel oc in
         Format.fprintf out "%a@." (pp_dot_seq ~name ~display_trivial) seq)

  let pp_dot_file ?name ~display_trivial filename proof =
    pp_dot_seq_file ?name ~display_trivial filename (Iter.singleton proof)
end
