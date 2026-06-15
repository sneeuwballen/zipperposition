(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Low Level Proofs — Clause-Based} *)

open Logtk
module Fmt = CCFormat

let section = Util.Section.make "llproof"

type clause = Literal.t array
(** A clause: disjunction of literals. Free variables are implicitly universal.
*)

type inst = (Type.t HVar.t * Term.t) list
(** Instantiation: pairs of (variable, replacement term). Identity pairs (v → v)
    should be omitted. *)

type tag = Proof.tag
type name = string

type check_res =
  | R_ok
  | R_fail
  | R_skip

type t = {
  id: int;
  concl: clause;
  step: step;
  mutable checked: check_res option;
}

and step =
  | Goal
  | Assert
  | Trivial
  | By_def of Name.t
  | Define of Name.t
  | Esa of name * t list
  | Inference of {
      name: name;
      tags: tag list;
      parents: parent list;
    }

and parent = {
  p_proof: t;
  p_inst: inst;
}

let concl p = p.concl
let step p = p.step
let id p = p.id
let p_inst p inst = { p_proof = p; p_inst = inst }
let p_of p = p_inst p []
let pp_tags = Proof.pp_tags

let pp_inst out (l : inst) : unit =
  Format.fprintf out "[@[<hv>%a@]]"
    (Util.pp_list ~sep:",@ " (fun out (v, t) ->
         Format.fprintf out "%a → %a" HVar.pp v Term.pp t))
    l

let pp_step out (s : step) : unit =
  match s with
  | Goal -> Fmt.string out "goal"
  | Assert -> Fmt.string out "assert"
  | Trivial -> Fmt.string out "trivial"
  | By_def id -> Fmt.fprintf out "(by_def :of %a)" Name.pp id
  | Define id -> Fmt.fprintf out "(@[define@ %a@])" Name.pp id
  | Esa (n, _) -> Fmt.fprintf out "(esa %s)" n
  | Inference { name = n; tags; _ } ->
    Fmt.fprintf out "(inf %s%a)" n pp_tags tags

let parents (p : t) : parent list =
  match p.step with
  | Goal | Assert | Trivial | By_def _ | Define _ -> []
  | Esa (_, l) -> List.map p_of l
  | Inference { parents = l; _ } -> l

let premises (p : t) : t list =
  let open_p { p_proof; _ } = p_proof in
  List.rev_map open_p @@ parents p

let tags (p : t) : tag list =
  match p.step with
  | Inference { tags; _ } -> tags
  | _ -> []

let equal a b = a.id = b.id
let compare a b = CCInt.compare a.id b.id
let hash a = Hash.int a.id

module Tbl = CCHashtbl.Make (struct
  type t_ = t
  type t = t_

  let hash = hash
  let equal = equal
end)

let pp_id out (p : t) : unit = Fmt.int out p.id

let pp_clause out (cl : clause) : unit =
  Array.iteri
    (fun i lit ->
      if i > 0 then Fmt.string out " | ";
      Literal.pp out lit)
    cl

let pp_res out (p : t) = pp_clause out (concl p)

let pp_parent out p =
  match p.p_inst with
  | [] -> pp_res out p.p_proof
  | _ :: _ ->
    Format.fprintf out "@[(@[%a@])@,%a@]" pp_res p.p_proof pp_inst p.p_inst

let pp_inst_some out (inst : inst) =
  if inst <> [] then Fmt.fprintf out "@ :inst %a" pp_inst inst

let pp out (p : t) : unit =
  Fmt.fprintf out "(@[<hv2>proof/%d %a%a@ :res @[%a@]@ :from [@[<hv>%a@]]%a@])"
    p.id pp_step (step p) Proof.pp_tags (tags p) pp_clause (concl p)
    (Util.pp_list pp_parent) (parents p) pp_inst_some
    (match p.step with
    | Inference { parents; _ } ->
      List.filter_map
        (fun par ->
          if par.p_inst <> [] then
            Some par.p_inst
          else
            None)
        parents
      |> List.concat
    | _ -> [])

let pp_dag out (p : t) : unit =
  let seen = Tbl.create 32 in
  let rec pp out (p : t) =
    if not @@ Tbl.mem seen p then (
      Tbl.add seen p ();
      pp out p;
      Fmt.fprintf out "@,";
      List.iter (pp_parent out) (parents p)
    )
  in
  Fmt.fprintf out "(@[<hv2>proof@ %a@])" pp p

let mk_ : clause -> step -> t =
  let n = ref 0 in
  fun concl step -> { id = CCRef.incr_then_get n; concl; step; checked = None }

let goal cl = mk_ cl Goal
let assert_ cl = mk_ cl Assert
let trivial cl = mk_ cl Trivial
let by_def id cl = mk_ cl (By_def id)
let define id cl = mk_ cl (Define id)
let esa cl name ps = mk_ cl (Esa (name, ps))

let inference ~tags cl name ps : t =
  mk_ cl (Inference { name; parents = ps; tags })

let get_check_res t = t.checked
let set_check_res t r = t.checked <- Some r

let pp_check_res out = function
  | R_ok -> Fmt.string out "ok"
  | R_fail -> Fmt.string out "fail"
  | R_skip -> Fmt.string out "skip"

module Dot = struct
  let as_graph : (t, string * inst) CCGraph.t =
    CCGraph.make (fun p ->
        let descr =
          match step p with
          | Goal -> "goal"
          | Assert -> "assert"
          | Trivial -> "trivial"
          | By_def id -> Fmt.sprintf "by_def(%a)" Name.pp id
          | Define id -> Fmt.sprintf "define(%a)" Name.pp id
          | Esa (name, _) -> name
          | Inference { name; _ } -> name
        in
        let descr = Fmt.sprintf "@[<h>%s%a@]" descr pp_tags (tags p) in
        parents p |> Iter.of_list
        |> Iter.map (fun p' -> (descr, p'.p_inst), p'.p_proof))

  let _to_str_escape fmt = Util.ksprintf_noc ~f:Util.escape_dot fmt

  let color p : string option =
    match step p with
    | Goal -> Some "green"
    | Assert -> Some "yellow"
    | Trivial -> Some "gold"
    | By_def _ | Define _ -> Some "navajowhite"
    | _ -> Some "grey"

  let pp_dot_seq ~name out seq =
    CCGraph.Dot.pp_all
      ~tbl:(CCGraph.mk_table ~eq:equal ~hash 64)
      ~eq:equal ~name ~graph:as_graph
      ~attrs_v:(fun p ->
        let top, b_color =
          match get_check_res p with
          | None -> "[no-check]", []
          | Some R_ok ->
            "[check ✔]", [ `Color "green"; `Other ("penwidth", "6") ]
          | Some R_fail ->
            "[check ×]", [ `Color "red"; `Other ("penwidth", "8") ]
          | Some R_skip -> "[check ø]", [ `Color "yellow" ]
        in
        let label =
          _to_str_escape "@[<v>%s@,@[<2>%a@]@]@." top pp_clause (concl p)
        in
        let attrs = [ `Label label; `Style "filled" ] in
        let shape = `Shape "box" in
        let color =
          match color p with
          | None -> []
          | Some c -> [ `Other ("fillcolor", c) ]
        in
        (shape :: color) @ b_color @ attrs)
      ~attrs_e:(fun (r, inst) ->
        let label = _to_str_escape "@[<v>%s%a@]@." r pp_inst_some inst in
        [ `Label label; `Other ("dir", "back") ])
      out seq;
    Format.pp_print_newline out ();
    ()

  let pp_dot ~name out proof = pp_dot_seq ~name out (Iter.singleton proof)

  let pp_dot_seq_file ?(name = "llproof") filename seq =
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "llproof.pp-dot" in
    Util.debugf ~section 1 "print LLProof graph to@ `%s`" (fun k -> k filename);
    CCIO.with_out filename (fun oc ->
        let out = Format.formatter_of_out_channel oc in
        Format.fprintf out "%a@." (pp_dot_seq ~name) seq)

  let pp_dot_file ?name filename proof =
    pp_dot_seq_file ?name filename (Iter.singleton proof)
end
