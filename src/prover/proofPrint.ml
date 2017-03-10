
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Logtk
open ProofStep

module C = SClause

(* proof hashtable *)
module Tbl = PTbl

let section = ProofStep.section

type t = ProofStep.of_

let has_absurd_lits p = match p.result with
  | Clause c -> Literals.is_absurd (C.lits c)
  | _ -> false

let is_proof_of_false p =
  match p.result with
    | Form f when TypedSTerm.equal f TypedSTerm.Form.false_ -> true
    | Clause c ->
      Literals.is_absurd (C.lits c) && Trail.is_empty (C.trail c)
    | _ -> false

let get_name ~namespace p =
  try
    Tbl.find namespace p
  with Not_found ->
    let n = Tbl.length namespace in
    Tbl.add namespace p n;
    n

(** {2 Conversion to a graph of proofs} *)

(** Get a graph of the proof *)
let as_graph =
  CCGraph.make
    (fun p ->
       match rule p.step with
         | None -> Sequence.empty
         | Some rule ->
           let parents = Sequence.of_list p.step.parents in
           Sequence.map (fun p' -> rule, p') parents)

(** {2 IO} *)

let pp_result out = function
  | Form f -> TypedSTerm.pp out f
  | Clause c ->
    Format.fprintf out "%a/%d" C.pp c (C.id c)
  | BoolClause lits -> BBox.pp_bclause out lits

let pp_result_of out proof = pp_result out proof.result

let pp_notrec out p =
  Format.fprintf out "@[%a by %a@]"
    pp_result_of p pp_kind p.step.kind

let traverse ?(traversed=Tbl.create 16) proof k =
  let current, next = ref [proof], ref [] in
  while !current <> [] do
    (* exhaust the current layer of proofs to explore *)
    while !current <> [] do
      let proof = List.hd !current in
      current := List.tl !current;
      if Tbl.mem traversed proof then ()
      else (
        Tbl.add traversed proof ();
        (* traverse premises first *)
        List.iter (fun proof' -> next := proof' :: !next) proof.step.parents;
        (* yield proof *)
        k proof;
      )
    done;
    (* explore next layer *)
    current := !next;
    next := [];
  done

let pp_normal_step out step = match step.kind with
  | Assert _
  | Goal _ ->
    Format.fprintf out "@[<hv2>%a@]@," pp_kind step.kind
  | Data _ ->
    Format.fprintf out "@[<hv2>data %a@]@," pp_kind step.kind
  | Lemma -> Format.fprintf out "lemma"
  | Trivial -> Format.fprintf out "trivial"
  | Inference _
  | Simplification _
  | Esa _ ->
    Format.fprintf out "@[<hv2>%a@ with @[<hv>%a@]@]@,"
      pp_kind step.kind
      (Util.pp_list pp_result)
      (List.map (fun p->p.result) step.parents)

let pp_normal out proof =
  let sep = "by" in
  Format.fprintf out "@[<v>";
  let pp_bullet out = Format.fprintf out "@<1>@{<Green>*@}" in
  traverse proof
    (fun p ->
       Format.fprintf out "@[<hv2>%t @[%a@] %s@ %a@]@,"
         pp_bullet pp_result p.result sep pp_normal_step p.step
    );
  Format.fprintf out "@]"

let _pp_parent out = function
  | `Name i -> Format.fprintf out "%d" i
  | `Theory s -> Format.fprintf out "theory(%s)" s

let pp_kind_tstp out (k,parents) =
  let pp_parents = Util.pp_list _pp_parent in
  let pp_step status out (rule,parents) =
    match parents with
      | [] ->
        Format.fprintf out "inference(%a, [status(%s)])" (pp_rule ~info:false) rule status
      | _::_ ->
        Format.fprintf out "inference(%a, [status(%s)], [%a])"
          (pp_rule ~info:false) rule status pp_parents parents
  in
  match k with
    | Assert src
    | Goal src -> ProofStep.pp_src_tstp out src
    | Data _ -> assert false
    | Inference rule
    | Simplification rule -> pp_step "thm" out (rule,parents)
    | Esa rule -> pp_step "esa" out (rule,parents)
    | Lemma -> Format.fprintf out "lemma"
    | Trivial -> assert(parents=[]); Format.fprintf out "trivial([status(thm)])"

let pp_tstp out proof =
  let namespace = Tbl.create 5 in
  Format.fprintf out "@[<v>";
  traverse proof
    (fun p ->
       let name = get_name ~namespace p in
       let parents =
         List.map (fun p -> `Name (get_name ~namespace p)) p.step.parents
       in
       let role = "plain" in (* TODO *)
       match p.result with
         | Form f ->
           Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
             name role TypedSTerm.TPTP.pp f pp_kind_tstp (p.step.kind,parents)
         | BoolClause c ->
           let tr = Trail.of_list c in
           Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
             name role SClause.pp_trail_tstp tr pp_kind_tstp (p.step.kind,parents)
         | Clause c ->
           Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
             name role C.pp_tstp c pp_kind_tstp (p.step.kind,parents)
    );
  Format.fprintf out "@]";
  ()

(** Prints the proof according to the given input switch *)
let pp o out proof = match o with
  | Options.Print_none -> Util.debug ~section 1 "proof printing disabled"
  | Options.Print_tptp -> pp_tstp out proof
  | Options.Print_normal -> pp_normal out proof
  | Options.Print_zf -> failwith "proof printing in ZF not implemented" (* TODO? *)

let _pp_list_str = Util.pp_list CCFormat.string

let _escape_dot s =
  let b = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
       begin match c with
         | '|' | '\\' | '{' | '}' | '<' | '>' | '"' -> Buffer.add_char b '\\';
         | _ -> ()
       end;
       Buffer.add_char b c)
    s;
  Buffer.contents b

let _to_str_escape fmt =
  Util.ksprintf_noc ~f:_escape_dot fmt

let pp_dot_seq ~name out seq =
  (* TODO: check proof is a DAG *)
  let equal = ProofStep.equal_proof in
  let hash = ProofStep.hash_proof in
  CCGraph.Dot.pp_seq
    ~tbl:(CCGraph.mk_table ~eq:equal ~hash:hash 64)
    ~eq:equal
    ~name
    ~graph:as_graph
    ~attrs_v:(fun p ->
      let label = _to_str_escape "@[<2>%a@]" pp_result_of p in
      let attrs = [`Label label; `Style "filled"] in
      let shape = `Shape "box" in
      if is_proof_of_false p then [`Color "red"; `Label "[]"; `Shape "box"; `Style "filled"]
      else if has_absurd_lits p then `Color "orange" :: shape :: attrs
      else if is_assert p.step then `Color "yellow" :: shape :: attrs
      else if is_goal p.step then `Color "green" :: shape :: attrs
      else if is_trivial p.step then `Color "cyan" :: shape :: attrs
      else shape :: attrs
    )
    ~attrs_e:(fun r ->
      [`Label r.rule_name; `Other ("dir", "back")])
    out
    seq;
  Format.pp_print_newline out ();
  ()

let pp_dot ~name out proof = pp_dot_seq ~name out (Sequence.singleton proof)

let pp_dot_seq_file ?(name="proof") filename seq =
  (* print graph on file *)
  Util.debugf ~section 1 "print proof graph to@ `%s`" (fun k->k filename);
  CCIO.with_out filename
    (fun oc ->
       let out = Format.formatter_of_out_channel oc in
       Format.fprintf out "%a@." (pp_dot_seq ~name) seq)

let pp_dot_file ?name filename proof =
  pp_dot_seq_file ?name filename (Sequence.singleton proof)
