
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Hash = CCHash
module CC = CompactClause

type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make ~parent:Const.section "proof"

(** Classification of proof steps *)
type step_kind =
  | Inference of string
  | Simplification of string
  | Esa of string
  | File of StatementSrc.t
  | Trivial  (* trivial, or trivial within theories *)

type step_result =
  | Form of form
  | Clause of CompactClause.t

type t = {
  result : step_result;       (** conclusion of the step *)
  kind : step_kind;           (** kind of step *)
  parents : t array;          (** parent proof steps *)
  theories : string list;     (** theories used for the proof step *)
  additional_info : string list;   (** additional info, prover-dependent *)
}

type proof = t

let result p = p.result
let kind p = p.kind
let parents p = p.parents
let theories p = p.theories
let additional_info p = p.additional_info

let equal p1 p2 =
  p1.kind = p2.kind &&
  p1.additional_info = p2.additional_info &&
  p1.theories = p2.theories &&
  match p1.result, p2.result with
  | Form f1, Form f2 -> TypedSTerm.equal f1 f2
  | Clause c1, Clause c2 -> CC.equal c1 c2
  | _ -> false

let hash_fun p h =
  h |> Hash.int_ (Hashtbl.hash p.kind)
  |> (fun h -> match p.result with
      | Form f -> TypedSTerm.hash_fun f h
      | Clause c -> CC.hash_fun c h)

let hash p = Hash.apply hash_fun p

let compare p1 p2 =
  let c = Pervasives.compare p1.kind p2.kind in
  if c = 0
  then match p1.result, p2.result with
    | Form f1, Form f2 -> TypedSTerm.compare f1 f2
    | Clause c1, Clause c2 -> CC.compare c1 c2
    | Form _, Clause _ -> 1
    | Clause _, Form _ -> ~-1
  else c

(** {2 Constructors and utils} *)

let mk_f_trivial ?(info=[]) ?(theories=[]) f =
  { result=Form f; kind=Trivial; theories;
    parents = [| |]; additional_info=info; }

let mk_f_file ?(conjecture=false) ?(info=[]) ?(theories=[]) ~file ~name f =
  let src = StatementSrc.make ~is_conjecture:conjecture ~name file in
  { result=Form f; kind=File src; theories;
    parents = [| |]; additional_info=info; }

let mk_f_inference ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; theories; kind=Inference(rule);
    parents = Array.of_list parents; additional_info=info; }

let mk_f_simp ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; kind=Simplification(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_f_esa ?(info=[]) ?(theories=[]) ~rule f parents =
  { result=Form f; kind=Esa(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_c_trivial ?(info=[]) ?(theories=[]) c =
  { result=Clause c; kind=Trivial; theories;
    parents = [| |]; additional_info=info; }

let mk_c_src ?(info=[]) ?(theories=[]) ~src c =
  { result=Clause c; kind=File src; theories;
    parents = [| |]; additional_info=info; }

let mk_c_file ?(conjecture=false) ?(info=[]) ?(theories=[]) ~file ~name c =
  let src = StatementSrc.make ~is_conjecture:conjecture ~name file in
  mk_c_src ~info ~theories ~src c

let mk_c_inference ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; theories; kind=Inference(rule);
    parents = Array.of_list parents; additional_info=info; }

let mk_c_simp ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; kind=Simplification(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let mk_c_esa ?(info=[]) ?(theories=[]) ~rule c parents =
  { result=Clause c; kind=Esa(rule); theories;
    parents=Array.of_list parents; additional_info=info; }

let adapt_c p c =
  { p with result=Clause c; }

let adapt_f p f =
  { p with result=Form f; }

let is_file = function
  | {kind=File _; _} -> true
  | _ -> false

let is_trivial = function
  | {kind=Trivial; _} -> true
  | _ -> false

let is_proof_of_false p =
  match p.result with
  | Form f when TypedSTerm.equal f TypedSTerm.Form.false_ -> true
  | Clause c when CC.is_empty c -> true
  | _ -> false

let has_absurd_lits p = match p.result with
  | Clause c -> CompactClause.has_absurd_lits c
  | _ -> false

let rule p = match p.kind with
  | Trivial
  | File _ -> None
  | Esa rule
  | Simplification rule
  | Inference rule -> Some rule

let is_conjecture p = match p.kind with
  | File {StatementSrc.is_conjecture=true; _} -> true
  | _ -> false

module Theories = struct
  let eq = ["equality"]
  let arith = ["equality"; "arith"]
end

(** {2 Proof traversal} *)

module ProofTbl = Hashtbl.Make(struct
    type t = proof
    let equal = equal
    let hash = hash
  end)

type proof_set = unit ProofTbl.t

type proof_name = int ProofTbl.t

let traverse_depth ?(traversed=ProofTbl.create 11) proof k =
  let depth = ref 0 in
  let current, next = ref [proof], ref [] in
  while !current <> [] do
    (* exhaust the current layer of proofs to explore *)
    while !current <> [] do
      let proof = List.hd !current in
      current := List.tl !current;
      if ProofTbl.mem traversed proof then ()
      else begin
        ProofTbl.add traversed proof ();
        (* traverse premises first *)
        Array.iter (fun proof' -> next := proof' :: !next) proof.parents;
        (* yield proof *)
        k (proof, !depth);
      end
    done;
    (* explore next layer *)
    current := !next;
    next := [];
    incr depth;
  done

let traverse ?traversed proof k =
  traverse_depth ?traversed proof (fun (p, _depth) -> k p)

let distance_to_conjecture p =
  let best_distance = ref None in
  traverse_depth p
    (fun (p', depth) ->
       if is_conjecture p'
       then
         let new_best = match !best_distance with
           | None -> depth
           | Some depth' -> max depth depth'
         in
         best_distance := Some new_best
    );
  !best_distance

let get_name ~namespace p =
  try
    ProofTbl.find namespace p
  with Not_found ->
    let n = ProofTbl.length namespace in
    ProofTbl.add namespace p n;
    n

let to_seq proof = Sequence.from_iter (fun k -> traverse proof k)

(** Depth of a proof, ie max distance between the root and any axiom *)
let depth proof =
  let explored = ProofTbl.create 11 in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (proof, 0) q;
  while not (Queue.is_empty q) do
    let (p, d) = Queue.pop q in
    if ProofTbl.mem explored proof then () else begin
      ProofTbl.add explored proof ();
      begin match p.kind with
        | File _ | Trivial -> depth := max d !depth
        | Inference _ | Esa _ | Simplification _ -> ()
      end;
      (* explore parents *)
      Array.iter (fun p -> Queue.push (p, d+1) q) p.parents
    end
  done;
  !depth

(* physically share subproofs, to save memory *)
let share t =
  let h = ProofTbl.create 15 in
  let rec share t =
    let t' = { t with
               parents = Array.map share t.parents;
             } in
    try ProofTbl.find h t'
    with Not_found ->
      ProofTbl.add h t' t';
      t'
  in
  share t

(** {2 Conversion to a graph of proofs} *)

(** Get a graph of the proof *)
let as_graph =
  {CCGraph.
    origin=fst;
    dest=(fun (_,(_,n)) -> n);
    children=(fun p ->
      match rule p with
      | None -> Sequence.empty
      | Some rule ->
          let parents = Sequence.of_array p.parents in
          Sequence.map (fun p' -> p,(rule, p')) parents
    );
  }

(** {2 IO} *)

let pp_kind_tstp out k =
  match k with
  | File {StatementSrc.file; name=Some name; _} ->
      Format.fprintf out "file('%s', '%s')" file name
  | File {StatementSrc.file; name=None; _} ->
      Format.fprintf out "file('%s')" file
  | Inference rule ->
      Format.fprintf out "inference(%s, [status(thm)])" rule
  | Simplification rule ->
      Format.fprintf out "inference(%s, [status(thm)])" rule
  | Esa rule ->
      Format.fprintf out "inference(%s, [status(esa)])" rule
  | Trivial ->
      Format.fprintf out "trivial([status(thm)])"

let pp_kind out k =
  match k with
  | File {StatementSrc.file; name=Some name; is_conjecture=c; _} ->
      Format.fprintf out "'%s' in '%s'%s" name file
        (if c then " (conjecture)" else "")
  | File {StatementSrc.file; name=None; is_conjecture=c; _} ->
      Format.fprintf out "'%s'%s" file (if c then " (conjecture)" else "")
  | Inference rule ->
      Format.fprintf out "inf %s" rule
  | Simplification rule ->
      Format.fprintf out "simp %s" rule
  | Esa rule ->
      Format.fprintf out "esa %s" rule
  | Trivial -> CCFormat.string out "trivial"

let pp_result out = function
  | Form f -> TypedSTerm.pp out f
  | Clause c -> CC.pp out c

let pp_result_of out proof = pp_result out proof.result

let pp_notrec out p =
  Format.fprintf out "%a <-- %a [%a]"
    pp_result_of p pp_kind p.kind
    (Util.pp_list CCFormat.string) p.theories

let pp_debug out proof =
  Format.fprintf out "@[<v>";
  traverse proof
    (fun p -> match p.kind with
      | File _ ->
          Format.fprintf out "@[<hv2>@[%a@] <--@ %a,@ theories [%a]@]@,"
            pp_result p.result pp_kind p.kind
            (Util.pp_list CCFormat.string) p.theories
      | Trivial ->
          Format.fprintf out "@[<hv2>@[%a@] <--@ trivial,@ theories [%a]@]@,"
            pp_result p.result (Util.pp_list CCFormat.string) p.theories;
      | Inference _
      | Simplification _
      | Esa _ ->
          Format.fprintf out "@[<hv2>@[%a@] <--@ %a,@ theories [%a]@ with @[<hv>%a@]@]@,"
            pp_result p.result pp_kind p.kind
            (Util.pp_list CCFormat.string) p.theories
            (CCFormat.array pp_result)
            (Array.map (fun p->p.result) p.parents)
    );
  Format.fprintf out "@]"

let _pp_parent out = function
  | `Name i -> Format.fprintf out "%d" i
  | `Theory s -> Format.fprintf out "theory(%s)" s

let _pp_kind_tstp out (k,parents) =
  let pp_parents = Util.pp_list _pp_parent in
  match k with
  | Trivial ->
      Format.fprintf out "trivial([%a])" pp_parents parents
  | File {StatementSrc.file; name=Some name; _} ->
      Format.fprintf out "file('%s', '%s', [%a])" file name pp_parents parents
  | File {StatementSrc.file; name=None; _} ->
      Format.fprintf out "file('%s', [%a])" file pp_parents parents
  | Inference rule ->
      Format.fprintf out "inference('%s', [status(thm)], [%a])"
        rule pp_parents parents
  | Simplification rule ->
      Format.fprintf out "inference('%s', [status(thm)], [%a])"
        rule pp_parents parents
  | Esa rule ->
      Format.fprintf out "inference('%s', [status(esa)], [%a])"
        rule pp_parents parents

let pp_tstp out proof =
  let namespace = ProofTbl.create 5 in
  Format.fprintf out "@[<v>";
  traverse proof
    (fun p ->
       let name = get_name ~namespace p in
       let parents =
         List.map (fun p -> `Name (get_name ~namespace p)) (Array.to_list p.parents) @
         List.map (fun s -> `Theory s) p.theories
       in
       let role = "plain" in (* TODO *)
       match p.result with
       | Form f ->
           Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
             name role TypedSTerm.TPTP.pp f _pp_kind_tstp (p.kind,parents)
       | Clause c when CC.trail c <> [] ->
           (* FIXME: proper conversion of clauses *)
           Format.fprintf out "@[<2>tff(%d, %s,@ @[<2>@[(%a)@]@ %a@],@ @[%a@]).@]@,"
             name role
             TypedSTerm.TPTP.pp
               (CC.to_forms c
                  |> Array.map (SLiteral.map ~f:FOTerm.Conv.to_simple_term)
                  |> Array.map SLiteral.to_form
                  |> Array.to_list
                  |> TypedSTerm.Form.or_
                  |> TypedSTerm.Form.close_forall)
             CC.pp_trail_tstp (CC.trail c)
             _pp_kind_tstp (p.kind,parents)
       | Clause c ->
           Format.fprintf out "@[<2>cnf(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
             name role CC.pp_tstp c _pp_kind_tstp (p.kind,parents)
    );
  Format.fprintf out "@]";
  ()

(** Prints the proof according to the given input switch *)
let pp switch out proof = match switch with
  | "none" -> Util.debug ~section 1 "proof printing disabled"
  | "tstp" -> pp_tstp out proof
  | "debug" -> pp_debug out proof
  | _ -> failwith ("unknown proof-printing format: " ^ switch)

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
  CCFormat.ksprintf ~f:_escape_dot fmt

let pp_dot_seq ~name out seq =
  let no_other_info proof = match proof.theories, proof.additional_info with
    | [], [] -> true
    | _ -> false
  in
  (* TODO: check proof is a DAG *)
  CCGraph.Dot.pp_seq
    ~name ~graph:as_graph
    ~attrs_v:(fun p ->
      let label = if no_other_info p
      then _to_str_escape "@[<2>%a@]" pp_result_of p
      else
        CCFormat.sprintf "{%s|{theories:%s|info:%s}}"
          (_to_str_escape "@[<2>%a@]" pp_result_of p)
          (_to_str_escape "%a" _pp_list_str p.theories)
          (_to_str_escape "%a" _pp_list_str p.additional_info)
      in
      let attrs = [`Label label; `Style "filled"] in
      let shape = if no_other_info p then `Shape "box" else `Shape "record" in
      if is_proof_of_false p then [`Color "red"; `Label "[]"; `Shape "box"; `Style "filled"]
      else if has_absurd_lits p then `Color "orange" :: shape :: attrs
      else if is_file p then `Color "yellow" :: shape :: attrs
      else if is_conjecture p then `Color "green" :: shape :: attrs
      else if is_trivial p then `Color "cyan" :: shape :: attrs
      else shape :: attrs
    )
    ~attrs_e:(fun (_,(e,_)) -> [`Label e])
    out
    seq;
  Format.pp_print_flush out ();
  ()

let pp_dot ~name out proof = pp_dot_seq ~name out (Sequence.singleton proof)

let pp_dot_seq_file ?(name="proof") filename seq =
  (* print graph on file *)
  Util.debugf ~section 1 "print proof graph to@ `%s`" (fun k->k filename);
  try
    CCIO.with_out filename
      (fun oc ->
        let out = Format.formatter_of_out_channel oc in
        Format.fprintf out "%a@." (pp_dot_seq ~name) seq)
  with e ->
    Util.debugf ~section 1 "error: %s" (fun k->k (Printexc.to_string e))

let pp_dot_file ?name filename proof =
  pp_dot_seq_file ?name filename (Sequence.singleton proof)
