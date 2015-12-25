
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Hash = CCHash
module CC = CompactClause

type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

module FileInfo = struct
  type t = {
    filename : string;  (* file name *)
    name : string;      (* statement name *)
    role : string;
    conjecture : bool;  (* conjecture/negated conjecture? *)
  }
end

(** Classification of proof steps *)
type step_kind =
  | Inference of string
  | Simplification of string
  | Esa of string
  | File of FileInfo.t
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

let mk_f_file ?(conjecture=false) ?(info=[]) ?(theories=[]) ~role ~file ~name f =
  let file_info = FileInfo.( {conjecture; name;filename=file; role; } ) in
  { result=Form f; kind=File file_info; theories;
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

let mk_c_file ?(conjecture=false) ?(info=[]) ?(theories=[]) ~role ~file ~name c =
  let file_info = FileInfo.( {conjecture; name;filename=file; role; } ) in
  { result=Clause c; kind=File file_info; theories;
    parents = [| |]; additional_info=info; }

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
  | {kind=File _} -> true
  | _ -> false

let is_trivial = function
  | {kind=Trivial} -> true
  | _ -> false

let is_axiom = function
  | {kind=File {FileInfo.role="axiom"}} -> true
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

let role p = match p.kind with
  | Trivial
  | Inference _
  | Esa _
  | Simplification _ -> "plain"
  | File {FileInfo.role=role} -> role

let is_conjecture p = match p.kind with
  | File {FileInfo.conjecture=true} -> true
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
  let f p =
    match rule p with
    | None -> LazyGraph.Node(p, p, Sequence.empty)
    | Some rule ->
        let parents = Sequence.of_array p.parents in
        let parents = Sequence.map (fun p' -> (rule, p')) parents in
        LazyGraph.Node (p, p, parents)
  in
  LazyGraph.make ~eq:equal ~hash:hash f

(** {2 IO} *)

let pp_kind_tstp out k =
  let module F = FileInfo in
  match k with
  | File {F.role=role; F.filename=file; F.name=name} ->
      Format.fprintf out "file('%s', '%s')" file name
  | Inference rule ->
      Format.fprintf out "inference(%s, [status(thm)])" rule
  | Simplification rule ->
      Format.fprintf out "inference(%s, [status(thm)])" rule
  | Esa rule ->
      Format.fprintf out "inference(%s, [status(esa)])" rule
  | Trivial ->
      Format.fprintf out "trivial([status(thm)])"

let pp_kind out k =
  let module F = FileInfo in
  match k with
  | File {F.role=role; F.filename=file; F.name=name; F.conjecture=c} ->
      Format.fprintf out "%s '%s' in '%s'%s" role name file
        (if c then " (conjecture)" else "")
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

let pp_result_of buf proof = pp_result buf proof.result

let pp_notrec buf p =
  Format.fprintf buf "%a <-- %a [%a]"
    pp_result_of p pp_kind p.kind
    (Util.pp_list CCFormat.string) p.theories

let pp_debug out proof =
  let module F = FileInfo in
  traverse proof
    begin fun p -> match p.kind with
      | File {F.role=role; F.filename=file; F.name=name; F.conjecture=c} ->
          Format.fprintf out "@[%a <---@ %a,@ theories [%a]%s@]"
            pp_result p.result pp_kind p.kind
            (Util.pp_list CCFormat.string) p.theories
            (if c then " (conjecture)" else "")
      | Trivial ->
          Format.fprintf out "@[<2>%a <---@ trivial,@ theories [%a]@]"
            pp_result p.result (Util.pp_list CCFormat.string) p.theories;
      | Inference _
      | Simplification _
      | Esa _ ->
          Format.fprintf out "@[%a <---@ %a,@ theories [%a] with %a@]"
            pp_result p.result pp_kind p.kind
            (Util.pp_list CCFormat.string) p.theories
            (CCFormat.array pp_result)
            (Array.map (fun p->p.result) p.parents)
    end

let _pp_parent out = function
  | `Name i -> Format.fprintf out "%d" i
  | `Theory s -> Format.fprintf out "theory(%s)" s

let _pp_kind_tstp out (k,parents) =
  let module F = FileInfo in
  match k with
  | Trivial ->
      Format.fprintf out "trivial([%a])"
        (Util.pp_list _pp_parent) parents
  | File {F.role=role; F.filename=file; F.name=name} ->
      Format.fprintf out "file('%s', '%s', [%a])"
        file name (Util.pp_list _pp_parent) parents
  | Inference rule ->
      Format.fprintf out "inference('%s', [status(thm)], [%a])"
        rule (Util.pp_list _pp_parent) parents
  | Simplification rule ->
      Format.fprintf out "inference('%s', [status(thm)], [%a])"
        rule (Util.pp_list _pp_parent) parents
  | Esa rule ->
      Format.fprintf out "inference('%s', [status(esa)], [%a])"
        rule (Util.pp_list _pp_parent) parents

let pp_tstp out proof =
  let namespace = ProofTbl.create 5 in
  traverse proof
    (fun p ->
       let name = get_name ~namespace p in
       let parents =
         List.map (fun p -> `Name (get_name namespace p)) (Array.to_list p.parents) @
         List.map (fun s -> `Theory s) p.theories
       in
       match p.result with
       | Form f ->
           Format.fprintf out "tff(%d, %s, %a, %a).\n"
             name (role p) TypedSTerm.TPTP.pp f _pp_kind_tstp (p.kind,parents)
       | Clause c when CC.trail c <> [] ->
           Format.fprintf out "tff(%d, %s, (%a) %a, %a).\n"
             name (role p)
             TypedSTerm.TPTP.pp
               (CC.to_forms c
                  |> Array.map (SLiteral.map ~f:FOTerm.to_simple_term)
                  |> Array.map SLiteral.to_form
                  |> Array.to_list
                  |> TypedSTerm.Form.or_
                  |> TypedSTerm.Form.close_forall)
             CC.pp_trail_tstp (CC.trail c)
             _pp_kind_tstp (p.kind,parents)
       | Clause c ->
           Format.fprintf out "cnf(%d, %s, %a, %a).\n"
             name (role p) CC.pp_tstp c _pp_kind_tstp (p.kind,parents)
    )

(** Prints the proof according to the given input switch *)
let pp switch buf proof = match switch with
  | "none" -> Util.debug 1 "proof printing disabled"
  | "tstp" -> pp_tstp buf proof
  | "debug" -> pp_debug buf proof
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

let as_dot_graph =
  let no_other_info proof = match proof.theories, proof.additional_info with
    | [], [] -> true
    | _ -> false
  in
  let label proof =
    let s = if no_other_info proof
      then _to_str_escape "%a" pp_result_of proof
      else
        CCFormat.sprintf "{%s|{theories:%s|info:%s}}"
          (_to_str_escape "%a" pp_result_of proof)
          (_to_str_escape "%a" _pp_list_str proof.theories)
          (_to_str_escape "%a" _pp_list_str proof.additional_info)
    in
    (* let s = Util.sprintf "%a" pp_result_of proof in *)
    `Label s
  in
  let shape proof = if no_other_info proof then `Shape "box" else `Shape "record" in
  let attributes = [`Style "filled"] in
  LazyGraph.map
    ~vertices:(fun p ->
        if is_proof_of_false p then `Color "red" ::
                                    `Label "[]" :: `Shape "box" :: attributes
        else if has_absurd_lits p then `Color "orange" :: label p :: shape p :: attributes
        else if is_file p then label p :: `Color "yellow" :: shape p :: attributes
        else if is_conjecture p then label p :: `Color "green" :: shape p :: attributes
        else if is_trivial p then label p :: `Color "cyan" :: shape p :: attributes
        else label p :: shape p :: attributes)
    ~edges:(fun e -> [`Label e])
    as_graph

let pp_dot_seq ~name out seq =
  Sequence.iter (fun proof ->
      if not (LazyGraph.is_dag as_graph proof) then begin
        (* output warning, cyclic proof *)
        let cycle = LazyGraph.find_cycle as_graph proof in
        let cycle = List.map (fun (v,_,_) -> v) cycle in
        let pp_squared out pf = Format.fprintf out "[%a]" pp_notrec pf in
        Util.debugf 0 "warning: proof is not a DAG (cycle %a)"
          (fun k->k (Util.pp_list pp_squared) cycle);
      end)
    seq;
  LazyGraph.Dot.pp ~name as_dot_graph out seq;
  Format.pp_print_flush out ();
  ()

(** Add the proof to the given graph *)
let pp_dot ~name buf proof =
  pp_dot_seq ~name buf (Sequence.singleton proof)

(** print to dot into a file *)
let pp_dot_seq_file ?(name="proof") filename seq =
  (* print graph on file *)
  Util.debugf 1 "print proof graph to %s" (fun k->k filename);
  try
    CCIO.with_out filename
      (fun oc ->
        let out = Format.formatter_of_out_channel oc in
        Format.fprintf out "%a@." (pp_dot_seq ~name) seq)
  with e ->
    Util.debugf 1 "error: %s" (fun k->k (Printexc.to_string e))

let pp_dot_file ?name filename proof =
  pp_dot_seq_file ?name filename (Sequence.singleton proof)
