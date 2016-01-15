
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition
open ProofStep

let section = ProofStep.section

module type CLAUSE = sig
  type t
  val lits : t -> Literals.t
  val trail : t -> Trail.t
  val pp : t CCFormat.printer
  val pp_tstp : t CCFormat.printer
end

module Make(C : CLAUSE) = struct
  type t = C.t ProofStep.of_

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
      Tbl.find namespace p.id
    with Not_found ->
      let n = Tbl.length namespace in
      Tbl.add namespace p.id n;
      n

  (** {2 Conversion to a graph of proofs} *)

  (** Get a graph of the proof *)
  let as_graph =
    {CCGraph.
      origin=fst;
      dest=(fun (_,(_,n)) -> n);
      children=(fun p ->
        match rule p.step with
        | None -> Sequence.empty
        | Some rule ->
            let parents = Sequence.of_list p.step.parents in
            Sequence.map (fun p' -> p, (rule, p')) parents);
    }

  (** {2 IO} *)

  let pp_result out = function
    | Form f -> TypedSTerm.pp out f
    | Clause c -> C.pp out c

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
        if Tbl.mem traversed proof.step.id then ()
        else (
          Tbl.add traversed proof.step.id ();
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

  let pp_debug out proof =
    let sep = "by" in
    Format.fprintf out "@[<v>";
    let pp_bullet out () = Format.fprintf out "@<1>@{<Green>*@}" in
    traverse proof
      (fun p -> match p.step.kind with
        | File _ ->
            Format.fprintf out "@[<hv2>%a @[%a@]@ %s %a@]@,"
              pp_bullet () pp_result p.result sep pp_kind p.step.kind
        | Trivial ->
            Format.fprintf out "@[<hv2>%a @[%a@]@ %s trivial@]@,"
              pp_bullet () pp_result p.result sep
        | Inference _
        | Simplification _
        | Esa _ ->
            Format.fprintf out "@[<hv2>%a @[%a@]@ %s %a@ with @[<hv>%a@]@]@,"
              pp_bullet () pp_result p.result sep pp_kind p.step.kind
              (CCFormat.list ~start:"" ~stop:"" pp_result)
              (List.map (fun p->p.result) p.step.parents));
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
        Format.fprintf out "inference('%a', [status(thm)], [%a])"
          pp_rule rule pp_parents parents
    | Simplification rule ->
        Format.fprintf out "inference('%a', [status(thm)], [%a])"
          pp_rule rule pp_parents parents
    | Esa rule ->
        Format.fprintf out "inference('%a', [status(esa)], [%a])"
          pp_rule rule pp_parents parents

  let pp_tstp out proof =
    let namespace = Tbl.create 5 in
    Format.fprintf out "@[<v>";
    traverse proof
      (fun p ->
         let name = get_name ~namespace p.step in
         let parents =
           List.map (fun p -> `Name (get_name ~namespace p.step)) p.step.parents
         in
         let role = "plain" in (* TODO *)
         match p.result with
         | Form f ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
               name role TypedSTerm.TPTP.pp f _pp_kind_tstp (p.step.kind,parents)
         | Clause c when not (Trail.is_empty (C.trail c)) ->
             assert false
             (* FIXME: proper conversion of clauses
             Format.fprintf out "@[<2>tff(%d, %s,@ @[<2>@[(%a)@]@ %a@],@ @[%a@]).@]@,"
               name role
               TypedSTerm.TPTP.pp
                 (C.to_forms c
                    |> Array.map (SLiteral.map ~f:FOTerm.Conv.to_simple_term)
                    |> Array.map SLiteral.to_form
                    |> Array.to_list
                    |> TypedSTerm.Form.or_
                    |> TypedSTerm.Form.close_forall)
               CC.pp_trail_tstp (CC.trail c)
               _pp_kind_tstp (p.kind,parents)
              *)
         | Clause c ->
             Format.fprintf out "@[<2>cnf(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
               name role C.pp_tstp c _pp_kind_tstp (p.step.kind,parents)
      );
    Format.fprintf out "@]";
    ()

  (** Prints the proof according to the given input switch *)
  let pp o out proof = match o with
    | Options.Print_none -> Util.debug ~section 1 "proof printing disabled"
    | Options.Print_tptp -> pp_tstp out proof
    | Options.Print_normal -> pp_debug out proof

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
    (* TODO: check proof is a DAG *)
    let equal p1 p2 = ProofStep.equal p1.step p2.step in
    let hash p = ProofStep.hash p.step in
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
        else if is_file p.step then `Color "yellow" :: shape :: attrs
        else if is_conjecture p.step then `Color "green" :: shape :: attrs
        else if is_trivial p.step then `Color "cyan" :: shape :: attrs
        else shape :: attrs
      )
      ~attrs_e:(fun (_,(r,_)) -> [`Label r.rule_name])
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
end
