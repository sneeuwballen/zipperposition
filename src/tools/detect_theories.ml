
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Check presence of theories in files} *)

open Libzipperposition
open Libzipperposition_parsers
open Libzipperposition_meta

module HOT = TypedSTerm
module A = Ast_ho
module E = CCError

let files = ref []
let theory_files = ref []
let flag_print_theory = ref false
let flag_print_cnf = ref false
let flag_print_signature = ref false

let add_file f = files := f :: !files
let add_theory f = theory_files := f :: !theory_files

let options = Arg.align (
  [ "--theory", Arg.String add_theory, " use given theory file"
  ; "--print-theory", Arg.Set flag_print_theory, " print the whole theory"
  ; "--print-cnf", Arg.Set flag_print_cnf, " print the clausal form of input files"
  ; "--print-signature", Arg.Set flag_print_signature, " print initial theory signature"
  ] @ Options.make ()
  )

(* parse the given theory files into the prover *)
let parse_files prover files =
  E.fold_l
    (fun p file -> E.map fst (Prover.parse_file p file))
    prover files

let to_cnf decls =
  E.(
    TypeInference.infer_statements ?ctx:None decls
    >|= fun decls -> Cnf.cnf_of_seq (CCVector.to_seq decls)
  )

let count_clauses v =
  CCVector.fold
    (fun acc st -> match Statement.view st with
      | Statement.Assert _
      | Statement.Goal _ -> acc+1
      | _ -> acc)
    0 v

let parse_and_cnf files =
  let q = Queue.create () in
  let res = E.(
    fold_l
    (fun () file ->
      Util.debugf 1 "parse input file %s" (fun k->k file);
      (* parse *)
      Util_tptp.parse_file ~recursive:true file
      >|= Sequence.map Util_tptp.to_ast
      >>= fun decls ->
      Util.debugf 3 "parsed %d declarations..." (fun k->k (Sequence.length decls));
      (* CNF *)
      to_cnf decls
      >>= fun stmts ->
      Util.debugf 3 "obtained %d clauses..." (fun k->k (count_clauses stmts));
      (* convert clauses into Encoding.foclause *)
      let stmts =
        CCVector.to_seq stmts
        |> Cnf.convert ~file
        |> CCVector.to_seq
        |> Sequence.flat_map Statement.Seq.forms
        |> Sequence.map Encoding.foclause_of_clause
      in
      Queue.add stmts q;
      return ()
    ) () files
  )
  in
  match res with
  | `Ok () ->
      E.return (Sequence.of_queue q |> Sequence.flatten)
  | `Error msg ->
      E.fail msg

(* print content of the reasoner *)
let print_theory r =
  Reasoner.Seq.to_seq r
    |> Format.printf "@[<v2>theory:@ %a@]@."
      (CCFormat.seq ~start:"" ~stop:"" ~sep:" " Reasoner.Clause.pp);
  ()

let print_clauses c =
  Format.printf "@[<v2>clauses:@ %a@]@."
    (CCFormat.seq ~sep:" " (Encoding.pp_clause FOTerm.pp)) c

let print_signature signature =
  Format.printf "@[<v2>signature:@,@[%a@]@]@."
    (CCFormat.seq ~sep:" " (Util.pp_pair ~sep:" : " ID.pp HOT.pp))
    (ID.Map.to_seq signature)

let pp_theory_axiom out (name, terms) =
  Format.fprintf out "@[<hv>%a@ %a@]" ID.pp name (Util.pp_list ~sep:" " HOT.pp) terms

let pp_rewrite_ ppt out l =
  Format.fprintf out "@[<v2>rewrite system@ %a@]"
    (Util.pp_list ~sep:"" (Util.pp_pair ~sep:" --> " ppt ppt)) l

let pp_rewrite_system = pp_rewrite_ FOTerm.pp
let pp_pre_rewrite_system = pp_rewrite_ HOT.pp

type result = {
  lemmas : Plugin.foclause Sequence.t;
  theories : (ID.t * HOT.t list) Sequence.t;
  axioms : (ID.t * HOT.t list) Sequence.t;
  rewrite : (FOTerm.t * FOTerm.t) list Sequence.t;
  pre_rewrite : (HOT.t * HOT.t) list Sequence.t;
}

(* detect theories in clauses *)
let detect_theories prover clauses =
  let facts = clauses
    |> Sequence.map Plugin.holds#to_fact
    |> Sequence.map Reasoner.Clause.fact
  in
  (* add clauses (ignore prover) *)
  let _, consequences = Prover.Seq.of_seq prover facts in
  let consequence_terms = Sequence.map fst consequences in
  (* filter theories, axioms, lemmas... *)
  let theories = Sequence.fmap Plugin.theory#of_fact consequence_terms
  and lemmas = Sequence.fmap Plugin.lemma#of_fact consequence_terms
  and axioms = Sequence.fmap Plugin.axiom#of_fact consequence_terms
  and rewrite = Sequence.fmap Plugin.rewrite#of_fact consequence_terms
  and pre_rewrite = Sequence.fmap Plugin.pre_rewrite#of_fact consequence_terms
  in
  { theories; lemmas; axioms; rewrite; pre_rewrite; }

let main () =
  Arg.parse options add_file "detect_theories [options] (file1|stdin) [file2...]";
  (* set default *)
  begin match !theory_files with
    | [] -> theory_files := ["data/builtin.theory"]
    | _ -> ()
  end;
  if !files = [] then files := ["stdin"];
  (* parse theory files *)
  let prover = Prover.empty in
  if !flag_print_signature then print_signature (Prover.signature prover);
  let res = E.(
    parse_files prover !theory_files
    >>= fun prover ->
    Util.debug 3 "theory files parsed";
    if !flag_print_theory then print_theory (Prover.reasoner prover);
    if !flag_print_signature then print_signature (Prover.signature prover);
    (* parse CNF formulas *)
    parse_and_cnf !files
    >>= fun clauses ->
    Util.debug 3 "input files parsed and translated to CNF";
    if !flag_print_cnf then print_clauses clauses;
    let results = detect_theories prover clauses in
    Util.debug 3 "theory detection done";
    E.return results
  ) in
  match res with
  | `Error msg ->
      Util.debugf 0 "error: %s" (fun k->k msg); exit 1
  | `Ok {theories; lemmas; axioms; rewrite; pre_rewrite; } ->
      Util.debug 1 "success!";
      Format.printf "@[<2>axioms:@ @[%a@]@]@."
        (CCFormat.seq ~sep:"\n  " pp_theory_axiom) axioms;
      Format.printf "@[<2>theories:@ @[%a@]@]@."
        (CCFormat.seq ~sep:"\n  " pp_theory_axiom) theories;
      Format.printf "@[<2>lemmas:@ @[%a@]@]@."
        (CCFormat.seq ~sep:"\n  " (Encoding.pp_clause FOTerm.pp)) lemmas;
      Format.printf "@[<2>rewrite systems:@ @[%a@]@]@."
        (CCFormat.seq~sep:"\n  " pp_rewrite_system) rewrite;
      Format.printf "@[<2>pre-rewrite systems:@ @[%a@]@]@."
        (CCFormat.seq ~sep:"\n  " pp_pre_rewrite_system) pre_rewrite;
      ()

let _ =
  try
    main ()
  with e ->
    Format.printf "%s@." (Printexc.to_string e);
    exit 1
