(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Zipperposition proof checker for MDAG traces — Clause-Based} *)

open Logtk
open Logtk_proofs

let ( let@ ) = ( @@ )
let file = ref ""
let verbose = ref false
let check_proof = ref true
let dot_file = ref None
let dot_prefix = ref None
let debug_prover = ref false

let options =
  Arg.align
    [
      "--verbose", Arg.Set verbose, " verbose output";
      "--no-check", Arg.Clear check_proof, " skip proof verification";
      ( "--debug",
        Arg.Unit
          (fun () ->
            debug_prover := true;
            Util.set_debug 5),
        " show exact clauses fed to LLProver" );
      ( "--dot",
        Arg.String (fun s -> dot_file := Some s),
        " print LLProof DAG to file in DOT" );
      ( "--dot-failures",
        Arg.String (fun s -> dot_prefix := Some s),
        " DOT prefix for failed LLProver attempts" );
    ]

let parse_args () =
  Arg.parse options (fun f -> file := f) "check MDAG proof trace"

let run file =
  let@ () = Trace_tef.with_setup () in
  let data = In_channel.with_open_bin file In_channel.input_all in
  if !verbose then
    Printf.eprintf "[checker] reading %d bytes from %s\n%!" (String.length data)
      file;
  let decoder = Proof_trace_decode.create data in
  if !verbose then Printf.eprintf "[checker] decoder created\n%!";
  let proof, footer =
    let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "decoder-proof" in
    Proof_trace_decode.decode_proof decoder
  in
  if !verbose then
    Printf.eprintf "[checker] proof decoded successfully (id=%d)\n%!"
      (LLProof.id proof);
  if !verbose then
    Format.printf "@[<2>metadata:@,%a@]@."
      (fun out kv ->
        List.iter (fun (k, v) -> Format.fprintf out "  %s = %s@." k v) kv)
      footer;
  if !check_proof then (
    let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "check-proof" in
    if !verbose then Printf.eprintf "[checker] checking proof...\n%!";
    let n_steps = ref 0 in
    let failures = ref [] in
    let on_check (p : LLProof.t) (r : LLProof_check.check_step_res) =
      incr n_steps;
      let step_str =
        match LLProof.step p with
        | LLProof.Goal -> "goal"
        | LLProof.Assert -> "assert"
        | LLProof.Trivial -> "trivial"
        | LLProof.By_def id -> Printf.sprintf "by-def(%s)" (Name.to_string id)
        | LLProof.Define id -> Printf.sprintf "define(%s)" (Name.to_string id)
        | LLProof.Esa (name, _) -> Printf.sprintf "esa(%s)" name
        | LLProof.Inference { name; _ } -> name
      in
      let res_str =
        match r with
        | LLProof_check.CS_check LLProof_check.R_ok -> "OK"
        | LLProof_check.CS_check LLProof_check.R_fail ->
          failures := (p, step_str) :: !failures;
          "FAIL"
        | LLProof_check.CS_skip `ESA -> "SKIP(esa)"
        | LLProof_check.CS_skip `Tags -> "SKIP(tags)"
        | LLProof_check.CS_skip `Trivial -> "SKIP(trivial)"
        | LLProof_check.CS_skip `Other -> "SKIP(other)"
      in
      Format.printf "  [%3d] %-20s %s@." !n_steps step_str res_str;
      if !debug_prover then (
        match LLProof.step p, r with
        | ( LLProof.Inference { parents; _ },
            LLProof_check.CS_check LLProof_check.R_fail ) ->
          Format.printf "    concl: @[%a@]@," LLProof.pp_clause
            (LLProof.concl p);
          List.iteri
            (fun i parent ->
              let prem = parent.LLProof.p_proof in
              Format.printf "    prem[%d]: @[%a@]" i LLProof.pp_clause
                (LLProof.concl prem);
              if parent.LLProof.p_inst <> [] then
                Format.printf "@ :inst @[%a@]" LLProof.pp_inst
                  parent.LLProof.p_inst;
              Format.printf "@,")
            parents;
          Format.printf "@]@."
        | _ -> ()
      )
    in
    let res, stats =
      LLProof_check.check ?dot_prefix:!dot_prefix ~on_check proof
    in
    (match !dot_file with
    | Some file ->
      if !verbose then
        Printf.eprintf "[checker] writing LLProof DAG to %s\n%!" file;
      LLProof.Dot.pp_dot_file file proof
    | None -> ());
    if !verbose && !failures <> [] then (
      Format.printf "failing steps:@.";
      List.iter
        (fun (p, name) ->
          Format.printf "@[<v2>  %s@ :concl @[%a@]" name LLProof.pp_clause
            (LLProof.concl p);
          List.iteri
            (fun i parent ->
              let prem = parent.LLProof.p_proof in
              Format.printf "@ :prem[%d]@ @[%a@]" i LLProof.pp_clause
                (LLProof.concl prem);
              if parent.LLProof.p_inst <> [] then
                Format.printf "@ :inst @[%a@]" LLProof.pp_inst
                  parent.LLProof.p_inst)
            (LLProof.parents p);
          Format.printf "@]@.")
        (List.rev !failures)
    );
    Format.printf "@[<2>proof_check@ :res %a@ :stats %a@]@."
      LLProof_check.pp_res res LLProof_check.pp_stats stats;
    if res = LLProof_check.R_fail then (
      Printf.eprintf "proof check FAILED\n%!";
      exit 1
    )
  ) else
    Format.printf "proof trace decoded successfully (verification skipped)@."

let () =
  parse_args ();
  if !file = "" then (
    Printf.eprintf "usage: zipperposition_checker [options] <file.mdag>\n%!";
    exit 1
  );
  run !file
