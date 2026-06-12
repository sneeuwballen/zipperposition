(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Zipperposition proof checker for MDAG traces} *)

open Logtk
open Logtk_proofs

let ( let@ ) = ( @@ )
let file = ref ""
let verbose = ref false
let check_proof = ref true
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
        Arg.String (fun s -> dot_prefix := Some s),
        " DOT prefix for debugging failed steps" );
    ]

let parse_args () =
  Arg.parse options (fun f -> file := f) "check MDAG proof trace"

let instantiate f inst =
  let f = TypedSTerm.rename_all_vars f in
  let vars, body = TypedSTerm.unfold_binder Binder.Forall f in
  let subst = List.fold_left2 Var.Subst.add Var.Subst.empty vars inst in
  TypedSTerm.Subst.eval subst body

let run file =
  let@ () = Trace_tef.with_setup () in
  let data = In_channel.with_open_bin file In_channel.input_all in
  Printf.eprintf "[checker] reading %d bytes from %s\n%!" (String.length data)
    file;
  let decoder = Proof_trace_decode.create data in
  Printf.eprintf "[checker] decoder created\n%!";
  let proof, footer =
    let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "decoder-proof" in
    Proof_trace_decode.decode_proof decoder
  in
  Printf.eprintf "[checker] proof decoded successfully\n%!";
  Format.printf "proof: %a@." Proof.S.pp_zf proof;
  if !verbose then
    Format.printf "@[<2>metadata:@,%a@]@."
      (fun out kv ->
        List.iter (fun (k, v) -> Format.fprintf out "  %s = %s@." k v) kv)
      footer;
  if !check_proof then (
    let@ _sp = Trace_core.with_span ~__FILE__ ~__LINE__ "check-proof" in
    Printf.eprintf "[checker] converting proof to LLProof...\n%!";
    let p' = LLProof_conv.conv proof in
    Printf.eprintf "[checker] LLProof conversion done, checking...\n%!";
    let n_steps = ref 0 in
    let failures = ref [] in
    let on_check (p : LLProof.t) (r : LLProof_check.check_step_res) =
      incr n_steps;
      let step_str =
        match LLProof.step p with
        | LLProof.Goal -> "goal"
        | Assert -> "assert"
        | Negated_goal _ -> "neg-goal"
        | Trivial -> "trivial"
        | By_def id -> Printf.sprintf "by-def(%s)" (Name.to_string id)
        | Define id -> Printf.sprintf "define(%s)" (Name.to_string id)
        | Instantiate _ -> "instantiate"
        | Esa (name, _) -> Printf.sprintf "esa(%s)" name
        | Inference { name; _ } -> name
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
        | ( LLProof.Inference { intros; parents; _ },
            LLProof_check.CS_check LLProof_check.R_fail ) ->
          let dump_vars label f =
            let vars, _body = TypedSTerm.unfold_binder Binder.Forall f in
            Format.printf "    %s forall vars: @[%a@]@," label
              (Util.pp_list ~sep:",@ " (fun out v ->
                   Format.fprintf out "%a#%d:%a" Var.pp v (Hashtbl.hash v)
                     TypedSTerm.pp (Var.ty v)))
              vars
          in
          dump_vars "concl (before intros)" (LLProof.concl p);
          List.iteri
            (fun i parent ->
              let prem_concl = LLProof.concl parent.LLProof.p_proof in
              dump_vars (Printf.sprintf "prem[%d] (before inst)" i) prem_concl)
            parents;
          let concl =
            let f = instantiate (LLProof.concl p) intros in
            TypedSTerm.Subst.eval Var.Subst.empty f
          in
          Format.printf "    concl (after):@ @[%a@]@," TypedSTerm.pp_with_ty
            concl;
          List.iteri
            (fun i parent ->
              let prem_concl = LLProof.concl parent.LLProof.p_proof in
              let inst = parent.LLProof.p_inst in
              let deforall =
                if inst <> [] then
                  instantiate prem_concl inst
                else
                  prem_concl
              in
              Format.printf "    prem[%d] (after):@ @[%a@]@," i
                TypedSTerm.pp_with_ty deforall)
            parents;
          Format.printf "@]@."
        | _ -> ()
      )
    in
    let res, stats = LLProof_check.check ?dot_prefix:!dot_prefix ~on_check p' in
    if !verbose && !failures <> [] then (
      Format.printf "failing steps (with substitutions):@.";
      List.iter
        (fun (p, name) ->
          let step = LLProof.step p in
          Format.printf "@[<v2>  %s@ :concl@ @[%a@]" name TypedSTerm.pp_with_ty
            (LLProof.concl p);
          (match step with
          | LLProof.Inference { intros; local_intros; _ } ->
            if intros <> [] then
              Format.printf "@ :intros @[%a@]"
                (Util.pp_list ~sep:",@ " TypedSTerm.pp)
                intros;
            if local_intros <> [] then
              Format.printf "@ :local-intros @[%a@]"
                (Util.pp_list ~sep:",@ " TypedSTerm.pp)
                local_intros
          | _ -> ());
          List.iteri
            (fun i parent ->
              let prem = parent.LLProof.p_proof in
              Format.printf "@ :prem[%d]@ @[%a@]@ :res %a" i LLProof.pp_step
                (LLProof.step prem) TypedSTerm.pp_with_ty (LLProof.concl prem);
              if parent.LLProof.p_inst <> [] then
                Format.printf "@ :inst @[%a@]"
                  (Util.pp_list ~sep:",@ " TypedSTerm.pp)
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
