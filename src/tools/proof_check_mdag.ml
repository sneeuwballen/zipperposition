(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof checker for MDAG proof traces} *)

open Logtk
open Logtk_proofs

let file = ref ""
let verbose = ref false
let print_proof = ref false

let options =
  Arg.align
    [
      "--verbose", Arg.Set verbose, " verbose output";
      "--print-proof", Arg.Set print_proof, " print decoded proof";
    ]

let parse_args () =
  Arg.parse options (fun f -> file := f) "check MDAG proof trace"

let pp_footer out kv =
  List.iter (fun (k, v) -> Format.fprintf out "  %s = %s@." k v) kv

let run file =
  let data = In_channel.with_open_bin file In_channel.input_all in
  let decoder = Proof_trace_decode.create data in
  let proof, footer = Proof_trace_decode.decode_proof decoder in
  if !verbose then (
    Format.printf "@[<2>footer:@,%a@]@." pp_footer footer;
    Format.printf "result: unsat@."
  );
  if !print_proof then
    Format.printf "@[<2>proof:@,@[%a@]@]@." Proof.S.pp_tstp proof;
  Printf.printf "proof trace decoded successfully@."

let () =
  parse_args ();
  if !file = "" then (
    Printf.eprintf "usage: zipper_proof_check_mdag [options] <file.mdag>\n%!";
    exit 1
  );
  run !file
