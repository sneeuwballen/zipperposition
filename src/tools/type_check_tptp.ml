
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 TPTP Syntax and types checking} *)

open Logtk
open Logtk_parsers

module PT = STerm
module T = TypedSTerm
module Err = CCError
module A = Ast_tptp

let print_line () =
  Printf.printf "%s\n" (CCString.repeat "=" 60);
  ()

let cat_input = ref false  (* print input declarations? *)
let stats = ref false
let pp_base = ref false

let options = Arg.align (
    [ "--cat", Arg.Set cat_input, " print (annotated) declarations"
    ] @ Options.mk_global_opts ()
  )

(* check the given file *)
let check file =
  Err.(
    print_line ();
    Printf.printf "checking file %s...\n" file;
    Util_tptp.parse_file ~recursive:true file
    >>= fun decls ->
    Util_tptp.infer_types decls
    >>= fun decls' ->
    let sigma = Util_tptp.type_declarations decls' in
    Format.printf "@[<hv2>signature:@ %a@]@."
      (ID.Map.print ID.pp T.pp) sigma;
    (* print formulas *)
    if !cat_input then
      Format.printf "@[<2>formulas:@ %a@]@."
        (CCFormat.seq ~start:"" ~stop:"" ~sep:"" (A.pp T.pp))
        decls';
    if !stats then begin
      Format.printf "number of symbols: %d@." (ID.Map.cardinal sigma);
      Format.printf "number of input declarations: %d@." (Sequence.length decls);
    end;
    Err.return ()
  )

let main () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "check_tptp [options] [file1|stdin] file2...";
  (if !files = [] then files := ["stdin"]);
  files := List.rev !files;
  let res = Err.fold_l
      (fun () file -> check file)
      () !files;
  in
  match res with
  | `Ok () ->
      print_line ();
      Format.printf "success!@."
  | `Error msg ->
      print_endline msg

let _ =
  main ()
