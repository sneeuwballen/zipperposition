
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 TPTP Syntax and types checking} *)

open Logtk
open Logtk_parsers

module PT = STerm
module T = TypedSTerm
module Err = CCResult
module A = Ast_tptp

open Err.Infix

let print_line () =
  Printf.printf "%s\n" (CCString.repeat "=" 60);
  ()

let cat_input = ref false  (* print input declarations? *)
let pp_base = ref false

let options = Arg.align (
    [ "--cat", Arg.Set cat_input, " print (annotated) declarations"
    ] @ Options.make ()
  )

(* check the given file *)
let check file =
  print_line ();
  Format.printf "checking file `%s`...@." file;
  Parsing_utils.parse file
  >>= TypeInference.infer_statements ?ctx:None
  >|= fun decls ->
  let sigma =
    CCVector.to_seq decls
    |> Sequence.flat_map Statement.Seq.ty_decls
    |> ID.Map.of_seq
  in
  Format.printf "@[<hv2>signature:@ @[<v>%a@]@]@."
    (ID.Map.pp ~sep:"" ~arrow:" : " ID.pp T.pp) sigma;
  (* print formulas *)
  if !cat_input then
    let pp_stmt = Statement.pp T.pp T.pp T.pp in
    Format.printf "@[<v2>statements:@ %a@]@."
      (CCVector.pp ~sep:"" pp_stmt)
      decls;
    ()

let main () =
  CCFormat.set_color_default true;
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "check_tptp [options] [file1|stdin] file2...";
  if !files = [] then files := ["stdin"];
  files := List.rev !files;
  let res =
    Err.fold_l
      (fun () file -> check file)
      () !files;
  in
  match res with
    | Err.Ok () ->
      print_line ();
      Format.printf "@{<Green>success!@}@."
    | Err.Error msg ->
      print_endline msg

let _ =
  main ()
