
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
let dump = ref false
let pp_base = ref false

let options = Arg.align (
    [ "--cat", Arg.Set cat_input, " print (annotated) declarations"
    ; "--dump", Arg.Set dump, " print annotated declarations and nothing else"
    ] @ Options.make ()
  )

(* check the given file *)
let check file =
  if !dump then  (
    Format.printf "# file `%s`@." file;
  ) else (
    print_line ();
    Format.printf "checking file `%s`...@." file;
  );
  let input = Parsing_utils.input_of_file file in
  Parsing_utils.parse_file input file
  >>= TypeInference.infer_statements
    ~on_var:(Input_format.on_var input)
    ~on_undef:(Input_format.on_undef_id input)
    ~on_shadow:(Input_format.on_shadow input)
    ~implicit_ty_args:(Input_format.implicit_ty_args input)
    ?ctx:None
  >|= fun decls ->
  let sigma =
    CCVector.to_seq decls
    |> Sequence.flat_map Statement.Seq.ty_decls
    |> ID.Map.of_seq
  in
  if not !dump then (
    Format.printf "@[<hv2>signature:@ @[<v>%a@]@]@."
      (ID.Map.pp ~sep:"" ~arrow:" : " ID.pp T.pp) sigma;
  );
  (* print formulas *)
  (* FIXME: use [Options.output] *)
  if !dump then (
    let pp_stmt = Statement.ZF.pp T.ZF.pp_inner T.ZF.pp_inner T.ZF.pp_inner in
    CCFormat.set_color_default false;
    Format.printf "@[<v>%a@]@." (CCVector.pp ~sep:"" pp_stmt) decls;
  ) else if !cat_input then (
    let pp_stmt = Statement.pp T.pp T.pp T.pp in
    Format.printf "@[<v2>statements:@ %a@]@."
      (CCVector.pp ~sep:"" pp_stmt)
      decls;
  );
  ()

let main () =
  CCFormat.set_color_default true;
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "type_check [options] [file1|stdin] file2...";
  if !files = [] then files := ["stdin"];
  files := List.rev !files;
  let res =
    Err.fold_l
      (fun () file -> check file)
      () !files;
  in
  match res with
    | Err.Ok () ->
      if not !dump then (
        print_line ();
        Format.printf "@{<Green>success!@}@."
      )
    | Err.Error msg ->
      print_endline msg

let _ =
  main ()
