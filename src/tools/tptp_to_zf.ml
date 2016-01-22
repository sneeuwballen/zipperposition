
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Convert from TPTP to ZF} *)

open Libzipperposition
open Libzipperposition_parsers

open CCError.Infix

module T = TypedSTerm

let pp_stmt = Statement.pp T.ZF.pp T.ZF.pp T.ZF.pp

let pp_stmts out seq =
  CCVector.print ~start:"" ~stop:"" ~sep:"" pp_stmt out seq

let declare_term out () =
  let id = ID.make "term" in
  let st = Statement.ty_decl ~src:UntypedAST.default_attrs id T.Ty.tType in
  pp_stmt out st

let process file =
  Util_tptp.parse_file ~recursive:true file
  >|= Sequence.map Util_tptp.to_ast
  >>= TypeInference.infer_statements ?ctx:None
  >|= fun stmts ->
  (* declare "term" then proceed *)
  Format.printf "@[<v>%a@,%a@]@." declare_term () pp_stmts stmts;
  ()

let options = Options.make()

let () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "tptp_to_zf [options] [file|stdin]";
  let file = match !files with
    | [] -> "stdin"
    | [f] -> f
    | _::_ -> failwith "expected at most one file"
  in
  let res = process file in
  match res with
  | `Ok () -> ()
  | `Error msg ->
      print_endline msg;
      exit 1


