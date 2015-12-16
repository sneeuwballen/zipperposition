
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Reduction to CNF of TPTP file} *)

open Logtk
open Logtk_parsers

module T = TypedSTerm
module F = T.Form
module A = Ast_tptp
module Err = CCError

let declare_types = ref false
let print_sig = ref false
let flag_distribute_exists = ref false
let flag_disable_renaming = ref false

let options = Arg.align (
    [ "--declare", Arg.Set declare_types, " declare types of symbols"
    ; "--signature", Arg.Set print_sig, " print signature"
    ; "--distribute-exist", Arg.Set flag_distribute_exists,
      " distribute existential quantifiers during miniscoping"
    ; "--disable-def", Arg.Set flag_disable_renaming, " disable definitional CNF"
    ; "--time-limit", Arg.Int Util.set_time_limit, " hard time limit (in s)"
    ] @ Options.mk_global_opts ()
  )

(* TODO: choose between TPTP and regular printer *)

(* process the given file, converting it to CNF *)
let process file =
  Util.debugf 1 "process file %s" (fun k->k file);
  let res =
    Err.(
      (* parse *)
      Util_tptp.parse_file ~recursive:true file
      >>= fun decls ->
      (* to CNF *)
      Util_tptp.infer_types decls
      >>= fun decls ->
      let opts =
        (if !flag_distribute_exists then [Cnf.DistributeExists] else []) @
        (if !flag_disable_renaming then [Cnf.DisableRenaming] else []) @
        []
      in
      let decls = Util_tptp.to_cnf ~opts decls in
      let sigma = Cnf.type_declarations (CCVector.to_seq decls) in
      if !print_sig
      then (
        Format.printf "@[<hv2>signature:@ (@[<v>%a@]@])@."
          (ID.Map.print ~start:"" ~stop:"" ~sep:"" ~arrow:" : " ID.pp T.pp) sigma
      );
      (* print *)
      Format.printf "@[<v2>%d statements:@ %a@]@."
        (CCVector.length decls)
        (CCVector.print ~start:"" ~stop:"" ~sep:"" Cnf.pp_statement)
        decls;
      Err.return ()
    )
  in match res with
  | `Ok () -> ()
  | `Error msg ->
      print_endline msg;
      exit 1

let main () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "cnf_of_tptp [options] [file1|stdin] file2...";
  (if !files = [] then files := ["stdin"]);
  files := List.rev !files;
  List.iter process !files;
  Util.debug 1 "success!";
  ()

let _ =
  main ()
