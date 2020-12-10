
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

(* TODO: also pick statement printer based on Options.output *)
let pp_stmt out s =
  let pp_t = match !Options.output with
    | Logtk.Options.O_none | Logtk.Options.O_zf -> T.ZF.pp_inner
    | Logtk.Options.O_normal -> T.pp_inner
    | Logtk.Options.O_tptp -> T.TPTP.pp
  in
  Statement.pp pp_t pp_t pp_t out s

(* check the given file *)
let check file =
  if !dump then  (
    Format.printf "# file `%s`@." file;
  ) else (
    (* print_line ();
       Format.printf "checking file `%s`...@." file; *)
    ()
  );
  let input = Parsing_utils.input_of_file file in
  Parsing_utils.parse_file input file
  >>= TypeInference.infer_statements
    ~on_var:(Input_format.on_var input)
    ~on_undef:(Input_format.on_undef_id input)
    ~on_shadow:(Input_format.on_shadow input)
    ~implicit_ty_args:(Input_format.implicit_ty_args input)
    ?ctx:None
  >>= (fun decls ->
        decls
        |> CCVector.to_iter
        |> Cnf.cnf_of_iter ~ctx:(Skolem.create ())
        |> CCVector.to_iter
        |> Cnf.convert
        |> CCResult.return) >>= (
          fun stmts -> 
            let found_fool_subterm = ref false in
            CCVector.to_iter stmts 
            |> Iter.flat_map Statement.Seq.terms 
            |> (fun trm ->
                try ignore(Iter.for_all (fun t -> 
                  let ans, is_fool = Term.in_fool_fragment t in
                  found_fool_subterm := !found_fool_subterm || is_fool;
                  ans
                  ) trm); ""
                with Failure msg -> msg) 
            |> fun x -> 
               if (x = "") 
               then (if !found_fool_subterm 
                     then CCResult.return () 
                     else CCResult.fail ("FAIL: Pure FO problem")) 
               else CCResult.fail ("FAIL: " ^ x))
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
      (* print_line (); *)
      print_endline "OK."
    )
  | Err.Error msg ->
    print_endline msg

let _ =
  main ()
