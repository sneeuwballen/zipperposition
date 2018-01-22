
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Find applied variables in a problem } *)

open Logtk
open Logtk_parsers

module T = TypedSTerm

(** encode a term *)
let rec detect_term t  =
  match T.view t with
    | T.App (f, args) -> T.is_var(f) || CCList.exists detect_term args
    | T.AppBuiltin (_, args) -> CCList.exists detect_term args
    | T.Bind (b, v, t) -> detect_term t
    | _ -> false

(** encode a statement *)
let detect_stmt stmt =
  match Statement.view stmt with
    | Statement.Def _ -> failwith "Not implemented"
    | Statement.Rewrite _ -> failwith "Not implemented"
    | Statement.Data _ -> failwith "Not implemented"
    | Statement.Lemma _ -> failwith "Not implemented"
    | Statement.Goal f -> detect_term f
    | Statement.NegatedGoal (_,_) -> failwith "Not implemented"
    | Statement.Assert f -> detect_term f
    | Statement.TyDecl (_, _) -> false

let process file =
  let input = Input_format.I_tptp in
  let parse = Util_tptp.parse_file ~recursive:true file in
  Util.debugf 5 "Parse: %s" (fun k -> k (match parse with | CCResult.Error e -> e | CCResult.Ok _ -> "OK"));
  let ast = Sequence.map Util_tptp.to_ast (CCResult.get_exn parse) in
  let typed_ast = TypeInference.infer_statements ?ctx:None
      ~on_var:(Input_format.on_var input)
      ~on_undef:(Input_format.on_undef_id input)
      ~on_shadow:(Input_format.on_shadow input)
      ~implicit_ty_args:false ast in
  Util.debugf 5 "Parse: %s" (fun k -> k (match typed_ast with | CCResult.Error e -> e | CCResult.Ok _ -> "OK"));
  let typed_ast = CCVector.to_list (CCResult.get_exn typed_ast) in
  let detected = CCList.exists detect_stmt typed_ast in
  detected

let options =
  Options.make()

let () =
  CCFormat.set_color_default true;
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse (Arg.align options) add_file "fo-detector [options] [files]";
  let number = CCList.fold_left (fun n file ->
      let detected = process file in
      Format.printf "%s: %b\n" file detected;
      if detected then n+1 else n
    ) 0 !files in
  Format.printf "Total HO: %d/%d\n" number (List.length !files);
