
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Find applied variables in a problem } *)

open Logtk
open Logtk_parsers

module T = TypedSTerm

type kind = PureFO | InFOOL | OutsideFOOL

let refine old_status new_status = 
  match old_status with
  | OutsideFOOL -> OutsideFOOL
  | InFOOL -> if new_status = OutsideFOOL then OutsideFOOL else InFOOL
  | PureFO -> new_status

let is_uniqueness_axiom f =
  let check_form f =
    let is_uniqueness_cl ~var t =
        match T.view t with
        | T.AppBuiltin(Eq, l) ->
        List.exists (fun t -> T.var_occurs ~var t) l &&
        List.exists (fun t -> T.is_const t) l
        | _ -> false
    in

    match T.view f with
    | T.Bind((Binder.Forall | Binder.Exists), var, body) ->
        (match T.view body with
        | T.AppBuiltin(Or, l) ->
        List.for_all (fun t -> is_uniqueness_cl ~var t) l 
        | _ -> false)
    | _ -> false
  in
  Statement.Seq.forms f
  |> Iter.exists check_form

let process file =
  let exception Stop of string in

  try
    let input = Parsing_utils.input_of_file file in
    let parsed = Parsing_utils.parse_file input file in
    (match parsed with 
      | CCResult.Error e -> raise (Stop (CCString.replace ~sub:"\n" ~by:"  " e))
      | _ -> ());
    let ast = CCResult.get_exn (parsed) in
    let typed_ast = TypeInference.infer_statements ?ctx:None
        ~on_var:(Input_format.on_var input)
        ~on_undef:(Input_format.on_undef_id input)
        ~on_shadow:(Input_format.on_shadow input)
        ~implicit_ty_args:false ast in
    Util.debugf 5 "Parse: %s" (fun k -> k (match typed_ast with | CCResult.Error e -> e | CCResult.Ok _ -> "OK"));
    (match typed_ast with 
      | CCResult.Error e -> raise (Stop (CCString.replace ~sub:"\n" ~by:"  " e))
      | _ -> ());
    if (Iter.exists (is_uniqueness_axiom) @@ CCVector.to_iter (CCResult.get_exn typed_ast)) 
    then CCFormat.printf "OK.@."
    else CCFormat.printf "NOK.@.";
  with 
  | Stop reason ->
    CCFormat.printf "FAIL: %s@." reason
  | CCResult.Get_error -> 
    CCFormat.printf "FAIL: %s is not parsable@." file
  

let options =
  Options.make()

let () =
  CCFormat.set_color_default true;
  let file = ref "" in
  let add_file = (:=) file in
  Arg.parse (Arg.align options) add_file "fo-detector [options] [files]";
  process !file
