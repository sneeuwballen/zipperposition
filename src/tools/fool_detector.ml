
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

let classify_stm f =
  let module Ty = T.Ty in
  let rec aux ~top t = 
    let ty = T.ty_exn t in
    let _,args,_ = Ty.unfold ty in
    if not (CCList.is_empty args) then OutsideFOOL
    else (
      let res =
        match T.view t with
        | Var _ -> if Ty.is_prop ty then InFOOL else PureFO
        | Const _ -> PureFO
        | App (hd, args) ->
          if T.is_fun hd || T.is_var hd then OutsideFOOL
          else aux_l ~top:false args
        | AppBuiltin(hd, args) ->
          let top =
            top && (Builtin.is_logical_op hd ||
            List.mem hd (Builtin.[True; False; Eq; Neq])) in
          aux_l ~top args
        | Ite(cond, cond_true, cond_false) ->
          refine (aux_l ~top [cond; cond_true;cond_false]) InFOOL
        | Let(var_def_pairs, body) ->
          let t_vars = List.map (fun p -> T.var (fst p)) var_def_pairs in
          let defs = List.map snd var_def_pairs in
          aux_l ~top (body :: t_vars @ defs)
        | Bind((Binder.Exists|Binder.Forall), var, body) ->
          aux_l ~top [T.var var; body]
        | _ -> OutsideFOOL in
      if not top && Ty.is_prop ty 
      then refine res InFOOL
      else res)
  and aux_l ~top = function
  | [] -> PureFO
  | x::xs ->
    CCList.fold_left (fun acc y -> 
      refine acc (aux ~top y)
    ) (aux ~top x) xs in
  aux ~top:true f

let statement_kind smt =
  Statement.Seq.forms smt
  |> Iter.fold_while (fun acc f ->
    let res = refine acc (classify_stm f) in
    let continue = if res == OutsideFOOL then `Stop else `Continue in
    res, continue
  ) PureFO


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
    let fool_found = ref false in
    CCVector.to_iter (CCResult.get_exn typed_ast)
    |> Iter.iter (fun stm ->
        match statement_kind stm with
        | OutsideFOOL ->
          let err = CCFormat.sprintf "Formula @[%a@] is not in FOOL" Statement.pp_input stm in
          raise (Stop err)
        | InFOOL -> fool_found := true
        | PureFO -> ());
    
    if not !fool_found then raise (Stop "Pure FO")
    else CCFormat.printf "OK.@."
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
