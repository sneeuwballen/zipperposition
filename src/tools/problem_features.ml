
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Reduction to CNF of TPTP file} *)

open Logtk
open Logtk_parsers

type feature =
  | Counter of string (* counter kind of feature *)
  | Vector of string (* occurence kind of feautre,
              for max/avg features *)

let get_f_name = function
  | Counter x -> x
  | Vector x -> x

module StrKey = struct
  type t = string
  let equal = CCString.equal
  let hash = CCString.hash
  let compare = CCString.compare
end
module StrTbl = CCHashtbl.Make(StrKey)

let cnt_map = StrTbl.create 64
let vec_map = StrTbl.create 64

let rec is_unit form = 
  match TypedSTerm.view form with
  | Bind ((Binder.Forall | Binder.Exists),_,body) -> is_unit body
  | AppBuiltin(hd, args) ->
    (Builtin.equal hd Builtin.Eq || Builtin.equal hd Builtin.Neq) &&
    (List.length args < 2 || 
      not (TypedSTerm.Ty.is_prop (TypedSTerm.ty_exn (List.hd args))))
  | _ -> true

let update feature value =
  match feature with
  | Counter name ->
    StrTbl.replace cnt_map name
           ((StrTbl.get_or cnt_map name ~default:0) + value)
  | Vector name ->
    StrTbl.replace vec_map name
           (value :: (StrTbl.get_or vec_map name ~default:[]))

let update_vec feature value =
  match feature with
  | Counter _ -> invalid_arg "counter feature expected"
  | Vector _ -> update feature value

let incr_counter feature =
  match feature with
  | Counter _ -> update feature 1
  | Vector _ -> invalid_arg "vector feature expected"


(* formula features *)
let num_forms = Counter "num_formulas" (* OK *)
let num_unit_form = Counter "num_unit_formulas" (*OK*)
let num_def_form = Counter "num_definition_formulas" (* OK *)
let num_pred_atoms = Counter "num_predicate_atoms" (*OK*)
let num_prop_atoms = Counter "num_prop_atoms" (*OK*)
let num_eq_atoms = Counter "num_equational_atoms" (*OK*)
let num_var_atoms = Counter "num_variable_atoms" (*OK*)
let f_depth = Vector "formula_depth" (*OK*)
let num_distinct_syms = Counter "num_distinct_non_var_syms" (*OK*)
let num_singleton_vars = Counter "num_singleton_vars" (* OK *)
let num_forall_quants = Counter "num_universal_quants" (*OK*)
let num_exists_quants = Counter "num_exists_quant" (*OK*)
let num_lambda_binders = Counter "num_lambda_binders" (*OK*)
let pred_arity = Vector "predicate_arity" (*OK*)
let num_functions = Counter "num_fun_syms" (*OK*)
let num_constants = Counter "num_consts" (*OK*)
let fun_arity = Vector "function_sym_arity" (*OK*)
let t_depth = Vector "term_depth" (*OK*)
let ftype_order = Vector "fun_type_order"
let vtype_order = Vector "var_type_order" (*OK*)
let lam_depth = Vector "lam_depth" (*OK*)
let num_quant_pred_vars = Counter "num_quant_pred_vars" (*OK*)
let num_nested_bools = Counter "num_nested_bools" (*OK*)
let num_logical_connectives = Counter "num_log_connectives" (*OK*)

(* clause features *)
let num_clauses  = Counter "num_clauses" (*OK*)
let num_non_horn = Counter "num_non_horn" (*OK*)
let num_unit_clauses = Counter "num_unit_clauses" (* OK*)
let clause_size = Vector "clause_size"
let pos_lits = Vector "pos_lits" (*OK*)
let neg_lits = Vector "neg_lits"
let pos_t_depth = Vector "pos_lit_term_depth"
let neg_t_depth = Vector "neg_lit_term_depth"

let all_features = [num_forms; num_unit_form; num_def_form  ; num_pred_atoms  ; num_prop_atoms; 
                    num_eq_atoms; num_var_atoms; f_depth; num_distinct_syms; num_singleton_vars; 
                    num_forall_quants; num_exists_quants; num_lambda_binders; pred_arity;
                    num_functions; num_constants  ; fun_arity; t_depth; ftype_order; vtype_order;
                    lam_depth; num_quant_pred_vars; num_nested_bools; num_logical_connectives;
                    num_clauses ; num_non_horn; num_unit_clauses; clause_size; pos_lits; 
                    neg_lits; pos_t_depth; neg_t_depth]


module T = TypedSTerm
module F = T.Form
module A = Ast_tptp

open CCResult.Infix

let traverse_term t =
  update_vec t_depth (T.depth t);
  if(T.Ty.is_prop (T.ty_exn t)) then (
    incr_counter num_pred_atoms;
    if(T.is_const t) then (incr_counter num_prop_atoms)
  );

  let rec aux top t =
    if (not top) && T.Ty.is_prop (T.ty_exn t) then (
      incr_counter num_nested_bools
    );
    match T.view t with
    | Var _  | Meta _ 
    | Const _ -> ()
    | AppBuiltin (b, l) ->
      if Builtin.is_logical_op b then (incr_counter num_logical_connectives);
      List.iter (aux true) l
    | App (hd, l) -> List.iter (aux false) (hd::l)
    | Bind (b,v,t) -> 
      if Binder.equal b Binder.Lambda then (
        incr_counter num_lambda_binders;
        update_vec lam_depth (T.depth t + 1)
      )
      else if Binder.equal b Binder.Forall || Binder.equal b Binder.exists then (
        if Binder.equal b Binder.Forall then (
          incr_counter num_forall_quants;
        ) else if Binder.equal b Binder.Exists then (
          incr_counter num_exists_quants
        );
        let var_order = 
          (if snd @@ T.Ty.arity @@ Var.ty v != 0 then 1 else 0)
          + T.Ty.order (Var.ty v) in
        update_vec vtype_order var_order;
        if (T.Ty.returns_prop (Var.ty v)) then incr_counter num_quant_pred_vars
      );
      aux true t
    | _ -> failwith "not implemented"
  in
  aux true t


let rec traverse_formula f =
  match T.view f with
  | Var _  | Meta _ 
  | Const _ -> traverse_term f
  | AppBuiltin (b, l) ->
    if ((Builtin.equal b Builtin.Eq ||
        Builtin.equal b Builtin.Neq) &&
        List.length l == 3 &&
        not (T.Ty.is_prop (List.hd l))) then (
      incr_counter num_eq_atoms;
      let lhs = List.nth l 1 and rhs = List.nth l 2 in
      if (T.is_var lhs && T.is_var rhs) then (
        incr_counter num_var_atoms
      );
      traverse_term lhs;
      traverse_term rhs
    ) else (
      incr_counter num_logical_connectives;
      List.iter traverse_formula l
    )
  | App (hd, l) -> traverse_term f
  | Bind (b,v,t) -> 
    if Binder.equal b Binder.Lambda then failwith "should not appear on top."
    else if Binder.equal b Binder.Forall || Binder.equal b Binder.exists then (
      if Binder.equal b Binder.Forall then (
        incr_counter num_forall_quants;
      ) else if Binder.equal b Binder.Exists then (
        incr_counter num_exists_quants
      );
      let var_order = 
        (if snd @@ T.Ty.arity @@ Var.ty v != 0 then 1 else 0)
        + T.Ty.order (Var.ty v) in
      update_vec vtype_order var_order;
      if (T.Ty.returns_prop (Var.ty v)) then incr_counter num_quant_pred_vars
    );
    traverse_formula t
  | _ -> failwith "not implemented"


let update_form_statistics f =
  if is_unit f then incr_counter num_unit_form;
  update_vec f_depth (TypedSTerm.depth f);

  let vars = ID.Tbl.create 64 in

  TypedSTerm.Seq.vars f
  |> Iter.iter (fun var -> 
    let id = Var.id var in
    ID.Tbl.replace vars id (1 + ID.Tbl.get_or ~default:0 vars id)
  );

  ID.Tbl.iter (fun key num ->  
    (* once when it is quantified, and once in the body. *)
    if num=1 then (incr_counter num_singleton_vars);
  ) vars;

  traverse_formula f

let collect_formula_features stmts =
  CCVector.iter (fun stm ->
    begin 
      match Statement.view stm with
      | Def _ | Rewrite _ ->
        incr_counter num_def_form;
        incr_counter num_forms
      | Assert _ | Goal _ | NegatedGoal _ ->
        incr_counter num_forms
      | TyDecl (id, ty) ->
        if(not (TypedSTerm.Ty.returns_tType ty)) then (
          incr_counter num_distinct_syms;
          update_vec ftype_order (TypedSTerm.Ty.order ty);
          let arity = snd @@ TypedSTerm.Ty.arity ty in
          if(TypedSTerm.Ty.returns_prop ty) then (
            (* predicate *)
            update_vec pred_arity arity
          ) else (
            (* function symbol *)
            update_vec fun_arity arity;
            if (arity == 0) then incr_counter num_constants
            else incr_counter num_functions
          )
        )
      | _ -> CCFormat.fprintf CCFormat.stderr 
                "Warning: %a is ignored." Statement.pp_input stm;
    end;
    Statement.Seq.forms stm
    |> Iter.iter update_form_statistics;
  ) stmts


let collect_clause_features clauses =
  Cnf.convert @@ CCVector.to_iter (clauses)
  |> CCVector.iter (fun cl ->
    let lits = 
      Iter.to_list @@ 
      Iter.filter (fun l -> not (SLiteral.is_true l) && not (SLiteral.is_false l)) 
      (Statement.Seq.lits cl) in
    let n = List.length lits in
    if (n > 0) then (
      let is_horn = List.length (List.filter SLiteral.is_pos lits) <= 1 in
      incr_counter num_clauses;
      if not is_horn then incr_counter num_non_horn;
      if n == 1 then incr_counter num_unit_clauses;
      update_vec clause_size n;
      update_vec pos_lits (List.length (List.filter SLiteral.is_pos lits));
      update_vec neg_lits (List.length (List.filter SLiteral.is_neg lits));
      List.iter (function 
      | SLiteral.Atom(t,sign) ->
        if sign then update_vec pos_t_depth (Term.depth t) else
        update_vec neg_t_depth (Term.depth t)
      | SLiteral.Eq(lhs, rhs) ->
        update_vec pos_t_depth (Term.depth lhs);
        update_vec pos_t_depth (Term.depth rhs)
      | SLiteral.Neq(lhs, rhs) ->
        update_vec neg_t_depth (Term.depth lhs);
        update_vec neg_t_depth (Term.depth rhs)
      | _ -> ()
      ) lits
    )
    
  )


let cnf_features = ref true

let options =
  [ "--cnf-features", Arg.Bool ((:=) cnf_features), " clausify the problem and include CNF features"] @
  Options.make ()
  |> List.sort Pervasives.compare
  |> Arg.align

(* process the given file, converting it to CNF *)
let process file =
  Util.debugf 1 "process file %s" (fun k->k file);
  let res =
  (* parse *)
  Util_tptp.enable_def_as_rewrite := true;
  let input = Parsing_utils.input_of_file file in
  Parsing_utils.parse_file input file
  >>= TypeInference.infer_statements ?ctx:None ~file
    ~on_var:(Input_format.on_var input)
    ~on_undef:(Input_format.on_undef_id input)
    ~on_shadow:(Input_format.on_shadow input)
    ~implicit_ty_args:(Input_format.implicit_ty_args input)
  >|= fun st ->
  collect_formula_features st;
  if !cnf_features then (
    collect_clause_features
      (Cnf.cnf_of_iter ~ctx:(Skolem.create()) (CCVector.to_iter st))
  )

  in match res with
  | CCResult.Ok () ->
    List.iter (function
      | Counter n ->
        if CCOpt.is_none (StrTbl.get cnt_map n) then (
          StrTbl.replace cnt_map n 0)
      | Vector n ->
        if CCOpt.is_none (StrTbl.get vec_map n) then (
          StrTbl.replace vec_map n [-1]
        )) all_features;
    
    
    let res_map = StrTbl.create 128 in
    
    StrTbl.iter (fun key val_ -> 
      StrTbl.replace res_map key (float_of_int val_)
    ) cnt_map;

    StrTbl.iter (fun key val_ -> 
      let iter_val = Iter.of_list val_ in
      let max_v = float_of_int (Iter.max_exn iter_val) in
      let min_v = float_of_int (Iter.min_exn iter_val) in
      let avg_v = (float_of_int (Iter.sum iter_val)) 
                  /. (float_of_int (Iter.length iter_val)) in
      StrTbl.replace res_map ("max_" ^ key) max_v;
      StrTbl.replace res_map ("min_" ^ key) min_v;
      StrTbl.replace res_map ("avg_" ^ key) avg_v;
    ) vec_map;

    let sorted =
      List.sort (fun (a,_) (b,_) -> CCString.compare a b) (StrTbl.to_list res_map)
    in

    CCFormat.printf "@[%a@]@." 
      (CCList.pp 
        ~pp_start:(CCFormat.return "{@.")
        ~pp_stop:(CCFormat.return "@.}")
        ~pp_sep:(CCFormat.return ",@.") (fun out (key, val_) ->  
      CCFormat.printf "  \"%s\" : %f" key val_
    )) sorted


  | CCResult.Error msg ->
    print_endline msg;
    exit 1

let main () =
  CCFormat.set_color_default true;
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "cnf_of_tptp [options] [file1|stdin] file2...";
  (if !files = [] then files := ["stdin"]);
  files := List.rev !files;
  List.iter process !files;
  begin match !Options.output with
  | Options.O_normal ->  ()
    (* Format.printf "%% @{<Green>success!@}@."; *)
  | _ -> ()
  end;
  ()

let _ =
  main ()
