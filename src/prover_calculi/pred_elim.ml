
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "pred-elim"

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module L = Literal
  module T = Term

  type gate_kind = And | Or | Ite

  type logic = 
    | NEqFO  (* nonequational FO *)
    | EqFO (* equational FO *)
    | Unsupported   (* HO or FO with theories *)

  exception UnsupportedLogic
  
  type pred_elim_info =
  {
    (* clauses with single pos/neg occurrence of a symbol *)
    mutable pos_cls : C.ClauseSet.t;
    mutable neg_cls : C.ClauseSet.t;
    (* clauses with multiple occurrences of a symbol *)
    mutable offending_cls : C.ClauseSet.t;
    (* clauses that have the gate shape (occurs in pos/neg cls)  *)
    mutable possible_gates : C.ClauseSet.t;
    (* do the clauses in the possible_gates form a gate, and if so which one? *)
    mutable is_gate : gate_kind option;
    (* max and min number of variables in clauses in any of the sets *)
    mutable max_vars : int;
    mutable min_vars : int;
    (* what was this number during the last check *)
    mutable last_check : (int * int) option;
  }

  let _logic = ref NEqFO
  let refine_logic new_val =
    if !_logic != Unsupported then (
      _logic := new_val
    )
  
  let _ignored_symbols = ref ID.Set.empty

  let mk_pred_elim_info () =
    {
      pos_cls = C.ClauseSet.empty; neg_cls = C.ClauseSet.empty;
      offending_cls=C.ClauseSet.empty; possible_gates = C.ClauseSet.empty;
      is_gate = None; max_vars = 0; min_vars = 0; last_check = None
    }
  
  let _pred_sym_idx = ref ID.Map.empty

  
  (* Scan the clause and if it is in supported logic fragment,
     store its literals in the symbol index *)
  let scan_cl_lits cl =
    let num_vars = List.length @@ Literals.vars (C.lits cl) in
    let get_sym_sign lit =
      match lit with
      | L.Equation(lhs,rhs,_) ->
        let sign = L.is_pos lit in
        let is_poly = 
          not (Type.VarSet.is_empty (T.ty_vars lhs))
          || not (Type.VarSet.is_empty (T.ty_vars rhs))
        in
        if not is_poly && T.is_fo_term lhs && T.is_fo_term rhs then (
          if Type.is_prop (T.ty lhs) then (
            if L.is_predicate_lit lit then (
              let hd_sym = T.head_exn lhs in
              Some (hd_sym, sign)
            ) else (
              (* reasoning with formulas is currently unsupported *)
              Util.debugf ~section 1 "unsupported because of @[%a@]@." (fun k -> k L.pp lit);
              _logic := Unsupported;
              raise UnsupportedLogic;
          )) else (refine_logic EqFO; None)
        ) else (
          _logic := Unsupported; 
          Util.debugf ~section 1 "unsupported because of @[%a@]@." (fun k -> k L.pp lit);
          raise UnsupportedLogic)
      | L.Int _ | L.Rat _  -> 
        Util.debugf ~section 1 "theories are not supported@." CCFun.id;
        _logic := Unsupported;
        raise UnsupportedLogic
      | _ -> None
    in

    let is_flat = function
    | L.Equation(lhs,_,_) as lit ->
      assert (L.is_predicate_lit lit);
      let args = T.args lhs in
      List.for_all T.is_var args &&
      T.Set.cardinal (T.Set.of_list args) == List.length args
    | _ -> assert false
    in

    let update_idx pos neg offending gates num_vars cl =
      let update ~action sym =
        _pred_sym_idx := ID.Map.update sym (fun old ->
          let entry = CCOpt.get_or ~default:(mk_pred_elim_info ()) old in
          begin match action with
          | `Pos ->
            entry.pos_cls <- C.ClauseSet.add cl entry.pos_cls
          | `Neg ->
            entry.neg_cls <- C.ClauseSet.add cl entry.neg_cls
          | `Offending ->
            entry.offending_cls <- C.ClauseSet.add cl entry.offending_cls
          | `Gates -> 
            entry.possible_gates <- C.ClauseSet.add cl entry.possible_gates end;
          entry.max_vars <- max entry.max_vars num_vars;
          entry.min_vars <- max entry.min_vars num_vars;
          Some entry
        ) !_pred_sym_idx
      in
      ID.Set.iter (update ~action:`Pos) pos;
      ID.Set.iter (update ~action:`Neg) neg;
      ID.Set.iter (update ~action:`Offending) offending;
      ID.Set.iter (update ~action:`Gates) gates
    in

      
    let pos,neg,offending,gates = CCArray.foldi (fun ((pos,neg,offending,gates) as acc) idx lit ->
      let symbol_is_fresh sym =
        not (ID.Set.mem sym pos) && not (ID.Set.mem sym neg) &&
        not (ID.Set.mem sym offending) && not (ID.Set.mem sym !_ignored_symbols)
      in
      (match get_sym_sign lit with
      | Some (sym, sign) when symbol_is_fresh sym ->
        let is_offending = ref false in
        for i = idx+1 to (C.length cl) do
          (match get_sym_sign (C.lits cl).(i) with
           | Some(sym', _) ->
             is_offending := !is_offending || ID.equal sym sym'
           | None -> () )
        done;
        let gates = if is_flat lit then ID.Set.add sym gates else gates in
        if !is_offending then (
          pos,neg,ID.Set.add sym offending,gates
        ) else if sign then (
          ID.Set.add sym pos,neg,offending,gates
        ) else (pos, ID.Set.add sym neg,offending,gates)
      | _ -> acc)
    ) (ID.Set.empty, ID.Set.empty, ID.Set.empty, ID.Set.empty) (C.lits cl) in
    
    update_idx pos neg offending gates num_vars cl

  let initialize () =
    let init_clauses =
      C.ClauseSet.to_list (Env.ProofState.ActiveSet.clauses ())
      @ C.ClauseSet.to_list (Env.ProofState.PassiveSet.clauses ())
    in
    begin try
      Util.debugf ~section 2 "init_cl: @[%a@]@."
        (fun k -> k (CCList.pp C.pp) init_clauses);
      
      (* build the symbol index *)
      List.iter scan_cl_lits init_clauses;

      Util.debugf ~section 1 "logic has%sequalities"
        (fun k -> k (if !_logic == EqFO then " " else " no "));
    with UnsupportedLogic ->
      Util.debugf ~section 1 "logic is unsupported" CCFun.id;
      (* releasing possibly used memory *)
      _pred_sym_idx := ID.Map.empty
    end;
    Signal.StopListening

  let register () =
    Signal.on Env.on_start initialize


  
  let setup () = ()
end

let _enabled = ref false

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module PredElim = Make(E) in

    E.flex_add k_enabled !_enabled;
    
    PredElim.setup ()
  in
  { Extensions.default with Extensions.
                         name="pred_elim";
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--pred-elim", Arg.Bool ((:=) _enabled), " scan clauses for AC definitions";
  ]