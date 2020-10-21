
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

  type logic = 
    | NEqFO  (* nonequational FO *)
    | EqFO (* equational FO *)
    | Unsupported   (* HO or FO with theories *)

  exception UnsupportedLogic
  
  type pred_elim_info =
  {
    sym : ID.t;
    (* clauses with single pos/neg occurrence of a symbol *)
    mutable pos_cls : C.ClauseSet.t;
    mutable neg_cls : C.ClauseSet.t;
    (* clauses with multiple occurrences of a symbol *)
    mutable offending_cls : C.ClauseSet.t;
    (* clauses that have the gate shape (occurs in pos/neg cls)  *)
    mutable possible_gates : C.ClauseSet.t;
    (* do the clauses in the possible_gates form a gate, and if so which one? *)
    mutable is_gate : (C.t list * C.t list) option;
    (* max and min number of variables in clauses in any of the sets *)
    mutable max_vars : int;
    mutable min_vars : int;
    (* what was this number during the last check *)
    mutable last_check : (int * int) option;
    mutable heap_idx : int;
  }

  module TaskWrapper = struct
    type t = pred_elim_info
    let idx task = task.heap_idx
    let set_idx task idx = 
      task.heap_idx <- idx
    let lt a b =
      let card t = 
        C.ClauseSet.cardinal t.pos_cls + C.ClauseSet.cardinal t.neg_cls
      in
      card a < card b || (card a = card b && ID.compare a.sym b.sym < 0)
  end

  module TaskQueue = struct 
    include CCMutHeap.Make(TaskWrapper)

    let delete q t =
      filter q (fun t' -> ID.equal t.sym t'.sym)
  end

  let _task_queue = TaskQueue.create ()

  let _logic = ref NEqFO
  let refine_logic new_val =
    if !_logic != Unsupported then (
      _logic := new_val
    )
  
  let _ignored_symbols = ref ID.Set.empty

  let mk_pred_elim_info sym =
    {
      sym; pos_cls = C.ClauseSet.empty; neg_cls = C.ClauseSet.empty;
      offending_cls=C.ClauseSet.empty; possible_gates = C.ClauseSet.empty;
      is_gate = None; max_vars = 0; min_vars = 0; last_check = None;
      heap_idx = -1;
    }
  
  let _pred_sym_idx = ref ID.Map.empty

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

  let scan_cl_lits cl =
    let num_vars = List.length @@ Literals.vars (C.lits cl) in
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
          let entry = CCOpt.get_or ~default:(mk_pred_elim_info sym) old in
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
        if !is_offending then (
          pos,neg,ID.Set.add sym offending,gates
        ) else (
          let gates = if is_flat lit then ID.Set.add sym gates else gates in
          if sign then (ID.Set.add sym pos,neg,offending,gates)
          else (pos, ID.Set.add sym neg,offending,gates))
      | _ -> acc)
    ) (ID.Set.empty, ID.Set.empty, ID.Set.empty, ID.Set.empty) (C.lits cl) in
    
    update_idx pos neg offending gates num_vars cl

  let check_if_gate task =
    let sym = task.sym in
    let gates_l = C.ClauseSet.to_list task.possible_gates in
    let filter_gates ~sign ~lit_num_filter =
      List.filter (fun cl -> 
        lit_num_filter (Array.length (C.lits cl)) &&
        Array.exists (fun lit -> 
          match get_sym_sign lit with 
          | Some(sym', sign') -> ID.equal sym sym' && sign = sign'
          | None -> false) 
        (C.lits cl)
      ) gates_l
    in

    let find_and_or bin_clauses long_clauses =
      (* both AND and OR definitions are of the form (~)name \/ literals
         and a bunch of binary clauses (~)name \/ lit   *)
      CCList.find_map (fun long_cl ->
        let sym_lits, other_lits = List.partition (fun lit ->
          match get_sym_sign lit with
          | Some(sym',_) -> ID.equal sym sym'
          | _ -> false
        ) (CCArray.to_list @@ C.lits long_cl) in
        assert (List.length sym_lits = 1);
        let sym_name_lit = List.hd sym_lits in
        let bin_gates = CCArray.of_list bin_clauses in
        if List.length other_lits > CCArray.length bin_gates then None
        else (
          (* bit vector remembering which binary clauses we have already used *)
          let matched = CCBV.create ~size:(CCArray.length bin_gates) false in
          let is_gate = List.for_all (fun lit ->
            let found = ref false in
            let i = ref 0 in
            while not !found && !i < CCArray.length bin_gates do
              let cl = bin_gates.(!i) in
              if not (CCBV.get matched !i) then (
                let idx_name_opt = CCArray.find_idx (fun lit ->
                  match get_sym_sign lit with
                  | Some(sym',_) -> ID.equal sym sym'
                  | None -> false
                ) (C.lits cl) in
                let idx_name, _ = CCOpt.get_exn idx_name_opt in
                let name_lit = L.negate (C.lits cl).(idx_name) in
                let other_lit = L.negate (C.lits cl).(1 - idx_name) in
                let is_matched = 
                  not @@ Iter.is_empty
                    (L.variant (lit,0) (other_lit,1)
                    |> Iter.filter (fun (subst,_) -> 
                        not @@ Iter.is_empty @@ 
                          L.variant ~subst (sym_name_lit, 0) (name_lit,1)))
                in
                if is_matched then (
                  CCBV.set matched !i;
                  found := true
                ))
            done;
            !found
          ) other_lits in
          let bin_cls = CCBV.select matched bin_gates in
          if is_gate then Some(long_cl, bin_cls)
          else None))
      long_clauses
    in

    let check_and () =
      let pos_gates = filter_gates ~sign:true ~lit_num_filter:(fun n -> n >= 3) in
      let neg_gates = filter_gates ~sign:false ~lit_num_filter:((=) 2) in
      match find_and_or neg_gates pos_gates with
      | Some (pos_cl, neg_cls) -> 
        task.is_gate <- Some([pos_cl], neg_cls);
        true
      | None -> false
    in
    let check_or () =
      let pos_gates = filter_gates ~sign:true ~lit_num_filter:((=) 2) in
      let neg_gates = filter_gates ~sign:false ~lit_num_filter:(fun n -> n >= 3) in
      match find_and_or pos_gates neg_gates with
      | Some(neg_cl, pos_cls) ->
        task.is_gate <- Some(pos_cls, [neg_cl]);
        true
      | None -> false
    in
    (* not yet implemented *)
    let check_ite () = false in
    ignore (check_and () || check_or () || check_ite ())

  let schedule_tasks () =
    ID.Map.iter (fun _ task -> 
      check_if_gate task;
      if C.ClauseSet.is_empty task.offending_cls then (
        TaskQueue.insert _task_queue task;
      )
    ) !_pred_sym_idx
    
  let do_pred_elim () = ()

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
      schedule_tasks ();
      do_pred_elim ();

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