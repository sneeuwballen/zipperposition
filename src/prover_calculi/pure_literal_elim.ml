open Logtk
open Libzipperposition

let section = Util.Section.make ~parent:Const.section "ple"

type id_sgn = ID.t * bool
module IDMap = CCMap.Make(struct
  type t = id_sgn
  let compare (id1, sgn1) (id2, sgn2) =
    let open CCOrd in
    ID.compare id1 id2
    <?> (CCBool.compare, sgn1, sgn2)
end)

module IntSet = Util.Int_set
module TST = TypedSTerm

exception AppVarFound

let _enabled = ref false
let total_clauses = Util.mk_stat "total_clauses"
let removed_clauses = Util.mk_stat "removed_clauses"

let pp_key = CCPair.pp ID.pp CCBool.pp

let cl_syms lits = 
  let lit_syms lit = 
    SLiteral.fold (fun acc t -> 
      ID.Set.union (ID.Set.of_iter (TST.Seq.symbols t)) acc)
    ID.Set.empty lit
  in
  List.fold_left (fun acc lit -> ID.Set.union acc (lit_syms lit)) ID.Set.empty lits

(* Computes a map (symbol, polarity) -> item, that is for each item we know how many times
   it occurs positive, how many times it occurs negative and for the clauses in
   which it occurs, what other symbols occur with what polarity *)
let compute_occurence_map (seq : (TST.t SLiteral.t list, TST.t, TST.t) Statement.t Iter.t) =

  (* given map of symbols to clauses in which they occur and a new clause
     in the form (cl_idx, literals) return updated map and converted clause *)
  let process_literals ids2clauses (cl_idx, lits) =

    (* This will compute the map (symbol, polarity) -> occurrence count
       for the list of literals *)
    let rec ocurrences map = 
      let inc_map hd sign map =
        let prev = IDMap.get_or (hd, sign) map ~default:0 in
        IDMap.add (hd,sign) (prev+1) map 
      in
      function
      | [] -> map
      | x :: xs ->
        try
          match x with
          | SLiteral.Atom(pred, sign) ->
            ocurrences (inc_map (TST.head_exn pred) sign map) xs
          | SLiteral.Neq(lhs,rhs)
          | SLiteral.Eq(lhs,rhs) when TST.Ty.is_prop (TST.ty_exn lhs) ->
            let (l_hd, r_hd) = CCPair.map_same TST.head_exn (lhs,rhs) in
            (* each symbol occurs once negative and once positive *)
            let inc_l = inc_map l_hd false (inc_map l_hd true map) in
            ocurrences (inc_map r_hd false (inc_map r_hd true inc_l)) xs
          | _ -> ocurrences map xs
        with Invalid_argument _ -> 
          (* invalid arg is raised if the head is not a symbol *)
          raise AppVarFound
    in

    let all_symbol_occurences = ocurrences IDMap.empty lits in

    let id_map' = 
      IDMap.keys all_symbol_occurences
      |> Iter.map fst
      |> ID.Set.of_iter
      |> (fun syms -> ID.Set.fold (fun sym acc -> 
        let prev = ID.Map.get_or sym acc ~default:IntSet.empty in
        ID.Map.add sym (IntSet.add cl_idx prev) acc
      ) syms ids2clauses)
    in
    id_map', all_symbol_occurences
  in

  Iter.fold (fun (forbidden, ids2clauses, clauses) (stmt : (TST.t SLiteral.t list, TST.t, TST.t) Statement.t) -> 
    match Statement.view stmt with
      (* Ignoring type declarations *)
    | Statement.TyDecl _ -> 
      forbidden, ids2clauses, clauses
    | Statement.Data _
    | Statement.Lemma _
    | Statement.Def _
    | Statement.Rewrite _ ->
      let f_syms = 
        Statement.Seq.forms stmt
        |> Iter.fold (fun acc lits -> ID.Set.union acc (cl_syms lits)) ID.Set.empty
      in
      (ID.Set.union f_syms forbidden), ids2clauses, clauses
    | Statement.Assert lits ->
      (* normal clause *)
      let ids2clauses', new_cl = 
        process_literals ids2clauses (List.length clauses, lits) in
      forbidden, ids2clauses', new_cl :: clauses
    | Statement.NegatedGoal (skolems,list_of_lits) ->
      (* clauses stemming from the negated goal *)
      let ids2clauses', clauses' = 
        List.fold_left (fun (ids2cls, cls) lits -> 
          let ids2cls', new_cl = 
            process_literals ids2cls (List.length cls, lits) in
          (ids2cls', new_cl :: cls)
        ) (ids2clauses, clauses) list_of_lits
      in
      forbidden, ids2clauses', clauses'
    | Statement.Goal lits -> 
      (* after CNFing a 'normal' problem all goals should 
         be negated and clausified *)
      failwith "Not implemented: Goal"
  ) (ID.Set.empty, ID.Map.empty, []) seq

let get_pure_symbols forbidden ids2clauses clauses =
  let calculate_pure init_pure cl_status all_clauses =
    let clauses = CCArray.of_list (List.rev clauses) in
    let rec aux processed symbol_occurences  = function
    | [] -> processed
    | (sym :: syms) as all_syms ->
      if ID.Set.mem sym processed || ID.Set.mem sym forbidden then (
        aux processed symbol_occurences syms
      ) else (
        let clauses_to_remove =
          CCList.filter_map (fun idx -> 
            if not (CCBV.get cl_status idx) then (
              CCBV.set cl_status idx;
              Some (clauses.(idx))
            )
            else None
          ) (IntSet.to_list @@ ID.Map.find sym ids2clauses)
        in
        let symbol_occurences', next_to_process =
          List.fold_left (fun (sym_occs, next_to_process) cl -> 
            IDMap.fold (fun key occ (sym_occs, next_to_process) ->
              let prev = IDMap.find key sym_occs in
              let new_ = prev-occ in
              let sym_occs' = IDMap.add key new_ sym_occs in
              if new_ = 0 && not (ID.Set.mem sym processed || ID.Set.mem sym forbidden
                                 || (CCList.mem ~eq:ID.equal (fst key) all_syms)) then (
                sym_occs', (fst key) :: next_to_process
              ) else (sym_occs', next_to_process)
            ) cl (sym_occs, next_to_process) 
          ) (symbol_occurences, []) clauses_to_remove
        in
        Util.debugf ~section 1 "became pure: @[%a@]@." (fun k -> k (CCList.pp ID.pp) next_to_process);
        aux (ID.Set.add sym processed) symbol_occurences' (next_to_process @ syms))
    in
    aux ID.Set.empty all_clauses (ID.Set.to_list init_pure)
  in


  (* joins all clauses in one map with occurences of symbol *)
  let all_clauses = 
    List.fold_left (fun acc cl -> 
      IDMap.union (fun _ a b -> CCOpt.return @@ a + b) acc cl
    ) IDMap.empty clauses
  in
  let all_symbols = 
    IDMap.keys all_clauses
    |> Iter.map fst
    |> ID.Set.of_iter
  in
  let init_pure = 
    ID.Set.filter (fun sym -> 
      not @@ ID.Set.mem sym forbidden &&
      (IDMap.get_or ~default:0 (sym, true) all_clauses == 0 ||
       IDMap.get_or ~default:0 (sym, false) all_clauses == 0)
    ) all_symbols
  in
  Util.debugf ~section 1 "initially pure: @[%a@]@." (fun k -> k (ID.Set.pp ID.pp) init_pure);
  let clause_status = CCBV.create ~size:(List.length clauses) false in
  calculate_pure init_pure clause_status all_clauses

let remove_pure_clauses (seq : (TST.t SLiteral.t list, TST.t, TST.t) Statement.t Iter.t) =
  let forbidden, ids2cls, cls = compute_occurence_map seq in

  let pure_syms = 
    ID.Set.diff
      (get_pure_symbols forbidden ids2cls cls)
    forbidden
  in

  let filter_if_has_pure stmt lits =
    Util.incr_stat total_clauses;
    let ans = 
      CCOpt.return_if 
        (ID.Set.is_empty @@ ID.Set.inter pure_syms (cl_syms lits)) 
      stmt
    in
    if CCOpt.is_none ans then (
      Util.incr_stat removed_clauses;
      Util.debugf ~section 2 "removed: @[%a@]@." 
        (fun k -> k (CCList.pp (SLiteral.pp TST.pp)) lits); 
    );
    ans
  in
  Iter.filter_map (fun stmt -> 
    match Statement.view stmt with
    | Statement.TyDecl _
    | Statement.Data _
    | Statement.Lemma _
    | Statement.Def _
    | Statement.Rewrite _ ->
      Some stmt
    | Statement.Assert lits ->
      (* normal clause *)
      filter_if_has_pure stmt lits
    | Statement.NegatedGoal (skolems,list_of_lits) ->
      (* clauses stemming from the negated goal *)
      let new_cls = 
        CCList.filter_map (fun x -> filter_if_has_pure x x) list_of_lits 
      in
      if CCList.is_empty new_cls then None
      else if List.length new_cls == List.length list_of_lits then Some stmt
      else (
        let new_stm =
          Statement.neg_goal
            ~attrs:(Statement.attrs stmt)
            ~proof:(Statement.proof_step stmt)
            ~skolems
            new_cls
        in
        Some (new_stm)
      )
    | Statement.Goal lits -> 
      (* after CNFing a 'normal' problem all goals should 
         be negated and clausified *)
      Some stmt
  ) seq

let extension =
  let modifier (seq : (TST.t SLiteral.t list, TST.t, TST.t) Statement.t Iter.t) = 
    if !_enabled then 
      begin 
        try 
          remove_pure_clauses seq
        with AppVarFound | Not_found ->
          seq
      end
    else seq in
  
  let print_stats _ =
    if !_enabled then 
      CCFormat.printf "%%%a@.%%%a@." Util.pp_stat removed_clauses Util.pp_stat total_clauses;
    
  in
  Extensions.(
    { default with name="pure_literal_elimination"; 
      post_cnf_modifiers=[modifier];
      env_actions=[print_stats]; }
  )

let () =
  Options.add_opts
    [ "--pure-literal-preprocessing", Arg.Bool (fun v -> _enabled := v), 
      " remove all pure literals in fixpoint"];
  Extensions.register extension