open Logtk

type id_sgn = ID.t * bool
module IDMap = CCMap.Make(struct
  type t = id_sgn
  let compare (id1, sgn1) (id2, sgn2) =
    let open CCOrd in
    ID.compare id1 id2
    <?> (Bool.compare, sgn1, sgn2)
end)

module IntSet = Util.Int_set

exception AppVarFound



(* Computes a map symbol -> item, that is for each item we know how many times
   it occurs positive, how many times it occurs negative and for the clauses in
   which it occurs, what other symbols occur with what polarity *)
let compute_occurence_map seq =

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
            let pred = Lambda.snf pred in
            let hd = Term.head_exn pred in
            inc_map hd sign map
          | SLiteral.Neq(lhs,rhs)
          | SLiteral.Eq(lhs,rhs) when Type.is_prop (Term.ty lhs) ->
            let (lhs, rhs) = CCPair.map_same Lambda.snf (lhs,rhs) in
            let (l_hd, r_hd) = CCPair.map_same Term.head_exn (lhs,rhs) in
            (* each symbol occurs once negative and once positive *)
            let inc_l = inc_map l_hd false (inc_map l_hd true map) in
            inc_map r_hd false (inc_map r_hd true inc_l)
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

  Iter.fold (fun (forbidden, ids2clauses, clauses) stmt -> 
    match Statement.view stmt with
      (* Ignoring type declarations *)
    | Statement.TyDecl _ -> 
      forbidden, ids2clauses, clauses
    | Statement.Data _
    | Statement.Lemma _
    | Statement.Def _
    | Statement.Rewrite _ ->
      (* those are statements (not clauses!) that are treated specially by
         Zip -- and will definitely occur in both polarities,
         so we forbid their removal *)
      let f = Statement.Seq.symbols stmt in
      (ID.Set.union (ID.Set.of_iter f) forbidden), ids2clauses, clauses
    | Statement.Assert lits ->
      (* normal clause *)
      let ids2clauses', new_cl = 
        process_literals ids2clauses (List.length clauses, lits) in
      forbidden, ids2clauses, new_cl :: clauses
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
    | sym :: syms ->
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
              if new_ = 0 && not (ID.Set.mem sym processed || ID.Set.mem sym forbidden) then (
                sym_occs', (fst key) :: next_to_process
              ) else (sym_occs', next_to_process)
            ) cl (sym_occs, next_to_process) 
          ) (symbol_occurences, []) clauses_to_remove
        in
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
      (IDMap.find (sym, true) all_clauses == 0 ||
       IDMap.find (sym, false) all_clauses == 0)
    ) all_symbols
  in
  let clause_status = CCBV.create ~size:(List.length clauses) false in
  calculate_pure init_pure clause_status all_clauses

let remove_pure_clauses seq =
  let cl_syms lits = 
    let lit_syms lit = 
      SLiteral.fold (fun acc t -> 
        ID.Set.union (ID.Set.of_iter (Term.Seq.symbols t)) acc)
      ID.Set.empty lit
    in
    List.fold_left (fun acc lit -> ID.Set.union acc (lit_syms lit)) ID.Set.empty lits
  in

  let forbidden, ids2cls, cls = compute_occurence_map seq in
  let pure_syms = 
    ID.Set.diff
      (get_pure_symbols forbidden ids2cls cls)
    forbidden
  in
  let filter_if_has_pure stmt lits =
    CCOpt.return_if 
      (not @@ ID.Set.is_empty @@ ID.Set.inter pure_syms (cl_syms lits)) 
    stmt
  in
  Iter.filter_map (fun stmt -> 
    match Statement.view stmt with
    | Statement.TyDecl _
    | Statement.Data _
    | Statement.Lemma _
    | Statement.Def _
    | Statement.Rewrite _ ->
      None
    | Statement.Assert lits ->
      (* normal clause *)
      filter_if_has_pure stmt lits
    | Statement.NegatedGoal (skolems,list_of_lits) ->
      (* clauses stemming from the negated goal *)
      let new_cls = 
        List.filter_map (fun x -> filter_if_has_pure x x) list_of_lits 
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