open Logtk

type id_sgn = ID.t * bool
module IDMap = CCMap.Make(struct
  type t = id_sgn
  let compare (id1, sgn1) (id2, sgn2) =
    let open CCOrd in
    ID.compare id1 id2
    <?> (Bool.compare, sgn1, sgn2)
end)

type item = {
  pos_cnt : int;
  neg_cnt : int;
  other_occurences : int IDMap.t
}

exception AppVarFound


(* Computes a map symbol -> item, that is for each item we know how many times
   it occurs positive, how many times it occurs negative and for the clauses in
   which it occurs, what other symbols occur with what polarity *)
let compute_occurence_map seq =
  let process_literals id_map lits =

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

    IDMap.keys all_symbol_occurences
    |> Iter.map fst
    |> ID.Set.of_iter 
    |> (fun syms -> ID.Set.fold (fun sym acc -> 
      let pos_occ, neg_occ =
        IDMap.get_or (sym, true) all_symbol_occurences ~default:0,
        IDMap.get_or (sym, false) all_symbol_occurences ~default:0
      in
      let others = 
        IDMap.filter (fun (sym', _) _ -> not @@ ID.equal sym sym') all_symbol_occurences in
      let default = {pos_cnt=0; neg_cnt=0; other_occurences=IDMap.empty} in
      let prev = ID.Map.get_or sym id_map ~default in
      ID.Map.add sym {
          pos_cnt=prev.pos_cnt+pos_occ; 
          neg_cnt = prev.neg_cnt+neg_occ;
          other_occurences = 
            IDMap.union (fun sym old new_ -> Some (old + new_))
              prev.other_occurences others     
          } id_map
    ) syms id_map)
  in

  Iter.fold (fun (forbidden, id_map) stmt -> 
    match Statement.view stmt with
      (* Ignoring type declarations *)
    | Statement.TyDecl _ -> 
      forbidden, id_map
    | Statement.Data _
    | Statement.Lemma _
    | Statement.Def _
    | Statement.Rewrite _ ->
      (* those are statements (not clauses!) that are treated specially by
         Zip -- and will definitely occur in both polarities,
         so we forbid their removal *)
      let f = Statement.Seq.symbols stmt in
      (ID.Set.union (ID.Set.of_iter f) forbidden), id_map
    | Statement.Assert lits ->
      (* normal clause *)
      forbidden, process_literals id_map lits
    | Statement.NegatedGoal (skolems,clauses) -> 
      (* clauses stemming from the negated goal *)
      forbidden, List.fold_left (fun acc lits -> process_literals acc lits) id_map clauses 
    | Statement.Goal lits -> 
      (* after CNFing a 'normal' problem all goals should 
         be negated and clausified *)
      failwith "Not implemented: Goal"
  ) (ID.Set.empty, ID.Map.empty) seq
