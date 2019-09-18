module T = Term

exception CoveringImpossible

let rec all_combs = function 
  | [] -> []
  | x::xs ->
      let rest_combs = all_combs xs in
      if CCList.is_empty rest_combs then CCList.map (fun t->[t]) x 
      else CCList.flat_map 
            (fun i -> CCList.map (fun comb -> i::comb) rest_combs) 
           x

let cover_rigid_skeleton t solids =
  assert(List.for_all T.is_ground solids);
  (* If the term is not of base type, then it must be a bound variable *)
  assert(List.for_all (fun t -> not @@ Type.is_fun @@ T.ty t || T.is_bvar t) solids);

  let n = List.length solids in
  let sols_as_db = List.mapi (fun i t -> (t,T.bvar ~ty:(T.ty t) (n-i-1))) solids in

  let rec aux t =
    (* All the ways in which we can represent term t using solids *)
    let db_hits = 
      CCList.filter_map (fun (s, s_db) -> 
        if T.equal s t then Some s_db else None) 
      sols_as_db in
    let rest = 
      try 
        match T.view t with
        | AppBuiltin (hd,args) ->
          if CCList.is_empty args then [T.app_builtin ~ty:(T.ty t) hd []]
          else (
            let args_combined = all_combs (List.map aux args) in
            List.map (fun args -> T.app_builtin ~ty:(T.ty t) hd args) args_combined
          )
        | App(hd,args) ->
          if Term.is_var hd then [t]
          else (
            assert(not (CCList.is_empty args));
            let hd, args = T.head_term_mono t, CCList.drop_while T.is_type args in
            let hd_args_combined = all_combs (aux hd :: (List.map aux args)) in
            List.map (fun l -> T.app (List.hd l) (List.tl l)) hd_args_combined
          )
        | Fun _ -> 
          let ty_args, body = T.open_fun t in
          let pref_len = List.length ty_args in
          let res = aux (T.DB.unshift pref_len body) in
          List.map (fun r -> T.fun_l ty_args (T.DB.shift pref_len r)) res
        | DB i when i >= 0 -> []
        | _ -> [t]
      with CoveringImpossible -> [] in
    if CCList.is_empty db_hits && CCList.is_empty rest 
    then raise CoveringImpossible
    else db_hits @ rest in
  
  try
    aux t
  with CoveringImpossible -> []

