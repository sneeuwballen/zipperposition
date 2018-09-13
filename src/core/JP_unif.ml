
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Jensen-Pietrzykowski Unification} *)

module T = Term

(* Find disagreeing subterms, TODO: preferably one that is not below a variable (to get preunification if possible) *)
let rec find_disagreement s t = 
  let rec find_disagreement_l ss tt = 
    match ss, tt with
      | [], [] -> None
      | s' :: ss', t' :: tt' -> 
        let d = find_disagreement s' t' in 
        if d = None then find_disagreement_l ss' tt' else d
      | _, _ -> raise (Invalid_argument "types of unified terms should be equal")
  in
  match T.view s, T.view t with
    | T.App (f, ss), T.App (g, tt) when f = g -> find_disagreement_l ss tt 
    | T.AppBuiltin (f, ss), T.AppBuiltin (g, tt) when f = g -> find_disagreement_l ss tt 
    | T.Var x, T.Var y when x = y -> None
    | T.DB i, T.DB j when i = j -> None
    | T.Const a, T.Const b when a = b -> None
    | T.Fun (ty_s, s'), T.Fun (ty_t, t') -> find_disagreement s' t' (*TODO: what about the types?*)
    | _ -> Some (s, t)


(* Projection rule (u, v is the disagreement pair) *)
let project u v = 
  match T.view u with
    | T.App (x, uu) when T.is_var x -> uu |> 
      List.map (fun u' -> 
        if Type.needs_args (T.ty u') then None
        else None (* TODO *)
      )
    (* TODO: AppBuildin?? *)
    | _ -> [None]