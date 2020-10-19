
open Logtk

module Lit = Literal
module Pos = Position
module PB = Pos.Build
module BIn = Builtin
module T = Term

(** As described in FBoolSup paper, Boolean selection function
    selects positions in the clause that are non-interpreted 
    Boolean subterms. *)

type t = Literal.t array -> (Term.t * Pos.t) list

type parametrized = strict:bool -> ord:Ordering.t -> t

(* Zipperposition interprets argument positions
   inverted, so we need to convert
   them before calling PositionBuilder *)
let inv_idx args idx = 
    List.length args - idx - 1

let is_selectable ~forbidden ~top t =
  let module VS = T.VarSet in
  Type.is_prop (T.ty t) && 
  not top && 
  not (T.is_var t) && not (T.is_true_or_false t) &&
  (
    T.is_ground t || (* avoid computing the set of vars for t *)
    VS.is_empty forbidden || (* avoid computing the set of vars for t *)
    VS.is_empty (VS.inter (VS.of_iter (T.Seq.vars t)) forbidden)
  )

let get_forbidden_vars ~ord lits =
  (* Check the HOSup paper for the definition of forbidden vars *)
  Literals.maxlits_l ~ord lits 
  |> CCList.map (fun (l,_) -> l) 
  |> Array.of_list
  |> CCArray.fold (
      fun vars lit ->
        Lit.fold_terms ~vars:true ~ty_args:false
                       ~which:`Max ~ord ~subterms:false lit
        |> Iter.filter_map (fun (t,_) -> (T.as_var @@ T.head_term t))
        |> T.VarSet.of_iter
        |> T.VarSet.union vars
    ) T.VarSet.empty

let select_leftmost ~ord ~kind lits =
  let open CCOpt in
  let forbidden = get_forbidden_vars ~ord lits in

  let rec aux_lits idx =
    if idx >= Array.length lits then None
    else (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation(lhs,rhs,_) ->
        let in_literal = 
          match Ordering.compare ord lhs rhs with 
          | Comparison.Lt ->
            (aux_term ~top:true ~pos_builder:(PB.right pos_builder) rhs)
          | Comparison.Gt ->
            (aux_term ~top:true ~pos_builder:(PB.left pos_builder) lhs)
          | _ ->
            (aux_term ~top:true ~pos_builder:(PB.left pos_builder) lhs)
              <+>
            (aux_term ~top:true ~pos_builder:(PB.right pos_builder) rhs)
        in
        in_literal <+> (aux_lits (idx+1))
      | _ -> aux_lits (idx+1))
  and aux_term ~top ~pos_builder t =
    (* if t is app_var then return the term itself, but do not recurse  *)
    if T.is_app_var t then (
      CCOpt.return_if (is_selectable ~top ~forbidden t) (t, PB.to_pos pos_builder)
    ) else (
      match T.view t with
      | T.App(_, args)
      | T.AppBuiltin(_, args) ->
        (* reversing the argument indices *)
        let inner = aux_term_args args ~idx:(List.length args - 1) ~pos_builder in
        let outer =
          if not (is_selectable ~top ~forbidden t) then None
          else Some (t, PB.to_pos pos_builder) 
        in
        if kind == `Inner then inner <+> outer
        else outer <+> inner
      | T.Const _ when is_selectable ~top ~forbidden t -> Some (t, PB.to_pos pos_builder)
      | _ -> None
    )
  and aux_term_args ~idx ~pos_builder = function
  | [] -> None
  | x :: xs ->
    assert (idx >= 0);
    (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x)
      <+>
    (aux_term_args ~idx:(idx-1) ~pos_builder xs) 
  in
  CCOpt.map_or ~default:[] (fun x -> [x]) (aux_lits 0)

let collect_green_subterms_ ~forbidden ~filter ~ord ~pos_builder t k = 
  let rec aux_term ~top ~pos_builder t k =
    if filter ~forbidden ~top t then (k (t, PB.to_pos pos_builder));

    if T.is_app_var t then ( (* only green subterms are eligible *) )
    else (
      match T.view t with
      | T.AppBuiltin((BIn.Eq|BIn.Neq|BIn.Xor|BIn.Equiv),( ([a;b] | [_;a;b]) as l)) 
          when Type.is_prop (T.ty a) ->
        (* only going to the larger side of the (dis)equation *)
        let offset = List.length l - 2 in (*skipping possible tyarg*)
        (match Ordering.compare ord a b with 
          | Comparison.Lt ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b k
          | Comparison.Gt ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a k
          | _ ->
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l (1+offset)) pos_builder) b k;
            aux_term ~top:false ~pos_builder:(PB.arg (inv_idx l offset) pos_builder) a k)
      | T.App(_, args)
      | T.AppBuiltin(_, args)->
        aux_term_args ~idx:(List.length args - 1) ~pos_builder args k
      | _ -> ())
  and aux_term_args ~idx ~pos_builder args k = 
    match args with
    | [] -> ()
    | x :: xs ->
      assert (idx >= 0);
      (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x k);
      (aux_term_args ~idx:(idx-1) ~pos_builder xs k) in
  aux_term ~top:true ~pos_builder t k

let all_selectable_subterms ?(forbidden=T.VarSet.empty) ~ord ~pos_builder =  
  collect_green_subterms_ ~forbidden ~filter:is_selectable ~ord ~pos_builder

let all_eligible_subterms ~ord ~pos_builder =
  (* make sure you select only subterms *)
  let filter ~forbidden ~top t  = 
    not top &&
    not (Type.is_tType (T.ty t)) in
  collect_green_subterms_ ~forbidden:(T.VarSet.empty) ~filter ~ord ~pos_builder 

let get_all_selectable ~ord lits = 
  let open CCOpt in
  
  let forbidden = get_forbidden_vars ~ord lits in

  let rec aux_lits idx k =
    if idx < Array.length lits then (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation (lhs,rhs,_) ->
        begin match Ordering.compare ord lhs rhs with
          | Comparison.Lt ->
            all_selectable_subterms ~ord ~forbidden ~pos_builder:(PB.right pos_builder) rhs k
          | Comparison.Gt ->
            all_selectable_subterms ~ord ~forbidden ~pos_builder:(PB.left pos_builder) lhs k
          | _ ->
            all_selectable_subterms ~ord ~forbidden ~pos_builder:(PB.left pos_builder) lhs k;
            all_selectable_subterms ~ord ~forbidden ~pos_builder:(PB.right pos_builder) rhs k
        end;
        aux_lits (idx+1) k
      | _ -> aux_lits (idx+1) k)
  in
  aux_lits 0

let by_size ~ord ~kind lits =
  let selector =
    if kind = `Max then Iter.max else Iter.min in
  get_all_selectable ~ord lits
  |> selector ~lt:(fun (s,_) (t,_) -> Term.ho_weight s < Term.ho_weight t)
  |> CCOpt.map_or ~default:[] (fun x -> [x])


let leftmost_innermost ~ord lits =
  select_leftmost ~ord ~kind:`Inner lits
let leftmost_outermost ~ord lits =
  select_leftmost ~ord ~kind:`Outer lits
let smallest ~ord lits =
  by_size ~ord ~kind:`Min lits
let largest ~ord lits =
  by_size ~ord ~kind:`Max lits

let none ~ord lits = []

let fun_names = 
  [ ("LI", leftmost_innermost);
    ("LO", leftmost_outermost);
    ("smallest", smallest);
    ("largest", largest);
    ("none", none) ]

let from_string ~ord name =
  try 
    (List.assoc name fun_names) ~ord
  with _ ->
    invalid_arg (name ^ " is not a valid bool selection name")

let all =
  let names_only = List.map fst fun_names in
  fun () -> names_only

let () =
  let set_bselect s = Params.bool_select := s in
  Params.add_opts
    [ "--bool-select", Arg.Symbol (all(), set_bselect), " set boolean literal selection function"];