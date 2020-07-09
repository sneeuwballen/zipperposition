
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

let can_be_selected ~top t = 
  Type.is_prop (T.ty t) && not top && 
  not (T.is_var t) && not (T.is_app_var t) &&
  not (T.is_true_or_false t)

let select_leftmost ~kind lits =
  let open CCOpt in
  
  let rec aux_lits idx =
    if idx >= Array.length lits then None
    else (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation(lhs,rhs,_) ->
        (aux_term ~top:true ~pos_builder:(PB.left pos_builder) lhs)
          <+>
        (aux_term ~top:true ~pos_builder:(PB.right pos_builder) rhs)
          <+>
        (aux_lits (idx+1))
      | _ -> aux_lits (idx+1))
  and aux_term ~top ~pos_builder t =
    match T.view t with
    | T.App(_, args)
    | T.AppBuiltin(_, args) ->
      let in_args = aux_term_args args ~idx:0 ~pos_builder in
      if CCOpt.is_some in_args && kind == `Inner then in_args
      else if can_be_selected ~top t then (Some (t, PB.to_pos pos_builder))
      else None
    | T.Const _ when can_be_selected ~top t-> Some (t, PB.to_pos pos_builder)
    | _ -> None
  and aux_term_args ~idx ~pos_builder = function
  | [] -> None
  | x :: xs ->
    (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x)
      <+>
    (aux_term_args ~idx:(idx+1) ~pos_builder xs) 
  in
  CCOpt.map_or ~default:[] (fun x -> [x]) (aux_lits 0)

let all_selectable_subterterms ~pos_builder t k =
  let rec aux_term ~top ~pos_builder t k =
    if can_be_selected top t then (k (t, PB.to_pos pos_builder));

    match T.view t with
    | T.App(_, args) ->
      aux_term_args ~idx:0 ~pos_builder args k
    | T.AppBuiltin(hd, args) when CCList.mem ~eq:BIn.equal hd [Eq; Neq; Xor; Equiv]->
      aux_term_args ~idx:0 ~pos_builder args k
    | _ -> ()
  and aux_term_args ~idx ~pos_builder args k = 
    match args with
    | [] -> ()
    | x :: xs ->
      (aux_term ~top:false ~pos_builder:(PB.arg idx pos_builder) x k);
      (aux_term_args ~idx:(idx+1) ~pos_builder xs k) in
  aux_term ~top:true ~pos_builder t k

let get_all_selectable lits = 
  let open CCOpt in
  
  let rec aux_lits idx k =
    if idx < Array.length lits then (
      let pos_builder = PB.arg idx (PB.empty) in
      match lits.(idx) with
      | Lit.Equation (lhs,rhs,_) ->
        all_selectable_subterterms ~pos_builder:(PB.left pos_builder) lhs k;
        all_selectable_subterterms ~pos_builder:(PB.right pos_builder) rhs k;
        aux_lits (idx+1) k
      | _ -> aux_lits (idx+1) k
    )
    in
  aux_lits 0

let by_size ~kind lits =
  let selector =
    if kind = `Max then Iter.max else Iter.min in
  get_all_selectable lits
  |> selector ~lt:(fun (s,_) (t,_) -> Term.ho_weight s < Term.ho_weight t)
  |> CCOpt.map_or ~default:[] (fun x -> [x])


let leftmost_innermost ~ord lits =
  select_leftmost ~kind:`Inner lits
let leftmost_outermost ~ord lits =
  select_leftmost ~kind:`Outer lits
let smallest ~ord lits =
  by_size ~kind:`Min lits
let largest ~ord lits =
  by_size ~kind:`Max lits

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