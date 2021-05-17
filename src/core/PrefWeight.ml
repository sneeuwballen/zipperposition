
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Non-Perfect Discrimination Tree} *)

module T = Term

module type S = sig 
    val insert_term : Term.t -> unit
    val calc_pref_weight : Term.t -> int
end

let section = Util.Section.make "pref.weight"


type node_tag =
  | Var
  | Sym of ID.t
  | BT of Builtin.t

module NodeTagMap = Map.Make(
  struct 
  let tag_to_id = function
    | Var -> 0 | Sym _ -> 1 | BT _ -> 2
  
  type t = node_tag 
  let compare a b = 
    let c = compare (tag_to_id a) (tag_to_id b) in
    if c == 0 then (
      match (a,b) with
      | (Sym id, Sym id') -> ID.compare id id'
      | (BT b, BT b') -> Builtin.compare b b'
      | _ -> 0
    ) else c
   end
)

type trie =
  | Empty
  | Node of trie NodeTagMap.t
(** {2 Unix index} *)

module Make(P : sig 
  val match_weight : float
  val miss_weight : float
end) = struct
  let _trie = ref Empty

  let rec split_tag_args t =
    let remove_tys = List.filter (fun t -> not (T.is_type t)) in

    match T.view t with
    | Var _ -> (Var, [])
    | DB _ -> (Var, [])
    | Const id -> (Sym id, [])
    | App(hd, args) ->
      assert(T.is_const hd || T.is_var hd || T.is_bvar hd);
      if T.is_var hd || T.is_bvar hd then (Var, [])
      else (Sym (T.as_const_exn hd), remove_tys args)
    | AppBuiltin(hd, args) -> (BT hd, remove_tys args)
    | Fun(_,body) -> split_tag_args body

  let calc_fails ts = 
    let rec aux acc = function
    | x :: xs ->
      aux (1 + acc) (snd (split_tag_args x) @ xs)
    | [] -> acc
    in
    aux 0 ts
  
  let insert_term t =
    let rec aux node  = function
      | t :: ts ->
        let tag, args = split_tag_args t in
        let rest = args @ ts in
        begin match node with
        | Empty ->
          let subtree = aux Empty rest in
          Node (NodeTagMap.singleton tag subtree)
        | Node branches -> 
          let branch = CCOpt.get_or ~default:Empty 
                        (NodeTagMap.find_opt tag branches) in
          let subtree = aux branch rest in
          Node (NodeTagMap.add tag subtree branches) end
      | _ -> node 
    in
    _trie := aux !_trie [Lambda.eta_expand (Lambda.snf t)]
  
  let calc_pref_weight t =
    let rec aux matches node = function
    | [] -> 
      (matches, 0) 
    | t :: ts ->
      let tag, args = split_tag_args t in
      match node with
      | Empty -> (matches, 1 + (calc_fails (args @  ts)))
      | Node (branches) ->
        (match NodeTagMap.find_opt tag branches with
         | None ->  (matches, 1 + calc_fails (args @  ts))
         | Some subtree -> aux (matches+1) subtree (args @ ts))
    in
    let matches, fails = 
      CCPair.map_same (float_of_int)
        (aux 0 !_trie [Lambda.eta_expand (Lambda.snf t)]) in
    Util.debugf ~section 1 "(%a, %g, %g)" (fun k -> k T.pp t matches fails);
    int_of_float (P.match_weight *. matches +. P.miss_weight *. fails)
end
