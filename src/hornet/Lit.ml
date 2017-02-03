
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

open Libzipperposition

module Fmt = CCFormat
module T = FOTerm
module P = Position
module PW = Position.With

type ty = Type.t
type term = T.t

type t =
  | Bool of bool
  | Atom of term * bool
  | Eq of term * term * bool
type lit = t

let true_ = Bool true
let false_ = Bool false
let bool b = Bool b
let atom ?(sign=true) t = Atom (t, sign)

let eq ?(sign=true) t u =
  if T.equal t u then bool sign
  else (
    (* canonical order *)
    let left, right = if T.compare t u < 0 then t, u else u, t in
    Eq (left, right, sign)
  )

let sign = function
  | Atom (_, b)
  | Eq (_,_,b)
  | Bool b -> b

let equal a b: bool = match a, b with
  | Bool b1, Bool b2 -> b1=b2
  | Atom (t1,sign1), Atom (t2,sign2) -> T.equal t1 t2 && sign1=sign2
  | Eq (t1,u1,sign1), Eq (t2,u2,sign2) ->
    sign1=sign2 &&
    T.equal t1 t2 && T.equal u1 u2
  | Bool _, _
  | Atom _, _
  | Eq _, _
    -> false

let hash = function
  | Bool b -> Hash.combine2 10 (Hash.bool b)
  | Atom (t,sign) -> Hash.combine3 20 (T.hash t) (Hash.bool sign)
  | Eq (t,u,sign) -> Hash.combine4 30 (T.hash t) (T.hash u) (Hash.bool sign)

let compare a b: int =
  let to_int = function Bool _ -> 0 | Atom _ -> 1 | Eq _ -> 2 in
  begin match a, b with
    | Bool b1, Bool b2 -> CCOrd.bool_ b1 b2
    | Atom (t1,sign1), Atom (t2,sign2) ->
      CCOrd.( T.compare t1 t2 <?> (bool_, sign1, sign2))
    | Eq (t1,u1,sign1), Eq (t2,u2,sign2) ->
      CCOrd.( T.compare t1 t2
        <?> (T.compare, u1, u2)
        <?> (bool_, sign1, sign2))
    | Bool _, _
    | Atom _, _
    | Eq _, _
      -> CCInt.compare (to_int a)(to_int b)
  end

let pp out t: unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t
  | Eq (t,u,true) -> Fmt.fprintf out "@[%a@ = %a@]" T.pp t T.pp u
  | Eq (t,u,false) -> Fmt.fprintf out "@[%a@ @<1>≠ %a@]" T.pp t T.pp u

let to_string = Fmt.to_string pp

let vars_seq = function
  | Bool _ -> Sequence.empty
  | Atom (t,_) -> T.Seq.vars t
  | Eq (t,u,_) -> Sequence.append (T.Seq.vars t) (T.Seq.vars u)

let vars_list l = vars_seq l |> Sequence.to_rev_list

let vars_set l =
  vars_seq l
  |> Sequence.to_rev_list
  |> CCList.sort_uniq ~cmp:(HVar.compare Type.compare)

let weight = function
  | Bool _ -> 0
  | Atom (t, _) -> T.weight t
  | Eq (t,u,_) -> T.weight t + T.weight u

(** {2 Positions} *)

module With_pos = struct
  type t = lit Position.With.t

  let pp = PW.pp pp
  let compare = PW.compare compare
  let to_string = Fmt.to_string pp
end

let direction ord = function
  | Bool _ -> None
  | Atom _ -> None
  | Eq (t,u,_) -> Ordering.compare ord t u |> CCOpt.return

let at_pos_exn pos lit = match lit, pos with
  | Bool b, P.Stop -> if b then T.true_ else T.false_
  | Atom (t,_), _ -> T.Pos.at t pos
  | Eq (t,_,_), P.Left pos' -> T.Pos.at t pos'
  | Eq (_,u,_), P.Right pos' -> T.Pos.at u pos'
  | _, _ -> raise Not_found

let active_terms ord lit =
  let yield_term t pos = PW.make t pos in
  match lit with
  | Atom (t,true) ->
    T.all_positions ~vars:false ~ty_args:false t
    |> Sequence.map PW.of_pair
  | Eq (t,u,true) ->
    begin match Ordering.compare ord t u with
      | Comparison.Eq -> Sequence.empty (* trivial *)
      | Comparison.Incomparable ->
        Sequence.doubleton
          (yield_term t (P.left P.stop)) (yield_term u (P.right P.stop))
      | Comparison.Gt -> Sequence.return (yield_term t (P.left P.stop))
      | Comparison.Lt -> Sequence.return (yield_term u (P.right P.stop))
    end
  | Bool _
  | Atom (_,false)
  | Eq (_,_,false) -> Sequence.empty

let passive_terms ord lit =
  let explore_term t pos =
    T.all_positions ~pos ~vars:false ~ty_args:false t
    |> Sequence.map PW.of_pair
  in
  match lit with
  | Atom (t,_) ->
    T.all_positions ~vars:false ~ty_args:false t
    |> Sequence.map PW.of_pair
  | Eq (t,u,_) ->
    begin match Ordering.compare ord t u with
      | Comparison.Eq -> Sequence.empty (* trivial *)
      | Comparison.Incomparable ->
        Sequence.append
          (explore_term t (P.left P.stop))
          (explore_term u (P.right P.stop))
      | Comparison.Gt -> explore_term t (P.left P.stop)
      | Comparison.Lt -> explore_term u (P.right P.stop)
    end
  | Bool _ -> Sequence.empty
