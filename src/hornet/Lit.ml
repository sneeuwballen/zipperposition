
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

open Libzipperposition

module Fmt = CCFormat
module T = FOTerm

type ty = Type.t
type term = T.t

type t =
  | Bool of bool
  | Atom of term * bool

let true_ = Bool true
let false_ = Bool false
let atom ?(sign=true) t = Atom (t, sign)

let sign = function
  | Atom (_, b)
  | Bool b -> b

let equal a b: bool = match a, b with
  | Bool b1, Bool b2 -> b1=b2
  | Atom (t1,sign1), Atom (t2,sign2) -> T.equal t1 t2 && sign1=sign2
  | Bool _, _
  | Atom _, _ -> false

let hash = function
  | Bool b -> Hash.combine2 10 (Hash.bool b)
  | Atom (t,sign) -> Hash.combine3 20 (T.hash t) (Hash.bool sign)

let compare a b: int =
  let to_int = function Bool _ -> 0 | Atom _ -> 1 in
  begin match a, b with
    | Bool b1, Bool b2 -> CCOrd.bool_ b1 b2
    | Atom (t1,sign1), Atom (t2,sign2) ->
      CCOrd.( T.compare t1 t2 <?> (bool_, sign1, sign2))
    | Bool _, _
    | Atom _, _ -> CCInt.compare (to_int a)(to_int b)
  end

let pp out t: unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t

let to_string = Fmt.to_string pp


let vars_seq = function
  | Bool _ -> Sequence.empty
  | Atom (t,_) -> T.Seq.vars t

let vars_list l = vars_seq l |> Sequence.to_rev_list

let vars_set l =
  vars_seq l
  |> Sequence.to_rev_list
  |> CCList.sort_uniq ~cmp:(HVar.compare Type.compare)

let weight = function
  | Bool _ -> 0
  | Atom (t, _) -> T.weight t
