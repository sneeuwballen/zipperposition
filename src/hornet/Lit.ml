
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

open Libzipperposition

module Fmt = CCFormat
module T = FOTerm

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

let pp out t: unit = match t with
  | Bool b -> Fmt.bool out b
  | Atom (t, true) -> T.pp out t
  | Atom (t, false) -> Fmt.fprintf out "@[@<1>¬@[%a@]@]" T.pp t

let to_string = Fmt.to_string pp
