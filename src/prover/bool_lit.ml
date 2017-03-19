
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Literal} *)

open Logtk

module type S = Bool_lit_intf.S

module type PAYLOAD = sig
  type t
  val dummy : t
end

let stat_num_lit = Util.mk_stat "msat.num_lits"

module Make(Payload : PAYLOAD)
  : S with type payload = Payload.t
= struct
  type t = {
    id: int; (* sign = sign of literal *)
    payload: Payload.t;
    neg: t; (* negation *)
  }
  type lit = t
  type payload = Payload.t

  let rec dummy = { id=0; neg=dummy; payload=Payload.dummy; }

  let fresh_id =
    let n = ref 1 in
    fun () ->
      Util.incr_stat stat_num_lit;
      let id = !n in
      incr n;
      id

  (* factory for literals *)
  let make =
    fun payload ->
      let id = fresh_id () in
      let rec pos = {
        id;
        payload;
        neg;
      } and neg = {
          id= -id;
          payload;
          neg=pos;
        } in
      pos

  let hash i = Hash.int i.id
  let equal i j = i.id = j.id
  let compare i j = CCInt.compare i.id j.id
  let neg i = i.neg
  let sign i = i.id > 0
  let abs i = if i.id > 0 then i else i.neg
  let norm i = abs i, i.id < 0
  let set_sign b i = if b then abs i else (abs i).neg
  let apply_sign b i = if b then i else i.neg
  let payload i = i.payload
  let to_int i = i.id
  let pp out i = Format.fprintf out "%s%d" (if sign i then "" else "¬") i.id

  module AsKey = struct
    type t = lit
    let compare = compare
    let hash = hash
    let equal = equal
  end

  module Set = CCSet.Make(AsKey)
  module Tbl = CCHashtbl.Make(AsKey)
end
