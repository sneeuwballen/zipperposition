
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module PW = Position.With

type clause = Hornet_types.clause
type proof = Hornet_types.proof
type constraint_ = Hornet_types.c_constraint_

type t = Hornet_types.horn_clause
type horn_clause = t

(* TODO: add a unique ID for fast comparison/storage? *)

let make ?(constr=[]) head body proof =
  { hc_head=head;
    hc_body=body;
    hc_proof=proof;
    hc_constr=constr;
  }

let head c = c.hc_head
let body c = c.hc_body
let proof c = c.hc_proof

let body_seq c = IArray.to_seq (body c)
let body_l c = IArray.to_list (body c)

let body_len c = IArray.length (body c)

let body1 c = IArray.get (body c) 0

let body_get c n =
  if n < 0 || n >= IArray.length (body c) then invalid_arg "Horn.body_get";
  IArray.get (body c) n

let pp = Hornet_types_util.pp_hclause

let concl_pos c = PW.return (head c) |> PW.head
let body_pos n c = PW.return (body_get c n) |> PW.body |> PW.arg n
let body1_pos = body_pos 0

(** {2 Pairing with Position} *)

module With_pos = struct
  type t = horn_clause Position.With.t
  let compare = PW.compare compare
  let pp = PW.pp pp
  let to_string = Fmt.to_string pp
end
