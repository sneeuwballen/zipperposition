
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Unique Identifiers} *)

type t = {
  id: int;
  name: string;
  mutable payload: exn; (** Use [exn] as an open type for user-defined payload *)
}
type t_ = t

exception No_payload

let make =
  let n = ref 0 in
  fun name ->
    let id = !n in
    incr n;
    {id; name; payload=No_payload; }

let copy t = make t.name

let id t = t.id
let name t = t.name
let payload t = t.payload

let set_payload_erase t e = t.payload <- e

let set_payload ?(can_erase=fun _->false) t e = match t.payload with
  | No_payload -> t.payload <- e
  | old_e when can_erase old_e -> t.payload <- e
  | old_e -> invalid_arg ("ID.set_payload: collision with "^ Printexc.to_string old_e)

let hash t = t.id
let equal i1 i2 = i1.id = i2.id
let compare i1 i2 = Pervasives.compare i1.id i2.id

let pp out id = CCFormat.string out id.name
let to_string = CCFormat.to_string pp

let pp_full out id = Format.fprintf out "%s/%d" id.name id.id
let pp_fullc out id = Format.fprintf out "%s/@{<Black>%d@}" id.name id.id

let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name = if i=0
      then CCPrint.sprintf "%c" names.[j]
      else CCPrint.sprintf "%c%d" names.[j] i
    in
    incr r;
    make name

module O_ = struct
  type t = t_
  let equal = equal
  let compare = compare
  let hash = hash
end

module Map = CCMap.Make(O_)
module Set = CCSet.Make(O_)
module Tbl = CCHashtbl.Make(O_)
