(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unique Identifiers} *)

open Name_payload

type payload = Name_payload.t

type t = {
  id: int;
  name: string;
  mutable payload: Name_payload.t list;
      (** Use [exn] as an open type for user-defined payload *)
}

type t_ = t

let make =
  let n = ref 0 in
  fun name ->
    let id = !n in
    incr n;
    { id; name; payload = [] }

let makef fmt = CCFormat.ksprintf ~f:make fmt
let copy t = make t.name
let id t = t.id
let name t = t.name
let payload t = t.payload

(* for temporary purposes *)
let dummy_of_int id =
  let name = "DUMMY_" ^ CCInt.to_string id in
  { id; name; payload = [] }

let set_payload ?can_erase t e =
  t.payload <- Name_payload.Pure.add ?can_erase e t.payload

let payload_find ~f:p t = Name_payload.Pure.find ~f:p t.payload
let payload_pred ~f:p t = Name_payload.Pure.exists ~f:p t.payload
let hash t = t.id
let equal i1 i2 = i1.id = i2.id
let compare i1 i2 = Stdlib.compare i1.id i2.id
let pp out id = CCFormat.string out id.name
let to_string = CCFormat.to_string pp
let pp_full out id = Format.fprintf out "%s/%d" id.name id.id
let pp_fullc = pp_full

let pp_tstp out id =
  if Util.tstp_needs_escaping id.name then
    CCFormat.fprintf out "'%s'" id.name
  else
    CCFormat.string out id.name

let pp_zf = pp_tstp

let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name =
      if i = 0 then
        Printf.sprintf "%c" names.[j]
      else
        Printf.sprintf "%c%d" names.[j] i
    in
    incr r;
    make name

module As_key = struct
  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = hash
end

module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)
module Tbl = CCHashtbl.Make (As_key)

let as_infix =
  payload_find ~f:(function
    | Name.Attr_infix s -> Some s
    | _ -> None)

let is_infix id = as_infix id |> CCOpt.is_some

let as_prefix =
  payload_find ~f:(function
    | Name.Attr_prefix s -> Some s
    | _ -> None)

let is_prefix id = as_prefix id |> CCOpt.is_some

let as_parameter id =
  payload_find id ~f:(function
    | Name.Attr_parameter i -> Some i
    | _ -> None)

let is_parameter id = as_parameter id |> CCOpt.is_some

let is_comm id =
  CCOpt.is_some
  @@ payload_find
       ~f:(function
         | Name.Attr_comm -> Some 1
         | _ -> None)
       id

let is_assoc id =
  CCOpt.is_some
  @@ payload_find
       ~f:(function
         | Name.Attr_assoc -> Some 1
         | _ -> None)
       id

let is_ac id = is_comm id && is_assoc id

let is_skolem id =
  payload_pred id ~f:(function
    | Name.Attr_skolem _ -> true
    | _ -> false)

let is_postcnf_skolem id =
  payload_pred id ~f:(function
    | Name.Attr_skolem K_after_cnf -> true
    | _ -> false)

let is_lazycnf_skolem id =
  payload_pred id ~f:(function
    | Name.Attr_skolem K_lazy_cnf -> true
    | _ -> false)

let as_skolem id =
  payload_find id ~f:(function
    | Name.Attr_skolem a -> Some a
    | _ -> None)

(* Note: If you want to reinsert mandatory arguments: They were here. (let num_mandatory_args _ =) *)
