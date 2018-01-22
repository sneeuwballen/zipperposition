
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unique Identifiers} *)

type t = {
  id: int;
  name: string;
  mutable payload: exn list; (** Use [exn] as an open type for user-defined payload *)
}
type t_ = t

let make =
  let n = ref 0 in
  fun name ->
    let id = !n in
    incr n;
    {id; name; payload=[]; }

let makef fmt = CCFormat.ksprintf ~f:make fmt

let copy t = make t.name

let id t = t.id
let name t = t.name
let payload t = t.payload

let set_payload ?(can_erase=fun _->false) t e =
  let rec aux = function
    | [] -> [e]
    | e' :: tail when can_erase e' -> e :: tail
    | e' :: tail -> e' :: aux tail
  in
  t.payload <- aux t.payload

let payload_find ~f:p t =
  begin match t.payload with
    | [] -> None
    | e1 :: tail ->
      match p e1, tail with
        | Some _ as res, _ -> res
        | None, [] -> None
        | None, e2 :: tail2 ->
          match p e2, tail2 with
            | Some _ as res, _ -> res
            | None, [] -> None
            | None, e3 :: tail3 ->
              match p e3 with
                | Some _ as res -> res
                | None -> CCList.find_map p tail3
  end

let payload_pred ~f:p t =
  begin match t.payload with
    | [] -> false
    | e :: _ when p e -> true
    | _ :: e :: _ when p e -> true
    | _ :: _ :: e :: _ when p e -> true
    | l -> List.exists p l
  end

let hash t = t.id
let equal i1 i2 = i1.id = i2.id
let compare i1 i2 = Pervasives.compare i1.id i2.id

let pp out id = CCFormat.string out id.name
let to_string = CCFormat.to_string pp

let pp_full out id = Format.fprintf out "%s/%d" id.name id.id
let pp_fullc out id = Format.fprintf out "%s/@{<Black>%d@}" id.name id.id

let pp_tstp out id =
  if Util.tstp_needs_escaping id.name
  then CCFormat.fprintf out "'%s'" id.name
  else CCFormat.string out id.name

let pp_zf = pp_tstp

let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name = if i=0
      then Printf.sprintf "%c" names.[j]
      else Printf.sprintf "%c%d" names.[j] i
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

exception Attr_infix of string

exception Attr_prefix of string

exception Attr_parameter of int

type skolem_kind = K_normal | K_ind (* inductive *)

exception Attr_skolem of skolem_kind * int

exception Attr_distinct

let as_infix = payload_find ~f:(function Attr_infix s->Some s | _ -> None)
let is_infix id = as_infix id |> CCOpt.is_some
let as_prefix = payload_find ~f:(function Attr_prefix s->Some s | _ -> None)
let is_prefix id = as_prefix id |> CCOpt.is_some
let as_parameter id = payload_find id ~f:(function Attr_parameter i -> Some i | _ -> None)
let is_parameter id = as_parameter id |> CCOpt.is_some

let is_skolem id =
  payload_pred id
    ~f:(function
      | Attr_skolem _ -> true
      | _ -> false)

let as_skolem id =
  payload_find id
    ~f:(function
      | Attr_skolem (a,_) -> Some a
      | _ -> None)

let num_mandatory_args id =
  let n_option =
    payload_find id
      ~f:(function
        | Attr_skolem (_, n) -> Some n
        | _ -> None)
  in
  match n_option with
    | Some n -> n
    | None -> 0

let is_distinct_object id =
  payload_pred id
    ~f:(function
      | Attr_distinct -> true
      | _ -> false)
