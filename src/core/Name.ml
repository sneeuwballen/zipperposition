open struct
  module NP = Name_payload
end

include Hstring

type payload = Name_payload.t

type skolem_kind =
  | K_normal
  | K_after_cnf
  | K_lazy_cnf
  | K_ind (* inductive *)

type Name_payload.t +=
  | Attr_infix of string  (** Infix name for pretty-printing *)
  | Attr_prefix of string  (** Prefix name for pretty-printing *)
  | Attr_parameter of int  (** Parameter, used for HO unif *)
  | Attr_skolem of skolem_kind
  | Attr_distinct
  | Attr_comm
  | Attr_assoc
  | Attr_cnf_def

let as_prefix =
  NP.find ~f:(function
    | Attr_prefix s -> Some s
    | _ -> None)

let as_infix =
  NP.find ~f:(function
    | Attr_infix s -> Some s
    | _ -> None)

let is_skolem id =
  NP.exists id ~f:(function
    | Attr_skolem _ -> true
    | _ -> false)

let is_postcnf_skolem id =
  NP.exists id ~f:(function
    | Attr_skolem K_after_cnf -> true
    | _ -> false)

let is_lazycnf_skolem id =
  NP.exists id ~f:(function
    | Attr_skolem K_lazy_cnf -> true
    | _ -> false)

let as_skolem id =
  NP.find id ~f:(function
    | Attr_skolem a -> Some a
    | _ -> None)

let as_parameter s =
  NP.find s ~f:(function
    | Attr_parameter i -> Some i
    | _ -> None)

let is_distinct_object s =
  NP.exists s ~f:(function
    | Attr_distinct -> true
    | _ -> false)

(** To be avoided when possible *)
let gensym =
  let r = ref 0 in
  let names = "abcdefghijklmopq" in
  fun () ->
    let i = !r / String.length names in
    let j = !r mod String.length names in
    let name =
      if i = 0 then
        Printf.sprintf "$$%c" names.[j]
      else
        Printf.sprintf "$$%c%d" names.[j] i
    in
    incr r;
    make name

let is_parameter id = as_parameter id |> CCOpt.is_some

let is_comm id =
  CCOpt.is_some
  @@ NP.find
       ~f:(function
         | Attr_comm -> Some 1
         | _ -> None)
       id

let is_assoc id =
  CCOpt.is_some
  @@ NP.find
       ~f:(function
         | Attr_assoc -> Some 1
         | _ -> None)
       id

let is_ac id = is_comm id && is_assoc id

(** {2 Printers} *)

let pp_full out self = Format.fprintf out "%s/%d" self.str self.id
let pp out self = CCFormat.string out self.str

let pp_tstp out id =
  if Util.tstp_needs_escaping id.str then
    CCFormat.fprintf out "'%s'" id.str
  else
    CCFormat.string out id.str

let pp_zf = pp_tstp
