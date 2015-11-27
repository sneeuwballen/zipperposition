
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Symbols} *)

type t = {
  mutable id : int;
  name : string;
}

type sym = t

let compare a b = Pervasives.compare a.id b.id
let equal a b = a.id = b.id
let hash_fun a h = CCHash.int a.id h
let hash a = a.id

module Map = Sequence.Map.Make(struct type t = sym let compare = compare end)
module Set = Sequence.Set.Make(struct type t = sym let compare = compare end)
module Tbl = Hashtbl.Make(struct type t = sym let equal = equal let hash = hash end)

let is_distinct c =
  let s = c.name in
  s <> "" && s.[0] = '"' && s.[String.length s - 1] = '"'

let to_string s = s.name
let pp out s = Format.pp_print_string out s.name

(** {2 Cstructors} *)
module H = Hashcons.Make(struct
  type t = sym
  let equal = equal
  let hash = hash
  let tag i c = c.id <- i
end)

let of_string name=
  let cs = {name=name; id= ~-1;} in
  H.hashcons cs

let gensym =
  let n = ref 0 in
  fun ?(prefix="_logtk") () ->
    let n' = !n in
    incr n;
    of_string (prefix ^ string_of_int n')

module Seq = struct
  let add_set set =
    Sequence.fold (fun set s -> Set.add s set) set
end

module TPTP = struct
  let real = of_string "$real"

  let forall_fun = of_string "!!"
  let exists_fun = of_string "??"

  (** {3 Arith} *)

  module Arith = struct

    let floor = of_string "$floor"
    let ceiling = of_string "$ceiling"
    let truncate = of_string "$truncate"
    let round = of_string "$round"

    let prec = of_string "$prec"
    let succ = of_string "$succ"

    let sum = of_string "$sum"
    let difference = of_string "$difference"
    let uminus = of_string "$uminus"
    let product = of_string "$product"
    let quotient = of_string "$quotient"

    let quotient_e = of_string "$quotient_e"
    let quotient_t = of_string "$quotient_t"
    let quotient_f = of_string "$quotient_f"
    let remainder_e = of_string "$remainder_e"
    let remainder_t = of_string "$remainder_t"
    let remainder_f = of_string "$remainder_f"

    let is_int = of_string "$is_int"
    let is_rat = of_string "$is_rat"

    let to_int = of_string "$to_int"
    let to_rat = of_string "$to_rat"

    let less = of_string "$less"
    let lesseq = of_string "$lesseq"
    let greater = of_string "$greater"
    let greatereq = of_string "$greatereq"

    let set =
      let l = [
        sum; difference; uminus; product; quotient;
        quotient_e; quotient_t; quotient_f;
        remainder_e; remainder_t; remainder_f;
        less; lesseq; greater; greatereq;
      ] in
      Set.of_seq (Sequence.of_list l)

    let symbols = Set.to_seq set

    let is_arith s = Set.mem s set
  end
end
