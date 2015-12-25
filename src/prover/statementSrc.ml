
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement Source} *)

open Libzipperposition

type t = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
  is_conjecture : bool;
}

let file x = x.file
let name x = x.name
let loc x = x.loc
let is_conjecture x = x.is_conjecture

let make ?(is_conjecture=false) ?loc ?name file = {
  name;
  loc;
  file;
  is_conjecture;
}

let pp out x =
  let pp_name out = function
    | None -> ()
    | Some n -> Format.fprintf out "at %s " n
  and pp_is_conj out x =
    if x.is_conjecture then Format.fprintf out "(conjecture)" else ()
  in
  Format.fprintf out "@[<2>%ain@ `%s`@,%a@ %a@]"
    pp_name x.name x.file ParseLocation.pp_opt x.loc pp_is_conj x

let to_string = CCFormat.to_string pp
