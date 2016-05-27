
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement Source} *)

type from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

type t =
  | From_file of from_file
  | Neg of t
  | CNF of t

let file x = x.file
let name x = x.name
let loc x = x.loc

let from_file ?loc ?name file = From_file { name; loc; file; }
let neg x = Neg x
let cnf x = CNF x

let pp_from_file out x =
  let pp_name out = function
    | None -> ()
    | Some n -> Format.fprintf out "at %s " n
  in
  Format.fprintf out "@[<2>%ain@ `%s`@,%a@]"
    pp_name x.name x.file ParseLocation.pp_opt x.loc

let rec pp out t =
  match t with
    | From_file x -> pp_from_file out x
    | Neg t -> Format.fprintf out "@[<2>neg(%a)@]" pp t
    | CNF t -> Format.fprintf out "@[<2>cnf(%a)@]" pp t

let to_string = CCFormat.to_string pp
