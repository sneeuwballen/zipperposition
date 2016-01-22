
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement Source} *)

type t = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

let file x = x.file
let name x = x.name
let loc x = x.loc

let make ?loc ?name file = { name; loc; file; }

let pp out x =
  let pp_name out = function
    | None -> ()
    | Some n -> Format.fprintf out "at %s " n
  in
  Format.fprintf out "@[<2>%ain@ `%s`@,%a@]"
    pp_name x.name x.file ParseLocation.pp_opt x.loc

let to_string = CCFormat.to_string pp
