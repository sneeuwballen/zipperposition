
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for Parsing} *)

open Libzipperposition

module Loc = ParseLocation

exception Parse_error of Loc.t * string

let () = Printexc.register_printer
  (function
    | Parse_error (loc, msg) ->
        Some 
          (CCFormat.sprintf "@[<2>parse error:@ @[%s@]@ %a@]" msg Loc.pp loc)
    | _ -> None)

let error loc msg = raise (Parse_error (loc,msg))
let errorf loc msg = CCFormat.ksprintf msg ~f:(error loc)
