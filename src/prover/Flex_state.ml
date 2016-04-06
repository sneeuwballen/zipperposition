
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {2 Extensible Map for State}  *)

module M = CCHet.Map

type 'a key = 'a CCHet.Key.t

let create_key () = CCHet.Key.create()

type t = M.t

let add = M.add
let get = M.find
let get_exn = M.find_exn
let get_or ~or_ k m =
  try M.find_exn k m
  with Not_found -> or_

let empty = M.empty
