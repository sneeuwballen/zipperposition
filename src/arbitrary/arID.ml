
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary generation of Identifiers} *)

open Logtk

type 'a arbitrary = 'a QCheck.arbitrary
type 'a gen = 'a QCheck.Gen.t

let default_g =
  let l = List.map ID.make ["f"; "g"; "h"; "a"; "b"; "c"; "d"] in
  QCheck.Gen.oneofl l

let default = QCheck.make ~print:ID.to_string default_g

let set_g =
  QCheck.Gen.(list default_g >|= ID.Set.of_list)
let set = QCheck.make set_g
