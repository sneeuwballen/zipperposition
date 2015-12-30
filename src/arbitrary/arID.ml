
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary generation of symbols} *)

open Libzipperposition
open QCheck

type 'a arbitrary = 'a QCheck.Arbitrary.t

let default =
  let l = List.map ID.make ["f"; "g"; "h"; "a"; "b"; "c"; "d"] in
  QCheck.Arbitrary.among l

let set =
  Arbitrary.(
    list default >|= ID.Set.of_list
  )
