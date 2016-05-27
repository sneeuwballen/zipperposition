(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement Source}

    Where a statement originally comes from (file, location, named statement,
    or result of some transformations, etc.) *)

type from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

type t =
  | From_file of from_file
  | Neg of t
  | CNF of t

val file : from_file -> string
val name : from_file -> string option
val loc : from_file -> ParseLocation.t option

val from_file : ?loc:ParseLocation.t -> ?name:string -> string -> t
(** make a new sourced item. Default [is_conjecture] is [false]. *)

val neg : t -> t
val cnf : t -> t

val pp_from_file : from_file CCFormat.printer
include Interfaces.PRINT with type t := t
