(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement Source}

    Where a statement originally comes from (file, location, named statement, etc.) *)

type t = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

val file : t -> string
val name : t -> string option
val loc : t -> ParseLocation.t option

val make : ?loc:ParseLocation.t -> ?name:string -> string -> t
(** make a new sourced item. Default [is_conjecture] is [false]. *)

include Interfaces.PRINT with type t := t
