(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {6 Specifications of Built-in Theories} *)

open Logtk

module Hash = CCHash
module T = FOTerm

type term = FOTerm.t

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = {
    sym : ID.t;
    ty : Type.t;
  }

end
