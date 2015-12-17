

(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {6 Specifications of Built-in Theories} *)

open Logtk

type term = FOTerm.t

(** TODO: theory of inductive types (e.g. lists, or finite domain types
          with only a few constructors);
          then, some case-reasoning inference over those inductive types *)

(** {2 Associativity-Commutativity} *)

module AC : sig
  type t = {
    sym : ID.t;
    ty : Type.t;
  }
end
