
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {2 Induction} *)

module type S = sig
  module Env : Env.S
  module Ctx = Env.Ctx

  module Meta : sig
    val t : Ind_types.t Plugin.t
    (** Plugin that encodes the fact that a type is inductive, together
        with the list of its constructor symbols.
        Example: [nat, [succ; zero]] *)
  end

  val register : unit -> unit
  (** Register the inference rules for inductive reasoning *)
end
