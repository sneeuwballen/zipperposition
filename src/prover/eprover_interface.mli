(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interfacing with E} *)

module type S = sig
  module Env : Env.S

  (** {5 Registration} *)

  val set_e_bin : string -> unit
  val try_e : Env.C.t Iter.t -> Env.C.t Iter.t -> Logtk.Proof.proof option

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make (E : Env.S) : S with module Env = E
