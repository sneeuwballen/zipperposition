
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {2 Induction} *)

open Libzipperposition

module type S = sig
  module Env : Env.S

  val register : unit -> unit
  (** Register the inference rules for inductive reasoning *)
end
