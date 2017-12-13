
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)


open Logtk
open Libzipperposition

type term = Term.t

(** Deal with boolean subterms as if prop was a specific case of datatype.
    The rules are:

          C[s]  
    ----------------------
      C[true] or s=false
    where s boolean, not a variable, proper subterm
*)


module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

(** {2 As Extension} *)

val extension : Extensions.t
