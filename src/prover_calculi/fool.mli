
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

  (** {5 Registration} *)

  val setup : unit -> unit
  val rw_bool_lits : Env.multi_simpl_rule
  (** Register rules in the environment *)

end

module Make(E : Env.S) : S with module Env = E
(* let f = Make(e) in f.ppaarra *)

(** {2 As Extension} *)

val extension : Extensions.t
