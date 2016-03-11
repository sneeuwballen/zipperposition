
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition
open Libzipperposition_parsers

module type S = Induction_intf.S

module Make
(E: Env.S)
(A : Avatar_intf.S with module E = E)
  : S
  with module Env = E

val init_from_decls :
  (ID.t * Ast_tptp.optional_info) Sequence.t -> unit
(** Initialize from a bunch of declarations' optional info, if one takes
    only pairs [(some_type_name : $tType, info)] *)

val extension : Extensions.t
