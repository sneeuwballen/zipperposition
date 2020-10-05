
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

module type S = sig
  module Env : Env.S
end

module Make(E : Env.S) : S with module Env = E