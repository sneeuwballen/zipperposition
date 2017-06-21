
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Defined positions for Defined Functions} *)

module Fmt = CCFormat

(** positions that are immediate arguments of some defined constant
    can be classified as follows:
    - active position (patterns on LHS of rules)
    - invariant positions (variable on LHS and RHS of rules)
    - accumulator positions (variable on LHS, non-variable on RHS)
*)
type t =
  | P_active
  | P_invariant
  | P_accumulator

type pos = t

let equal (a:t)(b:t): bool = a=b

let pp out = function
  | P_active -> Fmt.string out "active"
  | P_invariant -> Fmt.string out "invariant"
  | P_accumulator -> Fmt.string out "accumulator"

module Arr : sig
  type t = pos IArray.t
  val pp : t CCFormat.printer
end = struct
  type t = pos IArray.t

  let pp out (a:t) =
    Fmt.(within "[" "]" @@ hvbox @@
      seq @@ pair ~sep:(return ":") int pp)
      out (IArray.to_seqi a)
end
