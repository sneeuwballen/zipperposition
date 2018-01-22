
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Fingerprint term indexing} *)

(** Fingerprint term indexing, based on  the paper of the same
    name by S. Schulz.

    NOTE: less efficient, it seems, than {!NPDtree}
*)

type fingerprint_fun
(** A fingerprint function is a list of positions.

    To compute the fingerprint of a term, we map
    each position to a feature;
    fingerprints can then be compared for potential unifiability/matching.
*)

val fp3d : fingerprint_fun
val fp3w : fingerprint_fun
val fp4d : fingerprint_fun
val fp4m : fingerprint_fun
val fp4w : fingerprint_fun
val fp5m : fingerprint_fun
val fp6m : fingerprint_fun
val fp7 : fingerprint_fun
val fp7m : fingerprint_fun
val fp16 : fingerprint_fun

module Make(X : Set.OrderedType) : sig
  include Index.TERM_IDX with type elt = X.t

  val default_fp : fingerprint_fun

  val empty_with : fingerprint_fun -> t
  (** Empty index, using the given fingerprint function *)

  val get_fingerprint : t -> fingerprint_fun
end
