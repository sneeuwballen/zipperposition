
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cover Set} *)

(** Use for reasoning by case during induction *)

type cst = Ind_cst.t

type case

type t = case list

(** {6 Inductive Case}

    An inductive case is a term that belongs to the coverset of some
    inductive constant. The inductive constant must fall into one
    of the cases in its coverset.

    Every case starts with a constructor of its type. *)
module Case : sig
  type t = case

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val pp : t CCFormat.printer

  val to_term : t -> FOTerm.t

  val is_rec : t -> bool
  val is_base : t -> bool

  val to_lit : t -> Literal.t

  val sub_constants : t -> cst Sequence.t
  (** All sub-constants that are subterms of a specific case *)
end

val declarations : t -> (ID.t * Type.t) Sequence.t
(** [declarations set] returns a list of type declarations that should
    be made if [set] is new (declare the top cst and its subcases) *)

val cases : ?which:[`Rec|`Base|`All] -> t -> case Sequence.t
(** Cases of the cover set *)

val sub_constants : t -> cst Sequence.t
(** All sub-constants of a given inductive constant *)

val make : ?coverset_depth:int -> depth:int -> Type.t -> t
(** Build a cover set for the given type.

    a set of ground terms [[t1,...,tn]] with fresh
    constants inside (that are not declared as inductive!) such that
    [bigor_{i in 1...n} t=ti] is the skolemized version of the
    exhaustivity axiom on [t]'s type.

    @param depth the induction depth for the top constant in the coverset
*)
