
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clause context}

    A clause with a "hole" in it. Filling the whole with a term [t] is called
    "applying the context to [t]".

    The point is to relate different applications of the same context. *)

open Logtk

type term = Term.t
type subst = Subst.t

(** A context is represented as a regular array of literals, containing
    at least one specific variable [x], paired with this variable [x].
    Applying the context is a mere substitution *)
type t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val make : Literals.t -> var:Term.var -> t
(** Make a context from a var and literals containing this var.
    @raise Assert_failure if the variable isn't present in any literal *)

val extract : Literals.t -> term -> t option
(** [extract lits t] returns [None] if [t] doesn't occur in [lits]. Otherwise,
    it creates a fresh var [x], replaces [t] with [x] within [lits], and
    returns the corresponding context.
    Basically, if [extract lits t = Some c] then [apply c t = lits] *)

val extract_exn : Literals.t -> term -> t
(** Unsafe version of {!extract}.
    @raise Invalid_argument if the term is not to be found within the lits *)

val trivial : Literals.t -> term -> t
(** Trivial context, that contains 0 holes. *)

val apply : t -> term -> Literals.t
(** [apply c t] fills the hole of [c] with the given term [t]. [t] and [c]
    share no free variables. *)

val apply_same_scope : t -> term -> Literals.t
(** Same as {!apply}, but now variables from the context and variables
    from the term live in the same scope *)

val raw_lits : t -> Literals.t
(** give access to the underlying literals. Careful not to depend
    on the variable's actual name. *)

(*
val matching : t -> Literals.t -> term option
(** Match the context against a proper clause. On success, [matching ctx c]
    returns a term [Some t] such that [extract c t = ctx],
    and [apply ctx t = c] *)
FIXME: is this even doable?*)

val pp : t CCFormat.printer

(** {2 Sets of contexts} *)

module Set : CCSet.S with type elt = t
