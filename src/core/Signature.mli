(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Signature} *)

(** A signature is a finite mapping from identifiers to types and a property
    sym_in_conjecture. *)

type t = private {
  sym_map: (Type.t * bool) Name.Map.t;
  ty_map: Name.Set.t Type.Map.t;
}
(** A signature maps symbols to types, and there is a map in the backwards
    direction *)

val empty : t
(** Empty signature *)

val is_empty : t -> bool

val mem : t -> Name.t -> bool
(** Is the symbol declared? *)

exception AlreadyDeclared of Name.t * Type.t * Type.t

val declare : t -> Name.t -> Type.t -> t
(** Declare the symbol, or
    @raise AlreadyDeclared if the symbol is already defined
    @raise Invalid_argument if the type has free variables *)

val find : t -> Name.t -> Type.t option
(** Lookup the type of a symbol *)

val find_exn : t -> Name.t -> Type.t
(** Lookup the type of a symbol
    @raise Not_found if the symbol is not in the signature *)

val find_by_type : t -> Type.t -> Name.Set.t
(** Reverse lookup -- given a type return all IDs with that type *)

val sym_in_conj : Name.t -> t -> bool
val set_sym_in_conj : Name.t -> t -> t

val arity : t -> Name.t -> int * int
(** Arity of the given symbol, or failure. see {!Type.arity} for more details
    about the returned value.
    @raise Not_found if the symbol is not in the signature *)

val cardinal : t -> int
(** Number of symbols *)

val is_ground : t -> bool
(** Only ground types? *)

val merge : t -> t -> t
(** Merge two signatures together.
    @raise Type.Error if they share some symbols with distinct types *)

val diff : t -> t -> t
(** [diff s1 s2] contains the symbols of [s1] that do not appear in [s2]. Useful
    to remove base symbols. *)

val well_founded : t -> bool
(** Are there some symbols of arity 0 in the signature?
    @return true iff the Herbrand term universe of this signature is non empty
*)

module Seq : sig
  val symbols : t -> Name.t Iter.t
  val types : t -> Type.t Iter.t
  val to_iter : t -> (Name.t * (Type.t * bool)) Iter.t
  val of_iter : (Name.t * Type.t) Iter.t -> t
end

val to_set : t -> Name.Set.t
(** Set of symbols of the signature *)

val to_list : t -> (Name.t * (Type.t * bool)) list
val iter : t -> (Name.t -> Type.t * bool -> unit) -> unit
val fold : t -> 'a -> ('a -> Name.t -> Type.t * bool -> 'a) -> 'a

val is_bool : t -> Name.t -> bool
(** Has the symbol a boolean return sort?
    @raise Not_found if the symbol is not in the signature *)

val is_not_bool : t -> Name.t -> bool

(** {2 IO} *)

include Interfaces.PRINT with type t := t
