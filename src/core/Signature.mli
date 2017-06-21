
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Signature} *)

(** A signature is a finite mapping from identifiers to types. *)

type t = Type.t ID.Map.t
(** A signature maps symbols to types *)

val empty : t
(** Empty signature *)

val is_empty : t -> bool

val singleton : ID.t -> Type.t -> t

val mem : t -> ID.t -> bool
(** Is the symbol declared? *)

exception AlreadyDeclared of ID.t * Type.t * Type.t

val declare : t -> ID.t -> Type.t -> t
(** Declare the symbol, or
    @raise AlreadyDeclared if the symbol is already defined
    @raise Invalid_argument if the type has free variables *)

val find : t -> ID.t -> Type.t option
(** Lookup the type of a symbol *)

val find_exn : t -> ID.t -> Type.t
(** Lookup the type of a symbol
    @raise Not_found if the symbol is not in the signature *)

val arity : t -> ID.t -> int * int
(** Arity of the given symbol, or failure.
    see {!Type.arity} for more details about the returned value.
    @raise Not_found if the symbol is not in the signature *)

val cardinal : t -> int
(** Number of symbols *)

val is_ground : t -> bool
(** Only ground types? *)

val merge : t -> t -> t
(** Merge two signatures together.
    @raise Type.Error if they share some symbols with distinct types *)

val diff : t -> t -> t
(** [diff s1 s2] contains the symbols of [s1] that do not appear
    in [s2]. Useful to remove base symbols. *)

val well_founded : t -> bool
(** Are there some symbols of arity 0 in the signature?
    @return true iff the Herbrand term universe of this signature is
      non empty  *)

module Seq : sig
  val symbols : t -> ID.t Sequence.t
  val types : t -> Type.t Sequence.t
  val to_seq : t -> (ID.t * Type.t) Sequence.t
  val of_seq : (ID.t * Type.t) Sequence.t -> t
  val add_seq : t -> (ID.t * Type.t) Sequence.t -> t
end

val to_set : t -> ID.Set.t
(** Set of symbols of the signature *)

val to_list : t -> (ID.t * Type.t) list
val add_list : t -> (ID.t * Type.t) list -> t
val of_list : (ID.t * Type.t) list -> t

val iter : t -> (ID.t -> Type.t -> unit) -> unit

val fold : t -> 'a -> ('a -> ID.t -> Type.t -> 'a) -> 'a

val is_bool : t -> ID.t -> bool
(** Has the symbol a boolean return sort?
    @raise Not_found if the symbol is not in the signature *)

val is_not_bool : t -> ID.t -> bool

(** {2 IO} *)

include Interfaces.PRINT with type t := t
