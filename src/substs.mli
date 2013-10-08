(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Substitutions} *)

(** substitution, a list of (variable -> term) *)
type t = private
  | SubstBind of (Term.t * int * Term.t * int * t)
  | SubstEmpty
and scope = int
and 'a scoped = 'a * scope
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

val empty : t
  (** The identity substitution *)

val is_empty : t -> bool
  (** Is the substitution empty? *)

val eq : t -> t -> bool
  (** Check (naively, ie structurally) whether two substitutions are equal *)

val compare : t -> t -> scope
  (** Compare substitutions (arbitrary but total order) *)

val lookup : t -> Term.t -> scope -> Term.t * scope
  (** Lookup variable in substitution. Raise Not_found if not present. *)

val get_var : t -> Term.t -> scope -> Term.t * scope
  (** Lookup recursively the var in the substitution, until it is not a
      variable anymore, or it is not bound *)

val is_in_subst : t -> Term.t -> scope -> bool
  (** Check whether the variable is bound by the substitution *)

val bind : ?recursive:bool -> t -> Term.t -> scope -> Term.t -> scope -> t
  (** Add v -> t to the substitution. Both terms have a context. Raise
      Invalid_argument if v is already bound in the same context, to another term. *)

val remove : t -> Term.t -> int -> t
  (** Remove the given binding. No other variable should depend on it... *)

val append : t -> t -> t
  (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

(** Disambiguation of variables between different contexts *)
module Renaming : sig
  type t
    (** A renaming, from (variable,offset) to variable *)
  
  val create : int -> t
    (** Create a new general-purpose renaming, which manages to rename
        variables of any number of contexts without ambiguities *)

  val dummy : t
    (** Renaming that does not rename (yes...). It maps all variables to
        themselves, regardless of the scope they occur in. Use with caution! *)

  val clear : t -> unit
    (** Clear the content of the renaming *)

  val rename : t -> Term.t -> scope -> Term.t
    (** Rename the given variable, scoped by the given context *)
end

val apply : ?recursive:bool -> ?depth:int -> renaming:Renaming.t -> 
            t -> Term.t -> scope -> Term.t
  (** Apply substitution to term, replacing variables by the terms they are bound to.

      [renaming] is used to rename free variables (not bound by [subst])
      while avoiding collisions.    
      [recursive] decides whether, when [v] is replaced by [t], [subst] is
      applied to [t] recursively or not (default true). *)

val apply_f : ?recursive:bool -> ?depth:int -> renaming:Renaming.t -> 
              t -> Formula.t -> scope -> Formula.t
  (** Apply the substitution to the formula *)

val apply_no_renaming : ?recursive:bool -> ?depth:int ->
                        t -> Term.t -> scope -> Term.t
  (** Apply the substitution, and does not rename variables. {b Caution}, this
      can entail collisions between scopes! *)

module VarSet : Set.S with type elt = Term.t * scope
  (** Set of bound terms *)

val domain : t -> VarSet.t
  (** Domain of substitution *)

val codomain : t -> VarSet.t
  (** Codomain (image terms) of substitution *)

val introduced : t -> VarSet.t
  (** Variables introduced by the substitution (ie vars of codomain) *)

val compose : t -> t -> t
  (** [compose s1 s2] is the substitution that to [x] associates
      [s1 (s2 x)]. *)

(* XXX is it possible to express it with this representation of substs?
val join : t -> t -> t
  (** [join s1 s2] maps [x] to [s1 (s2 x)] if [x] is in the domain of [s2],
      and to [s1 x] if [x] is in the domain of s1 but not in [introduced s2].
      Basically, it hides the variables introduced in [s2] and bound in [s1] *)
*)

val is_renaming : t -> bool
  (** Check whether the substitution is a variable renaming *)

val infer : TypeInference.Ctx.t -> t -> unit
  (** Infer types using the signature in the given context.
      @raise Type.Error if types are not consistent *)

val check_type : TypeInference.Ctx.t -> t -> bool
  (** Is the substitution well-typeable in the given context? *)

val check_type_sig : Signature.t -> t -> bool

val pp_full : (Buffer.t -> Term.t -> unit) -> Buffer.t -> t -> unit
val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val fold : t -> 'a -> ('a -> Term.t -> scope -> Term.t -> scope -> 'a) -> 'a
val iter : t -> (Term.t * scope * Term.t * scope -> unit) -> unit

val to_seq : t -> (Term.t * scope * Term.t * scope) Sequence.t
val of_seq : ?recursive:bool -> ?subst:t ->
            (Term.t * scope * Term.t * scope) Sequence.t -> t
val of_list : ?recursive:bool -> ?subst:t ->
            (Term.t * scope * Term.t * scope) list -> t

val bij : t Bij.t
