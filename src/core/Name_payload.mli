(** Extra payload attached to names, scoped per logic context.

    A payload scope is a thread-local {!state}. Enter one with {!with_scope}
    (each file and retractable logic context gets one); {!add}, {!find},
    {!exists}, {!get}, {!erase} and {!clear} act on the innermost scope of the
    current thread and fail if no scope is active. Mutations are retracted when
    the scope exits. *)

type t = ..
(** Extensible payload type; extended by {!Name}, {!Type}, {!Ind_ty},
    {!Rewrite}, etc. *)

type state = { payloads: t list Hstring.Tbl.t }
(** Payload table of one scope. *)

val new_state : unit -> state

val copy : state -> state
(** Copy of a state, sharing nothing with the original. *)

val with_scope : (unit -> 'a) -> 'a
(** [with_scope f] runs [f] in a scope that starts as a copy of the current
    state (empty if none is active); on return or raise, the previous state is
    restored. *)

val add : ?can_erase:(t -> bool) -> Hstring.t -> t -> unit
val get : Hstring.t -> t list
val find : f:(t -> 'a option) -> Hstring.t -> 'a option
val exists : f:(t -> bool) -> Hstring.t -> bool
val erase : Hstring.t -> f:(t -> bool) -> unit
val clear : unit -> unit

val get_opt : unit -> state option
(** [Some st] if a payload scope is active on this thread, [None] otherwise.
    Unlike {!find} and friends, this never raises. *)

(** Pure operations on payload lists, used by {!ID}. *)
module Pure : sig
  val add : ?can_erase:(t -> bool) -> t -> t list -> t list
  val find : f:(t -> 'a option) -> t list -> 'a option
  val exists : f:(t -> bool) -> t list -> bool
end
