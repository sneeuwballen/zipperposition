
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic signal for multiple-callbacks Observer} *)

(** A signal can be used to decouple the (unique) emitter of some event,
    and (multiple) receivers for this event. The signal is typically
    created by the emitter (in the same module), and exposed;
    then, observers can register to the signal and be notified every
    time an event (a value) is emitted through the signal. *)

type 'a t
(** Signal of type 'a *)

val create : unit -> 'a t
(** New signal *)

val send : 'a t -> 'a -> unit
(** Trigger the signal *)

type handler_response =
  | ContinueListening
  | StopListening

val on : 'a t -> ('a -> handler_response) -> unit
(** Register a handler to the signal; the handler returns [true]
    if it wants to continue being notified, [false] otherwise *)

val on_every : 'a t -> ('a -> _) -> unit
(** [on_every s f] calls [f] on every event signalled on [s] *)

val once : 'a t -> ('a -> 'b) -> unit
(** Register a handler to be called only once *)

val propagate : 'a t -> 'a t -> unit
(** [propagate a b] propagates all values of [a] into [b]. Cycles
    are not detected. *)

(** {2 Combinators} *)

val map : 'a t -> ('a -> 'b) -> 'b t

val filter : 'a t -> ('a -> bool) -> 'a t

val set_exn_handler : (exn -> unit) -> unit
(** Set the handler that is called upon an exception in
    a Signal.  The default handler does nothing.
    If the handler raises an exception, it is not caught! *)
