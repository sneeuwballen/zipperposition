(** {1 Profiler interface} *)

type t

val enable_profiling : bool -> unit
(** Enable/disable profiling *)

val make : string -> t
(** Create a named profiler *)

type span

val enter_prof : t -> span
(** Enter the profiler *)

val exit_prof : span -> unit
(** Exit the profiler *)

val with_prof : t -> ('a -> 'b) -> 'a -> 'b
val message : (unit -> string) -> unit
val setup : unit -> unit
