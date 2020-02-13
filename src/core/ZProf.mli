
(** {1 Profiler interface} *)

type t

val enable_profiling : bool -> unit (** Enable/disable profiling *)

val make : string -> t (** Create a named profiler *)

val enter_prof : t -> unit (** Enter the profiler *)

val exit_prof : t -> unit (** Exit the profiler *)

val with_prof : t -> ('a -> 'b) -> 'a -> 'b

