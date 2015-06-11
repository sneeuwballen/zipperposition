(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Some helpers} *)

(** {2 Time facilities} *)

(** time elapsed since start of program *)
val get_total_time : unit -> float

(** time at which the program started *)
val get_start_time : unit -> float

(** {2 Misc} *)

(** Debug section *)
module Section : sig
  type t

  val full_name : t -> string  (** Full path to the section *)

  val set_debug : t -> int -> unit
  (** Debug level for section (and its descendants) *)

  val clear_debug : t -> unit
  (** Clear debug level (will be same as parent's) *)

  val get_debug : t -> int option
  (** Specific level of this section, if any *)

  val cur_level : t -> int
  (** Current debug level, with parent inheritance *)

  val iter : (string * t) Sequence.t
  (** all registered sections *)

  val root : t (** Default section, with no parent *)
  val logtk : t (** Section for all Logtk-related things *)

  val make : ?parent:t -> ?inheriting:t list -> string -> t
  (** [make ?parent ?inheriting name] makes a new section with the given name.
      It has a parent (default [root]), used to give it a name. It can
      also have a list of sections it inherits from.
      Unless specificed explicitely otherwise (using
      {!set_debug}, the level of the section will be the max level of its
      parent and its inherited sections. *)
end

val set_debug : int -> unit     (** Set debug level of [Section.root] *)
val get_debug : unit -> int     (** Current debug level for [Section.root] *)
val need_cleanup : bool ref     (** Cleanup line before printing? *)

val debug : ?section:Section.t -> int ->
            ('a, Buffer.t, unit, unit) format4 -> 'a
(** Print a debug message, with the given section and verbosity level.
    The message might be dropped if its level is too high.
    {b NOTE}: non-thread safe *)

val debugf : ?section:Section.t -> int ->
            ('a, Format.formatter, unit, unit) format4 -> 'a
(** Same as {!debug} but using {!Format}. Makes multi-line printing
    easier. *)

val pp_pos : Lexing.position -> string

val set_memory_limit : int -> unit
(** Limit the amount of memory available to the process (in MB) *)

val set_time_limit : int -> unit
(** Limit the CPU time available to the process (in seconds) *)

(** {2 OCaml Stack}

    requires [ocaml >= 4.01]

    @since 0.8 *)

module Exn : sig
  val pp_stack : Buffer.t -> int -> unit
  (** printer for the stack with given depth *)

  val fmt_stack : Format.formatter -> int -> unit

  val pp_backtrace : Buffer.t -> unit -> unit
  (** printer for backtraces, if enabled (print nothing otherwise) *)

  val fmt_backtrace : Format.formatter -> unit -> unit
end

(** {2 profiling facilities} *)

type profiler
val enable_profiling : bool ref           (** Enable/disable profiling *)
val mk_profiler : string -> profiler      (** Create a named profiler *)
val enter_prof : profiler -> unit         (** Enter the profiler *)
val exit_prof : profiler -> unit          (** Exit the profiler *)
val yield_prof : profiler -> unit         (** Yield control to sub-call *)

(** {2 Runtime statistics} *)

type stat

val mk_stat : string -> stat
val print_global_stats : unit -> unit
val incr_stat : stat -> unit
val add_stat : stat -> int -> unit

(** {2 Flags as integers} *)

module Flag : sig
  type gen = int ref
    (** Generator of flags *)

  val create : unit -> gen
    (** New generator *)

  val get_new : gen -> int
    (** New flag from the generator (2*previous flag) *)
end

(** {2 LogtkOrdering utils} *)

val lexicograph : ('a -> 'b -> int) -> 'a list -> 'b list -> int
  (** lexicographic order on lists l1,l2 which elements are ordered by f *)

val lexicograph_combine : int list -> int
  (** combine comparisons by lexicographic order *)

(** the opposite order, that sorts elements the opposite way *)
val opposite_order : ('a -> 'b -> int) -> 'a -> 'b -> int

(** {2 String utils} *)

val str_sub : sub:string -> int -> string -> int -> bool
  (** Equality from the given start position *)

val str_split : by:string -> string -> string list

val str_find : ?start:int -> sub:string -> string -> int
  (** Find [sub] in the string, returns its first index or -1 *)

val str_repeat : string -> int -> string
  (** The same char, repeated n times *)

val str_prefix : pre:string -> string -> bool
  (** [str_prefix ~pre s] returns [true] iff [pre] is a prefix of [s] *)

(** {2 Exceptions} *)

val finally : h:(unit -> unit) -> f:(unit -> 'a) -> 'a
  (** [finally h f] calls [f ()] and returns its result. If it raises, the
      same exception is raised; in {b any} case, [h ()] is called after
      [f ()] terminates. *)

(** {2 File utils} *)

val with_lock_file : string -> (unit -> 'a) -> 'a
  (** perform the action with a lock on the given file *)

val with_input : string -> (in_channel -> 'a) -> 'a option
  (** Open the given file for reading, and returns
      the result of the action applied to the input channel *)

val with_output : string -> (out_channel -> 'a) -> 'a option
  (** Open the given file for writing, and returns
      the result of the action applied to the output channel *)

val slurp : in_channel -> string
  (** Read the whole filedescriptor into a string *)

type 'a or_error = [`Error of string | `Ok of 'a]

val popen : cmd:string -> input:string -> string or_error
  (** Run the given command [cmd] with the given [input], wait for it
      to terminate, and return its stdout. *)

(** {2 Printing utils} *)

(** print into a string *)
val sprintf : ('a, Buffer.t, unit, string) format4 -> 'a

val fprintf : out_channel -> ('a, Buffer.t, unit, unit) format4 -> 'a

val printf : ('a, Buffer.t, unit, unit) format4 -> 'a
val eprintf : ('a, Buffer.t, unit, unit) format4 -> 'a
val on_buffer : (Buffer.t -> 'a -> unit) -> 'a -> string

val pp_pair: ?sep:string -> (Buffer.t -> 'a -> unit) ->
              (Buffer.t -> 'b -> unit) -> Buffer.t -> ('a * 'b) -> unit

val pp_opt : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit

(** print a list of items using the printing function *)
val pp_list: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a list -> unit

(** print an array of items with printing function *)
val pp_array: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit

(** print an array, giving the printing function both index and item *)
val pp_arrayi: ?sep:string -> (Buffer.t -> int -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit

(** Print the sequence *)
val pp_seq : ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a Sequence.t -> unit
