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
  val logtk : t (** Section for all -related things *)

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

val debugf : ?section:Section.t ->
             int ->
             ('a, Format.formatter, unit, unit) format4 ->
             ('a -> unit) ->
             unit
(** Print a debug message, with the given section and verbosity level.
    The message might be dropped if its level is too high. *)

val debug : ?section:Section.t -> int -> string -> unit
(** Cheap non-formatting version of {!debugf} *)

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

(** {2 Others} *)

val finally : do_:(unit -> unit) -> (unit -> 'a) -> 'a
  (** [finally ~do_ f] calls [f ()] and returns its result. If it raises, the
      same exception is raised; in {b any} case, [do_ ()] is called after
      [f ()] terminates. *)

val pp_pair :
  ?sep:string -> 'a CCFormat.printer -> 'b CCFormat.printer -> ('a * 'b) CCFormat.printer

val pp_list : ?sep:string -> 'a CCFormat.printer -> 'a list CCFormat.printer
(** Print a list without begin/end separators *)

val ord_option : 'a CCOrd.t -> 'a option CCOpt.t

(** {2 File utils} *)

type 'a or_error = [`Error of string | `Ok of 'a]

val popen : cmd:string -> input:string -> string or_error
  (** Run the given command [cmd] with the given [input], wait for it
      to terminate, and return its stdout. *)
