(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Some helpers} *)

(** Various helpers for the provers.

    It provides counters for statistics, basic profilers, helper functions,
    debugging functions…
*)

(** {2 Time facilities} *)

val total_time_s : unit -> float
(** time elapsed since start of program, in seconds *)

val get_time_mon_ : unit -> float
(** A "monotonic" clock (TODO! right now, just unix clock) *)

(** {2 Stand-alone convenience functions} *)

val finally : do_:(unit -> unit) -> (unit -> 'a) -> 'a
(** [finally ~do_ f] calls [f ()] and returns its result. If it raises, the
    same exception is raised; in {b any} case, [do_ ()] is called after
    [f ()] terminates. *)

val pp_pair :
  ?sep:string -> 'a CCFormat.printer -> 'b CCFormat.printer -> ('a * 'b) CCFormat.printer

val pp_list : ?sep:string -> 'a CCFormat.printer -> 'a list CCFormat.printer
(** Print a list without begin/end separators *)

val pp_seq : ?sep:string -> 'a CCFormat.printer -> 'a Seq.t CCFormat.printer
val pp_iter: ?sep:string -> 'a CCFormat.printer -> 'a Iter.t CCFormat.printer

val pp_list0 : ?sep:string -> 'a CCFormat.printer -> 'a list CCFormat.printer
(** Print a list with a whitespace in front if it's non empty, or
    does nothing if the list is empty
    Default separator is " " *)

val tstp_needs_escaping: string -> bool
(** Is this name a proper TSTP identifier, or does it need ' ' around it? *)

val pp_str_tstp : string CCFormat.printer (** possibly escaping *)

val pp_var_tstp : string CCFormat.printer

val ord_option : 'a CCOrd.t -> 'a option CCOrd.t

val for_all_2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** for_all_2 p a b ⟺ length a = length b && for_all2 p a b *)

(* TODO: use containers' at some point *)
val take_drop_while : ('a -> bool) -> 'a list -> 'a list * 'a list

val map_product : f:('a -> 'b list list) -> 'a list -> 'b list list

val seq_map_l : f:('a -> 'b list) -> 'a list -> 'b list Iter.t
val seq_zipi : 'a Iter.t -> (int * 'a) Iter.t

val invalid_argf: ('a, Format.formatter, unit, 'b) format4 -> 'a
val failwithf : ('a, Format.formatter, unit, 'b) format4 -> 'a

module Int_map : CCMap.S with type key = int
module Int_set : CCSet.S with type elt = int

val escape_dot : string -> string
(** String escaping for graphviz *)

val concat_view : string -> ('a -> string) -> 'a list -> string
(** concat_view separator view = String.concat separator ∘ map view *)

val superscript : string -> string
(** Raise all characters of a string to superscripts. Error on unsupported characters; refer to these:
⁽ ⁾ * ⁺ , ⁻ ᐧ ᐟ ⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ : ; ᑉ ⁼ > ˀ @ ᴬ ᴮ ᕪ ᴰ ᴱ ᣘ ᴳ ᴴ ᴵ ᴶ ᴷ ᴸ ᴹ ᴺ ᴼ ᴾ ᶲ ᴿ ᔆ ᵀ ᵁ ⱽ ᵂ ᕁ ˠ ᙆ [ ᐠ ] ᣔ ᗮ ` ᵃ ᵇ ᶜ ᵈ ᵉ ᶠ ᵍ ʰ ⁱ ʲ ᵏ ˡ ᵐ ⁿ ᵒ ᵖ ᵠ ʳ ˢ ᵗ ᵘ ᵛ ʷ ˣ ʸ ᶻ *)

val caller_file_line : int -> string
(** Return string "FILE line LINE" locating the caller at the given depth d. So d=0 tells where you wrote caller_file_line 0, and d=1 tells who called your function, etc. This is based on stack traces that—beware—might omit the relevant call site, e.g. root calls in anonymous functions. *)


(** {2 Debug and error printing} *)

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

  val iter : (string * t) Iter.t
  (** all registered sections *)

  val root : t (** Default section, with no parent *)

  val make : ?parent:t -> ?inheriting:t list -> string -> t
  (** [make ?parent ?inheriting name] makes a new section with the given name.
      It has a parent (default [root]), used to give it a name. It can
      also have a list of sections it inherits from.
      Unless specified explicitly otherwise (using
      {!set_debug}, the level of the section will be the max level of its
      parent and its inherited sections. *)
end

val set_debug : int -> unit (** Set debug level of [Section.root] *)

val get_debug : unit -> int (** Current debug level for [Section.root] *)

val break_on_debug : bool ref
(** Shall we wait for user input after each debug message? *)

val debugf : ?section:Section.t ->
  int ->
  ('a, Format.formatter, unit, unit) format4 ->
  ('a -> unit) ->
  unit
(** Print a debug message, with the given section and verbosity level.
    The message might be dropped if its level is too high. *)

val debug : ?section:Section.t -> int -> string -> unit
(** Cheap non-formatting version of {!debugf} *)

val ksprintf_noc :
  f:(string -> 'a) ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(** Same as [CCFormat.ksprintf], but without colors *)

val err_spf :
  ('b, Format.formatter, unit, string) format4 -> 'b
(** Version of {!sprintf} that adds a colored "error" prefix *)

val warn : string -> unit
(** Emit warning *)

val warnf : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Emit warning, with formatting *)

exception Error of string * string
(** generalist error that do not really belong elsewhere.
    [Error (where,what)] means that error [what] was raised from [where]. *)

val error : where:string -> string -> 'a
(** [error msg] raises [Error msg]
    @raise Error duh *)

val errorf : where:string -> ('b, Format.formatter, unit, 'a) format4 -> 'b
(** Formatting version of {!error} *)

val pp_pos : Lexing.position -> string

module UntypedPrint : sig
(** Adhoc polymorphic debug printing. Usage: open Util.UntypedPrint; then sprinkle ~< or "message"|< in front of expressions you want to trace—often without rebracketing! To register a new string conversion rule edit the file TypeTests.ml. That file and this module are for debugging only: they rely on the unstable Obj (OCaml's reflection) module and are not 100 % safe from segmentation faults. *)
  type any
  (** type for values of forgotten type *)

  val (~<) : 'a -> 'a
  (** Prefix an expression with ~< to trace it together with its location and call chain length. Output can contain oddities due to inlining and lack of runtime types. *)

  val (|<) : string -> 'a -> 'a
  (** Prefix an expression with [message|<] to trace it together with [message], location and call chain length. Output can contain oddities due to inlining and lack of runtime types. *)

  val str : 'a -> string
  (** Adhoc polymorphic to_string *)

  val add_pp : (any -> bool) -> ('a -> string) -> unit
  (** Call add_pp t p registers a string converter p to be used when printing untyped values passing the type test t. Affects str, ~< and |<. The p should accept any input x for which t x is true because otherwise a segmentation fault can occur. Printers registered later take precedence. This function is used in the file TypeTests.ml. *)
end

val set_memory_limit : int -> unit
(** Limit the amount of memory available to the process (in MB) *)

val set_time_limit : int -> unit
(** Limit the CPU time available to the process (in seconds) *)

(** {2 OCaml Stack}

    requires [ocaml >= 4.01]

    @since 0.8 *)

(* TODO: remove? *)

module Exn : sig
  val pp_stack : Buffer.t -> int -> unit
  (** printer for the stack with given depth *)

  val fmt_stack : Format.formatter -> int -> unit

  val pp_backtrace : Buffer.t -> unit -> unit
  (** printer for backtraces, if enabled (print nothing otherwise) *)

  val fmt_backtrace : Format.formatter -> unit -> unit

  val string_of_backtrace : unit -> string
end

(** {2 Runtime statistics} *)

type stat

val mk_stat : string -> stat

val print_global_stats : comment:string -> unit -> unit (** comment prefix *)

val incr_stat : stat -> unit

val add_stat : stat -> int -> unit

val pp_stat : Format.formatter -> stat -> unit


(** {2 Flags as integers} *)

module Flag : sig
  type gen = int ref
  (** Generator of flags *)

  val create : unit -> gen
  (** New generator *)

  val get_new : gen -> int
  (** New flag from the generator (2*previous flag) *)
end

(** {2 File utils} *)

type 'a or_error = ('a, string) CCResult.t

val popen : cmd:string -> input:string -> string or_error
(** Run the given command [cmd] with the given [input], wait for it
    to terminate, and return its stdout. *)
