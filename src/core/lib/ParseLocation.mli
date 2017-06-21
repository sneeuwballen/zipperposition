
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Location in a file} *)

(** A location is a range of characters in file, used for messages
    (error messages, warning, etc.).

    The range is delimited by a start position (line,column) and
    an end position (line, column) + an optional file name.
*)

type t = {
  file : string;
  start_line : int;
  start_column : int;
  stop_line : int;
  stop_column : int;
}

val mk : string -> int -> int -> int -> int -> t

val mk_pair : string -> (int * int) -> (int * int) -> t

val mk_pos : Lexing.position -> Lexing.position -> t

val eq : t -> t -> bool
val hash : t -> int

val combine : t -> t -> t
(** Position that spans the two given positions. The file is assumed to be
    the same in both case, and is chosen from one of the two positions. *)

val combine_list : t list -> t
(** N-ary version of {!combine}.
    @raise Invalid_argument if the list is empty *)

val smaller : t -> t -> bool
(** [smaller p1 p2] is true if [p1] is included in [p2], ie
    [p1] is a sub-location of [p2] (interval inclusion) *)

module Infix : sig
  val (<+>) : t option -> t option -> t option
  (** Combine two optional locations. Left has priority *)
end
include module type of Infix

include Interfaces.PRINT with type t := t

val pp_opt : t option CCFormat.printer

(** {2 Lexbuf} *)

val set_file : Lexing.lexbuf -> string -> unit
(** Change the file name used for positions in this lexbuf *)

val of_lexbuf : Lexing.lexbuf -> t
(** Recover a position from a lexbuf *)
