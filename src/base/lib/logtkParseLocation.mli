
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

(** {1 Location in a file} *)

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
  (** LogtkPosition that spans the two given positions. The file is assumed to be
      the same in both case, and is chosen from one of the two positions. *)

val combine_list : t list -> t
  (** N-ary version of {!combine}.
      @raise Invalid_argument if the list is empty *)

val smaller : t -> t -> bool
  (** [smaller p1 p2] is true if [p1] is included in [p2], ie
      [p1] is a sub-location of [p2] (interval inclusion) *)

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val pp_opt : Buffer.t -> t option -> unit

(** {2 Lexbuf} *)

val set_file : Lexing.lexbuf -> string -> unit
  (** Change the file name used for positions in this lexbuf *)

val of_lexbuf : Lexing.lexbuf -> t
  (** Recover a position from a lexbuf *)
