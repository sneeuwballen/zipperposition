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

(* {1 Basic S-expressions, with printing and parsing} *)

type t =
  | Atom of string  (** An atom *)
  | List of t list  (** A list of S-expressions *)
  (** S-expression *)

type token = [`Open | `Close | `Atom of string]
  (** Token that compose a Sexpr once serialized *)

val iter : (token -> unit) -> t -> unit
  (** Iterate on the S-expression, calling the callback with tokens *)

val traverse : t -> token Sequence.t
  (** Traverse. This yields a sequence of tokens *)

val lex : in_channel -> token Sequence.t
  (** Lex: create a sequence of tokens from the given in_channel. *)

val of_seq : token Sequence.t -> t
  (** Build a Sexpr from a sequence of tokens, or raise Failure *)

val pp_token : Format.formatter -> token -> unit
  (** Print a token on the given formatter *)

val pp_tokens : Format.formatter -> token Sequence.t -> unit
  (** Print a sequence of Sexpr tokens on the given formatter *)

val pp_sexpr : ?indent:bool -> Format.formatter -> t -> unit
  (** Pretty-print the S-expr. If [indent] is true, the S-expression
      is printed with indentation. *)
  
