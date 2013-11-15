
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

(** {1 De Bruijn environments} *)

type +'a t
  (** An environment that maps De Bruijn indices to values of
      type 'a. *)

val empty : 'a t
  (** Empty environment *)

val is_empty : 'a t -> bool
  (** Are there bindings? *)

val singleton : 'a -> 'a t
  (** Single binding *)

val push : 'a t -> 'a -> 'a t
  (** Create a new environment, when entering a scope, where the De Bruijn
      index 0 is bound to the given value *)

val push_none : 'a t -> 'a t
  (** Create a new environment, when entering a scope, where
      the De Bruijn index 0 is bound to nothing. *)

val push_none_multiple : 'a t -> int -> 'a t
  (** Call [push_none] [n] times (after we've entered [n] scopes, for
      instances) *)

val pop : 'a t -> 'a t
  (** Exit a scope, removing the top binding.
      @raise Invalid_argument if the env is empty *)

val size : 'a t -> int
  (** Number of scopes (number of times {!push} or {!push_none} were
      called to produce the given environement) *)

val find : 'a t -> int -> 'a option
  (** Find to which value the given De Bruijn index is bound to, or
      return None *)
