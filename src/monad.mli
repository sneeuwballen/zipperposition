
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

(** {1 Utils linked to monads} *)

(** {2 Signature of a monad} *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t
    (** Functorial map *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(** {2 Option Monad} *)

module Opt : sig
  include S with type 'a t = 'a option

  val maybe : 'a option -> 'a -> 'a
    (** Safe extraction from an option *)

  val get : 'a option -> 'a
    (** Get the content of the option.
        @raise Invalid_argument if the option is None *)

  val is_some : _ option -> bool
  val is_none : _ option -> bool
end

(** {2 List monad} *)

module L : sig
  include S with type 'a t = 'a list
end

(** {2 Error monad} *)

module Err : sig
  type 'a err =
    | Ok of 'a
    | Error of string

  include S with type 'a t = 'a err

  val guard : ?print:(exn -> string) -> (unit -> 'a) -> 'a t
    (** [guard f] returns [Ok (f x)] if [f x] evaluates without raising
        an exception, or [Error msg] if [f x] raises [e] and [msg] is the
        string representation of [e]. An exception printer can be
        provided (by default {!Printexc.to_string} is used) *)

  val fail : string -> 'a t
  val fail_exn : exn -> 'a t
end

(** {2 Monadic traversal}
This functor allows to build fold and map functions with a monadic interface.
*)

module type TRAVERSE = sig
  module M : S
    (** monad used for traversal *)

  val fold : 'a Sequence.t -> 'b M.t -> ('b -> 'a -> 'b M.t) -> 'b M.t
  val fold_l : 'a list -> 'b M.t -> ('b -> 'a -> 'b M.t) -> 'b M.t

  val map_l : 'a list -> ('a -> 'b M.t) -> 'b list M.t
end

module Traverse(M : S) : TRAVERSE with module M = M

module TraverseOpt : TRAVERSE with type 'a M.t = 'a option
module TraverseErr : TRAVERSE with type 'a M.t = 'a Err.t
