
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

(** {1 Interfaces} *)

module type EQ = sig
  type t
  val eq : t -> t -> bool
end

module type HASH = sig
  include EQ
  val hash : t -> int
end

module type ORD = sig
  type t
  val cmp : t -> t -> int
end

module type PRINT = sig
  type t
  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

module type ITER = sig
  type 'a t

  val to_seq : 'a t -> 'a Sequence.t
  val of_seq : ?init:'a t -> 'a Sequence.t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : ?init:'a t -> 'a list -> 'a t
end

module type SERIALIZABLE = sig
  type t
  
  val bij : t Bij.t
end

module type MONOID = sig
  type t

  val zero : t
  val plus : t -> t -> t
end

module type GROUP = sig
  include MONOID
  val inverse : t -> t
end
