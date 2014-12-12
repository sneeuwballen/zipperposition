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

(** {1 LogtkHashconsing} *)

module type HashedLogtkType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
end

module type S = sig
  type elt
    (** LogtkHashconsed objects *)

  module H : Weak.S with type data = elt

  type t
    (** LogtkHashconsing table *)

  val default : t
    (** default hashconsing table *)

  val weak_of : t -> H.t
    (** Get the underlying weak table *)

  val hashcons : ?table:t -> elt -> elt
    (** LogtkHashcons the elements *)

  val mem : ?table:t -> elt -> bool
    (** Is the element present in this table? *)

  val fresh_unique_id : ?table:t -> unit -> int
    (** Unique ID that will never occur again in this table (modulo 2^63...) *)

  val stats : ?table:t -> unit -> int*int*int*int*int*int
end

module Make(X : HashedLogtkType) = struct
  module H = Weak.Make(X)

  type t = {
    mutable count : int;
    tbl : H.t
  } (** LogtkHashconsing table *)

  type elt = X.t
    (** LogtkHashconsed objects *)

  (** default hashconsing table *)
  let default = {
    count = 0;
    tbl = H.create 1024
  }

  let weak_of t = t.tbl

  (** LogtkHashcons the elements *)
  let hashcons ?(table=default) x =
    let x' = H.merge table.tbl x in
    (if x == x'
      then begin
        X.tag table.count x;
        table.count <- table.count + 1
      end);
    x'

  let mem ?(table=default) x =
    H.mem table.tbl x

  let fresh_unique_id ?(table=default) () =
    let x = table.count in
    table.count <- x+1;
    x

  let stats ?(table=default) () =
    H.stats table.tbl
end
