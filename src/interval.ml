
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Intervals on integers} *)

open Logtk

(** {2 Signature a numberic type needs providing} *)

module type NUM = sig
  type t

  val compare : t -> t -> int
    (** Total order on numbers *)

  val pp : Buffer.t -> t -> unit
    (** Print into buffer *)
end

module type S = sig
  module N : NUM
    (** Number the interval is composed of *)

  type t

  val empty : t
    (** Empty interval. It doesn't contain any value *)

  val all : t
    (** Interval that contains all numbers *)

  val lt : N.t -> t
    (** Values strictly lower than this number *)

  val leq : N.t -> t
    (** Values lower or equal than this number *)

  val gt : N.t -> t
    (** Values greater than this number *)

  val geq : N.t -> t
    (** Infinite interval of all values bigger or equal than the number *)

  val range : N.t -> N.t -> t
    (** Make an interval that ranges from [low] to [high], both included. *)

  val mem : t -> N.t -> bool
    (** Is the number part of the interval? *)

  val inter : t -> t -> t
    (** Intersection of two intervals. *)

  val union : t -> t -> t
    (** Union of two intervals. This may lose precision if the two intervals
        are non-overlapping (e.g., [1,2] union [3,4] will give [1,4]. *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string

  val arbitrary : N.t QCheck.Arbitrary.t -> t QCheck.Arbitrary.t
end

module Make(N : NUM) = struct
  module N = N

  type bound =
    | Infinite
    | Strict of N.t
    | NonStrict of N.t

  type t =
    | Empty
    | Range of bound * bound

  let empty = Empty

  let all = Range (Infinite, Infinite)

  let lt x = Range (Infinite, Strict x)

  let leq x = Range (Infinite, NonStrict x)

  let gt x = Range (Strict x, Infinite)

  let geq x = Range (NonStrict x, Infinite)

  let range a b = match N.compare a b with
    | 0 -> Range (NonStrict a, NonStrict a)
    | n when n < 0 -> Range (NonStrict a, NonStrict b)
    | n -> (* a > b *) Empty

  (* x bigger than this lower bound? *)
  let _gt bound x = match bound with
    | Infinite -> true
    | Strict l -> N.compare l x < 0
    | NonStrict l -> N.compare l x <= 0

  (* x lower than this higher bound? *)
  let _lt bound x = match bound with
    | Infinite -> true
    | Strict h -> N.compare h x > 0
    | NonStrict h -> N.compare h x >= 0

  let mem i x = match i with
    | Empty -> false
    | Range (low, high) -> _gt low x && _lt high x

  (* assuming a and b contain the same number, takes the most strict bound *)
  let _most_strict a b = match a, b with
    | Strict _, _ -> a
    | _, Strict _ -> b
    | NonStrict _, NonStrict _ -> a (* arbitrary *)
    | _ -> assert false

  let _least_strict a b = match a, b with
    | NonStrict _, _ -> a
    | _, NonStrict _ -> b
    | Strict _, Strict _ -> a (* arbitrary *)
    | _ -> assert false

  (* max of the two lower bounds *)
  let _low_max a b = match a, b with
    | Infinite, _ -> b
    | _, Infinite -> a
    | (Strict na | NonStrict na), (Strict nb | NonStrict nb)->
      begin match N.compare na nb with
      | 0 -> _most_strict a b
      | n when n < 0 -> b
      | _ -> a  (* na > nb *)
      end

  (* min of the two lower bounds *)
  let _low_min a b = match a, b with
    | Infinite, _ -> a
    | _, Infinite -> b
    | (Strict na | NonStrict na), (Strict nb | NonStrict nb)->
      begin match N.compare na nb with
      | 0 -> _least_strict a b
      | n when n < 0 -> a
      | _ -> b  (* na > nb *)
      end

  (* max of the two higher bounds *)
  let _high_max a b = match a, b with
    | Infinite, _
    | _, Infinite -> Infinite
    | (Strict na | NonStrict na), (Strict nb | NonStrict nb)->
      begin match N.compare na nb with
      | 0 -> _least_strict a b
      | n when n < 0 -> b
      | _ -> a
      end

  let _high_min a b = match a, b with
    | Infinite, _ -> b
    | _, Infinite -> a
    | (Strict na | NonStrict na), (Strict nb | NonStrict nb)->
      begin match N.compare na nb with
      | 0 -> _most_strict a b
      | n when n < 0 -> a
      | _ -> b
      end

  let inter a b = match a, b with
    | Empty, _
    | _, Empty -> Empty
    | Range (la, ha), Range (lb, hb) ->
      let l = _low_max la lb in
      let h = _high_min ha hb in
      match l, h with
      | NonStrict n1, Strict n2 when N.compare n1 n2 >= 0 -> Empty
      | ((Strict n1 | NonStrict n1), (Strict n2 | NonStrict n2))
        when N.compare n1 n2 > 0 -> Empty
      | _ -> Range (l, h)

  let union a b = match a, b with
    | Empty, _ -> b
    | _, Empty -> a
    | Range (la, ha), Range (lb, hb) ->
      Range (_low_min la lb, _high_max ha hb)

  let _pp_bound_low buf b = match b with
    | Infinite -> Buffer.add_string buf "]-∞"
    | Strict n -> Printf.bprintf buf "]%a" N.pp n
    | NonStrict n -> Printf.bprintf buf "[%a" N.pp n

  let _pp_bound_high buf b = match b with
    | Infinite -> Buffer.add_string buf "+∞]"
    | Strict n -> Printf.bprintf buf "%a[" N.pp n
    | NonStrict n -> Printf.bprintf buf "%a]" N.pp n

  let pp buf a = match a with
    | Empty -> Buffer.add_string buf "Ø"
    | Range (l, h) ->
      _pp_bound_low buf l;
      Buffer.add_string buf ", ";
      _pp_bound_high buf h

  let to_string a = Util.on_buffer pp a

  let arbitrary ar =
    QCheck.Arbitrary.(
    choose
      [ lift lt ar
      ; lift gt ar
      ; lift leq ar
      ; lift geq ar
      ; lift2 range ar ar
      ; among [empty; all]
      ])
end

module Int = Make(struct
  type t = Big_int.big_int

  let compare = Big_int.compare_big_int

  let pp buf i = Buffer.add_string buf (Big_int.string_of_big_int i)
end)

module Rat = Make(struct
  type t = Ratio.ratio

  let compare = Ratio.compare_ratio

  let pp buf n = Buffer.add_string buf (Ratio.string_of_ratio n)
end)

module Real = Make(struct
  type t = float

  let compare = Pervasives.compare

  let pp buf f = Printf.bprintf buf "%.4f" f
end)

