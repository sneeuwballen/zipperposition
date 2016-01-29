
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Hashconsing} *)

module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
end

module type S = sig
  type elt
  (** Hashconsed objects *)

  val hashcons : elt -> elt
  (** Hashcons the elements *)

  val mem : elt -> bool
  (** Is the element present in this table? *)

  val fresh_unique_id : unit -> int
  (** Unique ID that will never occur again in this table (modulo 2^63...) *)

  val stats : unit -> int*int*int*int*int*int
end

module Make(X : HashedType) = struct
  module H = Weak.Make(X)
  type elt = X.t

  let count_ = ref 0
  let tbl : H.t = H.create 1024

  let hashcons x =
    let x' = H.merge tbl x in
    if x == x' then (
      X.tag !count_ x;
      incr count_;
    );
    x'

  let mem x = H.mem tbl x

  let fresh_unique_id () =
    let x = !count_ in
    incr count_;
    x

  let stats () = H.stats tbl
end

module MakeNonWeak(X : HashedType) = struct
  module H = Hashtbl.Make(X)
  type elt = X.t

  let count_ = ref 0
  let tbl : elt H.t = H.create 1024

  let hashcons x =
    try H.find tbl x
    with Not_found ->
      X.tag !count_ x;
      incr count_;
      H.add tbl x x;
      x

  let mem x = H.mem tbl x

  let fresh_unique_id () =
    let x = !count_ in
    incr count_;
    x

  let stats () =
    let stat = H.stats tbl in
    let open Hashtbl in
    let sum_buck = Array.fold_left (+) 0 stat.bucket_histogram in
    let min_buck = Array.fold_left min max_int stat.bucket_histogram in
    stat.num_buckets, stat.num_bindings, sum_buck,
    min_buck, 0, stat.max_bucket_length
end
