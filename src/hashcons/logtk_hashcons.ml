module type HashedType = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val tag : int -> t -> unit
end

module type S = sig
  type elt

  val hashcons : elt -> elt
  val mem : elt -> bool
  val fresh_unique_id : unit -> int
  val stats : unit -> int * int * int * int * int * int
end

module Spinlock = struct
  type t = bool Atomic.t

  let create () : t = Atomic.make_contended false

  let spin_some_ (self : t) =
    let i = ref 0 in
    while !i < 8 && Atomic.get self do
      incr i
    done;

    if !i = 8 then (
      let j = ref 0 in
      while !j < 32 && Atomic.get self do
        Domain.cpu_relax ();
        incr j
      done
    )

  let[@inline always] inline_lock (self : t) =
    let continue = ref true in
    while !continue do
      if Atomic.get self then spin_some_ self;
      if Atomic.compare_and_set self false true then continue := false
    done

  let[@inline] unlock (self : t) = Atomic.set self false
end

module Make (X : HashedType) : S with type elt = X.t = struct
  let n_shards = 256
  let shard_mask = 255

  module W = Weak.Make (X)

  type elt = X.t

  let locks : Spinlock.t array =
    Array.init n_shards (fun _ -> Spinlock.create ())

  let tbls : W.t array = Array.init n_shards (fun _ -> W.create 64)
  let global_id : int Atomic.t = Atomic.make 0
  let[@inline] shard_of x = X.hash x land max_int land shard_mask

  let hashcons x =
    let i = shard_of x in
    let lock = Array.unsafe_get locks i in

    Spinlock.inline_lock lock;
    let x' = W.merge (Array.unsafe_get tbls i) x in
    if x == x' then X.tag (Atomic.fetch_and_add global_id 1) x;
    Spinlock.unlock lock;
    x'

  let mem x =
    let i = shard_of x in
    let lock = locks.(i) in
    Spinlock.inline_lock lock;
    let res = W.mem tbls.(i) x in
    Spinlock.unlock lock;
    res

  let fresh_unique_id () = Atomic.fetch_and_add global_id 1

  let stats () =
    let n = ref 0 in
    let b = ref 0 in
    let sb = ref 0 in
    let mb = ref max_int in
    let mxb = ref 0 in
    for i = 0 to n_shards - 1 do
      let lock = locks.(i) in
      Spinlock.inline_lock lock;
      let sn, sbind, ssb, smb, _, smxb = W.stats tbls.(i) in
      n := !n + sn;
      b := !b + sbind;
      sb := !sb + ssb;
      mb := min !mb smb;
      mxb := max !mxb smxb;
      Spinlock.unlock lock
    done;
    !n, !b, !sb, !mb, 0, !mxb
end
[@@inline]
