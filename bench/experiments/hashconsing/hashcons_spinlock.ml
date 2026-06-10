module Spinlock : sig
  type t

  val create : unit -> t
  val inline_lock : t -> unit
  val unlock : t -> unit
end = struct
  type t = bool Atomic.t

  let create () : t = Atomic.make_contended false

  let inline_lock (self : t) =
    let continue = ref true in
    while !continue do
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
      );
      if Atomic.compare_and_set self false true then continue := false
    done

  let unlock (self : t) = Atomic.set self false
end

module Make (X : Hashcons_type.HashedType) :
  Hashcons_type.S with type elt = X.t = struct
  let n_shards = 1 lsl X.n_shards_log2
  let shard_mask = n_shards - 1

  module W = Weak.Make (X)

  type elt = X.t

  let locks : Spinlock.t array =
    Array.init n_shards (fun _ -> Spinlock.create ())

  let tbls : W.t array = Array.init n_shards (fun _ -> W.create X.init_size)
  let global_id : int Atomic.t = Atomic.make 0

  let hashcons x =
    let i = X.hash x land max_int land shard_mask in
    let lock = Array.unsafe_get locks i in
    Spinlock.inline_lock lock;
    let x' = W.merge (Array.unsafe_get tbls i) x in
    if x == x' then X.tag (Atomic.fetch_and_add global_id 1) x;
    Spinlock.unlock lock;
    x'

  let fresh_unique_id () = Atomic.fetch_and_add global_id 1
end
