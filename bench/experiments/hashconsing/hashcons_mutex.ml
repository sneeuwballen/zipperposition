module Make (X : Hashcons_type.HashedType) :
  Hashcons_type.S with type elt = X.t = struct
  let n_shards = 1 lsl X.n_shards_log2
  let shard_mask = n_shards - 1

  module W = Weak.Make (X)

  type elt = X.t

  let locks : Mutex.t array = Array.init n_shards (fun _ -> Mutex.create ())
  let tbls : W.t array = Array.init n_shards (fun _ -> W.create X.init_size)
  let global_id : int Atomic.t = Atomic.make 0

  let hashcons x =
    let i = X.hash x land max_int land shard_mask in
    let lock = Array.unsafe_get locks i in
    Mutex.lock lock;
    match W.merge (Array.unsafe_get tbls i) x with
    | x' ->
      if x == x' then X.tag (Atomic.fetch_and_add global_id 1) x;
      Mutex.unlock lock;
      x'
    | exception e ->
      Mutex.unlock lock;
      raise e

  let fresh_unique_id () = Atomic.fetch_and_add global_id 1
end
