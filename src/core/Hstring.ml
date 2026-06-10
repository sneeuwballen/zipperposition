type t = {
  str: string;
  mutable id: int;
  h: int;
}

module Hcons = Hashcons.Make (struct
  type nonrec t = t

  let equal a b = a.str = b.str
  let hash a = a.h

  let tag id t =
    assert (t.id = -1);
    t.id <- id

  let n_shards_log2 = 3 (* 8 shards *)
  let init_size = 64
end)

let[@inline] equal (a : t) b = a == b
let[@inline] hash a = a.h
let[@inline] to_string self = self.str

let[@inline] make str =
  let h = Hash.string str in
  Hcons.hashcons { str; h; id = -1 }

let makef fmt = Format.kasprintf make fmt

let compare a b =
  if a.h != b.h then
    CCInt.compare a.h b.h
  else
    CCString.compare a.str b.str

module As_key = struct
  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = hash
end

module Map = CCMap.Make (As_key)
module Set = CCSet.Make (As_key)
module Tbl = CCHashtbl.Make (As_key)
