module Make (X : Hashcons_type.HashedType) :
  Hashcons_type.S with type elt = X.t = struct
  module W = Weak.Make (X)

  type elt = X.t

  let count_ : int ref = ref 0
  let tbl : W.t = W.create X.init_size

  let hashcons x =
    let x' = W.merge tbl x in
    if x == x' then (
      X.tag !count_ x;
      incr count_
    );
    x'

  let fresh_unique_id () =
    let x = !count_ in
    incr count_;
    x
end
[@@inline]
