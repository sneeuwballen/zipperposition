module Make (X : Hashcons_type.HashedType) :
  Hashcons_type.S with type elt = X.t = struct
  module H = Hashtbl.Make (X)

  type elt = X.t

  let count_ : int ref = ref 0
  let tbl : elt H.t = H.create X.init_size

  let hashcons x =
    try H.find tbl x
    with Not_found ->
      X.tag !count_ x;
      incr count_;
      H.add tbl x x;
      x

  let fresh_unique_id () =
    let x = !count_ in
    incr count_;
    x
end
[@@inline]
