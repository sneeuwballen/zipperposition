(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Hashconsing} *)

module type HashedType = Logtk_hashcons.HashedType
module type S = Logtk_hashcons.S

module Make (X : HashedType) : S with type elt = X.t = Logtk_hashcons.Make (X)
[@@inline]

(*
module Make (X : HashedType) = struct
  module H = Weak.Make (X)

  type elt = X.t

  let count_ = ref 0
  let tbl : H.t = H.create 4_096

  let hashcons x =
    let x' = H.merge tbl x in
    if x == x' then (
      X.tag !count_ x;
      incr count_
    );
    x'

  let[@inline] mem x = H.mem tbl x

  let fresh_unique_id () =
    let x = !count_ in
    incr count_;
    x

  let stats () = H.stats tbl
end
[@@inline]
*)
