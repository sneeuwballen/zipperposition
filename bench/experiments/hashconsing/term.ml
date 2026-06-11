module type S = sig
  type t

  val const : string -> t
  val app : t -> t list -> t
  val compare : t -> t -> int
end

module Make (HC : Hashcons_type.Make) : S = struct
  type t = {
    mutable id: int;
    hash: int;
    view: view;
  }

  and view =
    | Const of string
    | App of t * t list

  let hash_id t = int t.id

  let hash_const_ s = combine2 4 (string s)
  let hash_app_ f l = combine3 10 (hash_id f) (list hash_id l)

  module HCArg = struct
    type nonrec t = t

    let equal (a : t) (b : t) =
      match a.view, b.view with
      | Const s1, Const s2 -> String.equal s1 s2
      | App (f1, l1), App (f2, l2) ->
        f1 == f2
        && List.length l1 = List.length l2
        && List.for_all2 ( == ) l1 l2
      | _ -> false

    let hash (a : t) = a.hash

    let tag i (t' : t) = t'.id <- i
    let n_shards_log2 = 5
    let init_size = 4096
  end

  module H = HC (HCArg)

  let ext_hashcons (x : t) : t =
    (* H.hashcons returns H.elt but H.elt = t by sharing constraint *)
    H.hashcons x

  let const s =
    let h = hash_const_ s in
    ext_hashcons { id = ~-1; hash = h; view = Const s }

  let app f l =
    let h = hash_app_ f l in
    ext_hashcons { id = ~-1; hash = h; view = App (f, l) }

  let compare a b = Stdlib.compare a.id b.id
end
