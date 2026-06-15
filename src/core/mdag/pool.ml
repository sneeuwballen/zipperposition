module A = Atomic

type 'a list_ =
  | Nil
  | Cons of int * 'a * 'a list_

type 'a t = {
  mk_item: unit -> 'a;
  clear: 'a -> unit;
  max_size: int;  (** Max number of items *)
  items: 'a list_ A.t;
}

let create ?(clear = ignore) ~mk_item ?(max_size = 512) () : _ t =
  { mk_item; clear; max_size; items = A.make Nil }

let rec acquire self =
  match A.get self.items with
  | Nil -> self.mk_item ()
  | Cons (_, x, tl) as l ->
    if A.compare_and_set self.items l tl then
      x
    else
      acquire self

let[@inline] size_ = function
  | Cons (sz, _, _) -> sz
  | Nil -> 0

let release self x : unit =
  let rec loop () =
    match A.get self.items with
    | Cons (sz, _, _) when sz >= self.max_size ->
      (* forget the item *)
      ()
    | l ->
      if not (A.compare_and_set self.items l (Cons (size_ l + 1, x, l))) then
        loop ()
  in

  self.clear x;
  loop ()

let with_resource (self : _ t) f =
  let x = acquire self in
  try
    let res = f x in
    release self x;
    res
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    release self x;
    Printexc.raise_with_backtrace e bt

module Raw = struct
  let release = release
  let acquire = acquire
end
