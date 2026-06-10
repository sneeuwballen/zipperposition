type t = bool Atomic.t

let create () : t = Atomic.make_contended false

let inline_lock (t : t) =
  let continue = ref true in
  while !continue do
    let i = ref 0 in
    while !i < 8 && Atomic.get t do
      incr i
    done;
    let j = ref 0 in
    while !j < 32 && Atomic.get t do
      Domain.cpu_relax ();
      incr j
    done;
    if Atomic.compare_and_set t false true then continue := false
  done

let unlock (t : t) = Atomic.set t false

let with_lock (t : t) (f : unit -> 'a) : 'a =
  inline_lock t;
  match f () with
  | x ->
    unlock t;
    x
  | exception e ->
    unlock t;
    raise e
