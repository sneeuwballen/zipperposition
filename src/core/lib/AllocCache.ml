
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Cache for Allocations} *)

module Arr = struct
  type 'a bucket = {
    mutable size: int;
    cached: 'a array array;
  }

  type 'a t = 'a bucket array

  let create ?(buck_size=16) n =
    if n<1 then invalid_arg "AllocCache.Arr.create";
    Array.init (n-1)
      (fun _ ->  { size=0; cached=Array.make buck_size [||]})

  let make c i x =
    if i<Array.length c then (
      let buck = c.(i) in
      if buck.size = 0 then Array.make i x
      else (
        (* remove last array *)
        let ret = buck.cached.(buck.size-1) in
        buck.size <- buck.size - 1;
        ret
      )
    ) else Array.make i x

  let free c a =
    if Array.length a < Array.length c then (
      let buck = c.(Array.length a) in
      if buck.size < Array.length buck.cached then (
        (* store [a] *)
        buck.cached.(buck.size) <- a;
        buck.size <- buck.size + 1
      )
    )

  let with_ c i x ~f =
    let a = make c i x in
    try
      let ret = f a in
      free c a;
      ret
    with e ->
      free c a;
      raise e

end

