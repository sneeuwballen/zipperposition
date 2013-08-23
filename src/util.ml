(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Some helpers} *)

(** {2 Time facilities} *)

(** Time elapsed since initialization of the program, and time of start *)
let get_total_time, get_start_time =
  let start = Unix.gettimeofday () in
  (function () ->
    let stop = Unix.gettimeofday () in
    stop -. start),
  (function () -> start)

(** {2 Misc} *)

let clear_line () =
  output_string Pervasives.stdout
    "\r                                                         \r";
  flush Pervasives.stdout


let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let get_debug () = !debug_level_
let need_cleanup = ref false
let debug l format =
  let b = Buffer.create 15 in
  if l <= !debug_level_
    then (
      (if !need_cleanup then clear_line ());
      Printf.bprintf b "[%.3f] " (get_total_time ());
      Printf.kbprintf
        (fun b -> print_endline (Buffer.contents b))
        b format)
    else
      Printf.kbprintf (fun _ -> ()) b format

let pp_pos pos =
  let open Lexing in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

(** {2 Infix operators} *)

module Infix = struct
  let (|>) x f = f x

  let (%>) f g = fun x -> g (f x)

  let (%%) f g = fun x -> f (g x)
end

(** {2 profiling facilities} *)

(** A profiler (do not call recursively) *)
type profiler = {
  prof_name : string;
  mutable prof_total : float;   (** total time *)
  mutable prof_calls : int;     (** number of calls *)
  mutable prof_max : float;     (** max time in the profiled function *)
  mutable prof_enter : float;   (** time at which we entered the profiler *)
}

(** Global switch for profiling *)
let enable_profiling = ref false

(** all profilers *)
let profilers = ref []

(** create a named profiler *)
let mk_profiler name =
  let prof = {
    prof_enter = 0.;
    prof_name = name;
    prof_total = 0.;
    prof_calls = 0;
    prof_max = 0.;
  } in
  (* register profiler *)
  profilers := prof :: !profilers;
  prof

(** Enter the profiler *)
let enter_prof profiler =
  if !enable_profiling then profiler.prof_enter <- Unix.gettimeofday ()

(** Exit the profiled code with a value *)
let exit_prof profiler =
  if !enable_profiling then begin
    let stop = Unix.gettimeofday () in
    let delta = stop -. profiler.prof_enter in
    profiler.prof_total <- profiler.prof_total +. delta;
    profiler.prof_calls <- profiler.prof_calls + 1;
    (if delta > profiler.prof_max then profiler.prof_max <- delta);
  end

(** Print profiling data upon exit *)
let () =
  at_exit (fun () ->
    if !enable_profiling && List.exists (fun profiler -> profiler.prof_calls > 0) !profilers
    then begin
      Printf.printf "%% %39s ---------- --------- --------- --------- ---------\n"
        (String.make 39 '-');
      Printf.printf "%% %-39s %10s %9s %9s %9s %9s\n" "function" "#calls"
        "total" "% total" "max" "average";
      (* sort profilers by name *)
      let profilers = List.sort
        (fun p1 p2 -> String.compare p1.prof_name p2.prof_name)
        !profilers in
      let tot = get_total_time () in
      List.iter
        (fun profiler -> if profiler.prof_calls > 0 then
          (* print content of the profiler *)
          Printf.printf "%% %-39s %10d %9.4f %9.2f %9.4f %9.4f\n"
            profiler.prof_name profiler.prof_calls profiler.prof_total
            (profiler.prof_total *. 100. /. tot) profiler.prof_max
            (profiler.prof_total /. (float_of_int profiler.prof_calls)))
        profilers
    end)

(** {2 Runtime statistics} *)

type stat = string * int64 ref 

let mk_stat, print_global_stats =
  let stats = ref [] in
  (* create a stat *)
  (fun name ->
    let stat = (name, ref 0L) in
    stats := stat :: !stats;
    stat),
  (* print stats *)
  (fun () ->
    List.iter
      (fun (name, cnt) -> Format.printf "%% %-30s ... %s@." name (Int64.to_string !cnt))
      !stats)

let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)
let add_stat (_, count) num = count := Int64.add !count (Int64.of_int num) (** add to stat *)

(** {2 Ordering utils} *)

let rec lexicograph f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x::xs, y::ys ->
     let c = f x y in
     if c <> 0 then c else lexicograph f xs ys
  | [],_ -> (-1)
  | _,[] -> 1

(** combine comparisons by lexicographic order *)
let rec lexicograph_combine l = match l with
  | [] -> 0
  | cmp::l' -> if cmp = 0 then lexicograph_combine l' else cmp

let opposite_order ord a b = - (ord a b)

(** {2 List utils} *)

let rec list_get l i = match l, i with
  | [], i -> raise Not_found
  | x::_, i when i = 0 -> x
  | _::xs, i when i > 0 -> list_get xs (i-1)
  | _ -> failwith "error in list_get"

let rec list_set l i elem = match l, i with
  | [], i -> invalid_arg "index too high"
  | _::xs, i when i = 0 -> elem::xs
  | x::xs, i when i > 0 -> x::(list_set xs (i-1) elem)
  | _ -> failwith "error in list_set"

let list_mapi l f =
  let rec recurse l i =
    match l with
    | [] -> []
    | x::l' -> f i x :: recurse l' (i+1)
  in recurse l 0

let list_iteri l f =
  let rec recurse l i =
    match l with
    | [] -> ()
    | x::l' -> f i x; recurse l' (i+1)
  in recurse l 0

let rec list_remove l i = match l, i with
  | [], i -> invalid_arg "index too high"
  | _::xs, i when i = 0 -> xs
  | x::xs, i when i > 0 -> x::(list_remove xs (i-1))
  | _ -> failwith "error in list_remove"

let list_pos l =
  let rec aux l idx = match l with
  | [] -> []
  | x::xs -> (x,idx) :: (aux xs (idx+1))
  in
  aux l 0

let rec list_mem comp x l = match l with
  | [] -> false
  | y::ys when comp x y -> true
  | _::ys -> list_mem comp x ys

let rec list_subset cmp l1 l2 =
  List.for_all
    (fun t -> list_mem cmp t l2)
    l1

let rec list_uniq eq l = match l with
  | [] -> []
  | x::xs when List.exists (eq x) xs -> list_uniq eq xs
  | x::xs -> x :: list_uniq eq xs

let list_merge comp l1 l2 =
  let rec recurse l1 l2 = match l1,l2 with
  | [], _ -> l2
  | _, [] -> l1
  | x::l1', y::l2' ->
    let cmp = comp x y in
    if cmp < 0 then x::(recurse l1' l2)
    else if cmp > 0 then y::(recurse l1 l2')
    else x::(recurse l1' l2')
  in
  List.rev (recurse l1 l2)

let rec list_union comp l1 l2 = match l1 with
  | [] -> l2
  | x::xs when list_mem comp x l2 -> list_union comp xs l2
  | x::xs -> x::(list_union comp xs l2)

let rec list_inter comp l1 l2 = match l1 with
  | [] -> []
  | x::xs when list_mem comp x l2 -> x::(list_inter comp xs l2)
  | _::xs -> list_inter comp xs l2

let list_find p l =
  let rec search i l = match l with
  | [] -> None
  | x::_ when p x -> Some (i, x)
  | _::xs -> search (i+1) xs
  in search 0 l

let list_fmap f l =
  let rec recurse acc l = match l with
  | [] -> List.rev acc
  | x::l' ->
    let acc' = match f x with | None -> acc | Some y -> y::acc in
    recurse acc' l'
  in recurse [] l

let list_flatmap f l =
  let rec recurse acc l = match l with
  | [] -> List.rev acc
  | x::l' -> recurse (List.rev_append (f x) acc) l'
  in recurse [] l

let rec list_take n l = match n, l with
  | 0, _ -> []
  | _, [] -> []
  | _, x::xs when n > 0 -> x :: (list_take (n-1) xs)
  | _ -> assert false

let rec list_drop n l = match n, l with
  | 0, _ -> l
  | n, [] -> []
  | n, _::l' -> (assert (n > 0); list_drop (n-1) l')

let rec list_range low high =
  assert (low <= high);
  match low, high with
  | _, _ when low = high -> []
  | _ -> low :: (list_range (low+1) high)

let rec times i f =
  if i = 0 then []
  else (f ()) :: (times (i-1) f)

(** Randomly shuffle the array, in place.
    See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let array_shuffle a = 
  for i = 1 to Array.length a - 1 do
    let j = Random.int i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  done

(** Randomly shuffle the list *)
let list_shuffle l =
  let a = Array.of_list l in
  array_shuffle a;
  Array.to_list a

(** {2 Array utils} *)

let array_foldi f acc a =
  let rec recurse acc i =
    if i = Array.length a then acc else recurse (f acc i a.(i)) (i+1)
  in recurse acc 0

let array_forall p a =
  let rec check i =
    if i = Array.length a then true else p a.(i) && check (i+1)
  in check 0

let array_forall2 p a1 a2 =
  let rec check i =
    if i = Array.length a1 then true else p a1.(i) a2.(i) && check (i+1)
  in
  if Array.length a1 <> Array.length a2
    then raise (Invalid_argument "array_forall2")
    else check 0

let array_exists p a =
  let rec check i =
    if i = Array.length a then false else p a.(i) || check (i+1)
  in check 0

(** all the elements of a, but the i-th, into a list *)
let array_except_idx a i =
  array_foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

(** {2 Finite bijections} *)

module Bijection(X : Hashtbl.HashedType) = struct
  type elt = X.t
  module H = Hashtbl.Make(X)
  type t = elt H.t

  let of_array cur next =
    assert (Array.length cur = Array.length next);
    let h = H.create (Array.length cur) in
    (* build the hashtable *)
    for i = 0 to Array.length cur - 1 do
      let x = cur.(i) and y = next.(i) in
      (if H.mem h x then failwith "Util.Bijection: duplicate element");
      H.add h x y
    done;
    h
  
  (* check it's a bijutation *)
  let is_permutation b =
    try
      H.iter (fun x y -> if not (H.mem b y) then raise Exit) b;
      true
    with Exit -> false

  let of_list cur next =
    of_array (Array.of_list cur) (Array.of_list next)

  let apply bij x =
    try H.find bij x
    with Not_found -> x  (* fixpoint *)

  let apply_list bij l =
    List.map (fun x -> apply bij x) l

  let apply_strict bij x = H.find bij x

  let compose p1 p2 =
    let h = H.create (H.length p1) in
    (* image of elements of p2 *)
    H.iter
      (fun x y ->
        try
          let z = H.find p1 y in
          H.add h x z  (* composition *)
        with Not_found ->
          H.add h x y)
      p2;
    (* elements of [p1] that [p2] ignored *)
    H.iter
      (fun y z -> if H.mem h y then () else H.add h y z)
      p1;
    h

  (* naive composition of a list *)
  let compose_list l =
    let id = H.create 3 in
    List.fold_right (fun p right -> compose p right) l id

  (* reverse function (composition = id) *)
  let rev b =
    let h = H.create (H.length b) in
    H.iter (fun x y -> H.add h y x) b;
    h
  
  let to_list p =
    H.fold (fun x y l -> (x,y)::l) p []
end

(** {2 String utils} *)

let str_sub ~sub i s j =
  let rec check k =
    if i + k = String.length sub
      then true
      else sub.[i + k] = s.[j+k] && check (k+1)
  in
  check 0

(* note: quite inefficient *)
let str_split ~by s =
  let len_by = String.length by in
  assert (len_by > 0);
  let l = ref [] in
  let n = String.length s in
  let rec search prev i =
    if i >= n
      then List.rev (String.sub s prev (n-prev) :: !l)  (* done *)
    else if is_prefix i 0
      then begin
        l := (String.sub s prev (i-prev)) :: !l;  (* save substring *)
        search (i+len_by) (i+len_by)
      end
    else search prev (i+1)
  and is_prefix i j =
    if j = len_by
      then true
    else if i = n
      then false
    else s.[i] = by.[j] && is_prefix (i+1) (j+1)
  in search 0 0

(* note: inefficient *)
let str_find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  try
    while !i + n < String.length s do
      (if str_sub sub 0 s !i then raise Exit);
      incr i
    done;
    -1
  with Exit ->
    !i

(** {2 File utils} *)

let with_lock_file filename action =
  let lock_file = Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
  Unix.lockf lock_file Unix.F_LOCK 0;
  try
    let x = action () in
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    x
  with e ->
    Unix.lockf lock_file Unix.F_ULOCK 0;
    Unix.close lock_file;
    raise e

let with_input filename action =
  try
    let ic = open_in filename in
    (try
      let res = Some (action ic) in
      close_in ic;
      res
    with Sys_error _ ->
      close_in ic;
      None)
  with Sys_error _ ->
    None

let with_output filename action =
  try
    let oc = open_out filename in
    (try
      let res = Some (action oc) in
      close_out oc;
      res
    with Sys_error s ->
      close_out oc;
      None)
  with Sys_error s ->
    None

(** {2 Printing utils} *)

let sprintf format =
  let buffer = Buffer.create 512 in
  Printf.kbprintf
    (begin fun fmt ->
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s
    end)
  buffer
  format

let fprintf oc format =
  let buffer = Buffer.create 512 in
  Printf.kbprintf
    (fun fmt -> Buffer.output_buffer oc buffer)
  buffer
  format

let printf format = fprintf stdout format
let eprintf format = fprintf stderr format

let on_buffer pp x =
  let buf = Buffer.create 24 in
  pp buf x;
  Buffer.contents buf

let pp_pair ?(sep=" ") px py buf (x,y) =
  px buf x;
  Buffer.add_string buf sep;
  py buf y

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item buf = function
  | x::((y::xs) as l) ->
    pp_item buf x;
    Buffer.add_string buf sep;
    pp_list ~sep pp_item buf l
  | x::[] -> pp_item buf x
  | [] -> ()

(** print an array of items using the printing function *)
let rec pp_array ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf a.(i)
  done

(** print an array of items using the printing function *)
let rec pp_arrayi ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf i a.(i)
  done

(** Print the sequence *)
let rec pp_seq ?(sep=", ") pp_item buf seq =
  Sequence.iteri
    (fun i x ->
      (if i > 0 then Buffer.add_string buf sep);
      pp_item buf x)
    seq
