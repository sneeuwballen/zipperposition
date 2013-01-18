(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

open Types
open Hash

(* debugging facilities *)
let debug_level_ = ref 0
let set_debug l = debug_level_ := l
let debug l s =
  if l <= !debug_level_ then begin
    (* cleanup possible progress bar *)
    (if l = 0 then (print_string "\r                                          ";
                    print_string "                                        \r"));
    print_endline (Lazy.force s) 
  end
let debug_level () = !debug_level_

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
      Printf.printf "%% %39s ---------- --------- --------- ---------\n" (String.make 39 '-');
      Printf.printf "%% %-39s %10s %9s %9s %9s\n" "function" "#calls" "total" "max" "average";
      let profilers = List.sort (fun p1 p2 -> String.compare p1.prof_name p2.prof_name) !profilers in
      List.iter
        (fun profiler -> if profiler.prof_calls > 0 then
          (* print content of the profiler *)
          Printf.printf "%% %-39s %10d %9.4f %9.4f %9.4f\n"
            profiler.prof_name profiler.prof_calls profiler.prof_total profiler.prof_max
            (profiler.prof_total /. (float_of_int profiler.prof_calls)))
        profilers
    end)

let rec lexicograph f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x::xs, y::ys ->
     let c = f x y in
     if c <> 0 then c else lexicograph f xs ys
  | [],_ -> ~-1
  | _,[] -> 1

let rec lexicograph_partial f l1 l2 =
  match l1, l2 with
  | [], [] -> Eq
  | x::xs, y::ys ->
    (match f x y with
    | Lt -> Lt | Gt -> Gt | Incomparable -> Incomparable
    | Eq -> lexicograph_partial f xs ys)
  | [], _ | _, [] -> Incomparable

let partial_to_total ord a b = match ord a b with
  | Lt -> -1
  | Gt -> 1
  | Eq | Incomparable -> 0

let total_to_partial ord a b = match ord a b with
  | x when x > 0 -> Gt
  | x when x = 0 -> Eq
  | _ -> Lt

let opposite_order ord a b = - (ord a b)

(** combine two partial comparisons, that are assumed to be
    compatible, ie they do not order differently if
    Incomparable is not one of the values *)
let or_partial cmp1 cmp2 = match cmp1, cmp2 with
  | Eq, Eq
  | Eq, Incomparable | Incomparable, Eq -> Eq
  | Lt, Incomparable | Incomparable, Lt -> Lt
  | Gt, Incomparable | Incomparable, Gt -> Gt
  | Incomparable, Incomparable -> Incomparable
  | _ -> assert false  (* not compatible *)

(** negation of a partial order relation *)
let not_partial cmp = match cmp with
  | Eq | Incomparable -> cmp
  | Lt -> Gt
  | Gt -> Lt

(** remove from l1, l2 elements that compare equal using f. This
    should do a quadratic number of comparisons (at worst, compares
    all elementts of l1 with all elements of l2) *)
let multiset_remove_eq f l1 l2 =
  let rec aux l1 acc1 l2 acc2 = match l1, l2 with
  | [], [] | _, [] | [], _ -> l1 @ acc1, l2 @ acc2
  | x1::xs1, x2::xs2 when f x1 x2 = Eq ->
    aux xs1 acc1 xs2 acc2 (* drop x1 and x2 *)
  | x1::xs1, x2::xs2 ->
    match remove x1 [] xs2, remove x2 [] xs1 with
      | None, None -> aux xs1 (x1::acc1) xs2 (x2::acc2) (* keep both *)
      | Some l2', None -> aux xs1 acc1 l2' (x2::acc2)
      | None, Some l1' -> aux l1' (x1::acc1) xs2 acc2
      | Some l2', Some l1' -> aux l1' acc1 l2' acc2  (* drop both *)
  (* if l contains an element equal to x, returns Some(l')
     where l' is l without this element. Otherwise, None. *)
  and remove x acc l = match l with
  | [] -> None
  | y::ys when f x y = Eq -> Some (acc @ ys)
  | y::ys -> remove x (y :: acc) ys
  in aux l1 [] l2 []

(* check that l1 and l2 are equal multisets under f *)
let multiset_eq f l1 l2 =
  let l1, l2 = multiset_remove_eq f l1 l2 in
  match l1, l2 with
  | [], [] -> true
  | _ -> false

(* naive recursive version, tries all permutations *)
let multiset_partial f l1 l2 = 
  (* first, remove common elements *)
  let l1, l2 = multiset_remove_eq f l1 l2 in
  (* now for a naive Mana and Dershowitz ordering, as presented in
     chapter "paramodulation-based theorem proving" of the
     handbook of automated reasoning. We look for an element that
     dominates the whole other multiset *)
  let rec find_dominating l1' l2' = match l1', l2' with
  | [], [] -> Incomparable
  | x1::xs1, [] -> if dominates x1 l2 then Gt else find_dominating xs1 []
  | [], x2::xs2 -> if dominates x2 l1 then Lt else find_dominating [] xs2
  | x1::xs1, x2::xs2 ->
    let x1_win = dominates x1 l2
    and x2_win = dominates x2 l1 in
    assert ((not x1_win) || (not x2_win));
    if x1_win then Gt else if x2_win then Lt else find_dominating xs1 xs2
  and dominates x l = match l with
  | [] -> true
  | y::ys when f x y = Gt -> dominates x ys
  | _ -> false
  in match l1, l2 with
  | [], [] -> Eq (* all elements removed by multiset_remove_eq *)
  | _ -> find_dominating l1 l2

(* ----------------------------------------------------------------------
 * lists
 * ---------------------------------------------------------------------- *)

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

let rec list_min cmp l =
  match l with
  | [] -> []
  | x::xs when List.exists (fun x' -> cmp x x' = Gt) xs ->
    list_min cmp xs
  | x::xs -> x :: (list_min cmp xs)

let rec list_range low high =
  assert (low <= high);
  match low, high with
  | _, _ when low = high -> []
  | _ -> low :: (list_range (low+1) high)

let rec times i f =
  if i = 0 then []
  else (f ()) :: (times (i-1) f)

(** Randomly shuffle the list. See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let list_shuffle l =
  let a = Array.of_list l in
  for i = 1 to Array.length a - 1 do
    let j = Random.int i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  done;
  Array.to_list a

(* ----------------------------------------------------------------------
 * arrays
 * ---------------------------------------------------------------------- *)

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

(* ----------------------------------------------------------------------
 * misc
 * ---------------------------------------------------------------------- *)

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

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

let on_buffer ?(margin=80) f t =
  let buff = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buff in
  Format.pp_set_margin formatter margin;
  f formatter t;
  Format.fprintf formatter "@?";
  Buffer.contents buff

let sprintf format =
  let buffer = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buffer in
  Format.kfprintf
    (begin fun fmt ->
    Format.pp_print_flush fmt ();
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s
    end)
  fmt
  format

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item formatter = function
  | x::y::xs -> Format.fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

(** print an array of items using the printing function *)
let rec pp_array ?(sep=", ") pp_item formatter a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Format.pp_print_string formatter sep);
    pp_item formatter a.(i)
  done

(** print an array of items using the printing function *)
let rec pp_arrayi ?(sep=", ") pp_item formatter a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Format.pp_print_string formatter sep);
    pp_item formatter i a.(i)
  done
