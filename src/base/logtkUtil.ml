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

(** Debug section *)
module Section = struct
  let null_level = -1 (* absence of level *)
  type t = {
    descr : descr;
    mutable full_name : string;
    mutable level : int;
  }
  and descr =
    | Root
    | Sub of string * t * t list  (* name, parent, inheriting *)

  let root={descr=Root; full_name=""; level=0; }

  (* computes full name of section *)
  let compute_full_name s =
    let buf = Buffer.create 15 in
    let rec add s = match s.descr with
      | Root -> true
      | Sub (name, parent, _) ->
          let parent_is_root = add parent in
          if not parent_is_root then Buffer.add_char buf '.';
          Buffer.add_string buf name;
          false
    in
    ignore (add s);
    Buffer.contents buf

  let full_name s = s.full_name

  (* full name -> section *)
  let section_table = Hashtbl.create 15

  let set_debug s i = assert (i>=0); s.level <- i
  let clear_debug s = s.level <- null_level
  let get_debug s =
    if s.level=null_level then None else Some s.level

  let make ?(parent=root) ?(inheriting=[]) name =
    if name="" then invalid_arg "Section.make: empty name";
    let sec = {
      descr=Sub(name, parent, inheriting);
      full_name="";
      level=null_level;
    } in
    let name' = compute_full_name sec in
    try
      Hashtbl.find section_table name'
    with Not_found ->
      (* new section! register it, add an option to set its level *)
      sec.full_name <- name';
      Hashtbl.add section_table name' sec;
      sec

  let logtk = make "logtk"

  let iter yield =
    yield ("", root);
    Hashtbl.iter (fun name sec -> yield (name,sec)) section_table

  (* recursive lookup, with inheritance from parent *)
  let rec cur_level_rec s =
    if s.level = null_level
    then match s.descr with
      | Root -> 0
      | Sub (_, parent, []) -> cur_level_rec parent
      | Sub (_, parent, [i]) -> max (cur_level_rec parent) (cur_level_rec i)
      | Sub (_, parent, inheriting) ->
          List.fold_left
            (fun m i -> max m (cur_level_rec i))
            (cur_level_rec parent) inheriting
    else s.level

  (* inlinable function *)
  let cur_level s =
    if s.level = null_level
      then cur_level_rec s
      else s.level
end

let set_debug = Section.set_debug Section.root
let get_debug () = Section.root.Section.level
let need_cleanup = ref false

let debug_buf_ = Buffer.create 32 (* shared buffer (not thread safe)  *)
let debug_fmt_ = Format.std_formatter

let debug ?(section=Section.root) l format =
  if l <= Section.cur_level section
    then (
      Buffer.clear debug_buf_;
      if !need_cleanup then clear_line ();
      (* print header *)
      let now = get_total_time () in
      if section == Section.root
        then Printf.bprintf debug_buf_ "%% [%.3f] " now
        else Printf.bprintf debug_buf_ "%% [%.3f %s] "
          now section.Section.full_name;
      Printf.kbprintf
        (fun b -> Buffer.output_buffer stdout b; print_char '\n'; flush stdout)
        debug_buf_ format)
    else
      Printf.ifprintf debug_buf_ format

let debugf ?(section=Section.root) l msg =
  if l <= Section.cur_level section
    then (
      let now = get_total_time () in
      if section == Section.root
        then Format.fprintf debug_fmt_ "@[<hov>%% [%.3f] " now
        else Format.fprintf debug_fmt_ "@[<hov>%% [%.3f %s] "
          now section.Section.full_name;
        Format.kfprintf
          (fun fmt -> Format.fprintf fmt "@]@.")
          debug_fmt_ msg
    )
    else
      Format.ifprintf debug_fmt_ msg

let pp_pos pos =
  let open Lexing in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

external set_memory_limit_stub : int -> unit = "logtk_set_memory_limit"

let set_memory_limit n =
  if n <= 0 then invalid_arg "set_memory_limit: expect positive arg";
  debug 1 "limit memory to %d MB" n;
  set_memory_limit_stub n

external set_time_limit_stub : int -> unit = "logtk_set_time_limit"

let set_time_limit n =
  if n <= 0 then invalid_arg "set_time_limit: expect positive arg";
  debug 1 "limit time to %ds" n;
  set_time_limit_stub n

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

(* difference with [exit_prof]: does not increment the total count *)
let yield_prof profiler =
  if !enable_profiling then begin
    let stop = Unix.gettimeofday () in
    let delta = stop -. profiler.prof_enter in
    profiler.prof_total <- profiler.prof_total +. delta;
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
    let stats = List.sort (fun (n1,_)(n2,_) -> String.compare n1 n2) !stats in
    List.iter
      (fun (name, cnt) ->
        debug 0 "stat: %-30s ... %s" name (Int64.to_string !cnt))
      stats)

let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)
let add_stat (_, count) num = count := Int64.add !count (Int64.of_int num) (** add to stat *)

(** {Flags as integers} *)

module Flag = struct
  type gen = int ref

  let create () = ref 1

  let get_new gen =
    let n = !gen in
    if n < 0 then failwith "LogtkUtil.Flag.get_new: too many flags allocated";
    gen := 2*n;
    n
end

(** {2 LogtkOrdering utils} *)

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

let str_repeat s n =
  assert (n>=0);
  let len = String.length s in
  assert(len > 0);
  let buf = Bytes.init (len*n) (fun i -> s.[i mod len]) in
  Bytes.unsafe_to_string buf

let str_prefix ~pre s =
  String.length pre <= String.length s &&
  (let i = ref 0 in
    while !i < String.length pre && s.[!i] = pre.[!i] do incr i done;
    !i = String.length pre)

(** {2 Exceptions} *)

let finally ~h ~f =
  try
    let x = f () in
    h ();
    x
  with e ->
    h ();
    raise e

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

(** slurp the entire content of the file_descr into a string *)
let slurp i_chan =
  let buf_size = 128 in
  let content = Buffer.create 120
  and buf = String.make 128 'a' in
  let rec next () =
    let num = input i_chan buf 0 buf_size in
    if num = 0
      then Buffer.contents content (* EOF *)
      else (Buffer.add_substring content buf 0 num; next ())
  in next ()

type 'a or_error = [`Error of string | `Ok of 'a]

(** Call given command with given output, and return its output as a string *)
let popen ~cmd ~input =
  try
    let from, into = Unix.open_process cmd in
    (* send input to the subprocess *)
    output_string into input;
    close_out into;
    (* read ouput from the subprocess *)
    let output = slurp from in
    (* wait for subprocess to terminate *)
    ignore (Unix.close_process (from, into));
    CCError.return output
  with Unix.Unix_error (e, _, _) ->
    let msg = Unix.error_message e in
    CCError.fail msg

(** {2 Printing utils} *)

let sprintf format =
  let buffer = Buffer.create 64 in
  Printf.kbprintf
    (fun fmt -> Buffer.contents buffer)
    buffer
    format

let fprintf oc format =
  let buffer = Buffer.create 64 in
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

let pp_opt pp buf x = match x with
  | None -> Buffer.add_string buf "None"
  | Some x -> Printf.bprintf buf "Some %a" pp x

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item buf = function
  | x::((y::xs) as l) ->
    pp_item buf x;
    Buffer.add_string buf sep;
    pp_list ~sep pp_item buf l
  | x::[] -> pp_item buf x
  | [] -> ()

(** print an array of items using the printing function *)
let pp_array ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf a.(i)
  done

(** print an array of items using the printing function *)
let pp_arrayi ?(sep=", ") pp_item buf a =
  for i = 0 to Array.length a - 1 do
    (if i > 0 then Buffer.add_string buf sep);
    pp_item buf i a.(i)
  done

(** Print the sequence *)
let pp_seq ?(sep=", ") pp_item buf seq =
  Sequence.iteri
    (fun i x ->
      (if i > 0 then Buffer.add_string buf sep);
      pp_item buf x)
    seq
