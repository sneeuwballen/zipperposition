
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Some helpers} *)

module Fmt = CCFormat

(** {2 Time facilities} *)

type timestamp = float

let timestamp_sub a b = a -. b
let timestamp_add a b = a +. b
let timestamp_cmp = CCFloat.compare

let get_time_mon_() : timestamp = Unix.gettimeofday()

let start_ = get_time_mon_()

let total_time_s () = timestamp_sub (get_time_mon_()) start_
let start_time () = start_

(** {2 Misc} *)

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

  let base = make "base"

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

let break_on_debug = ref false

(* wait for user input *)
let wait_user_input () =
  ignore (input_line stdin)

let set_debug = Section.set_debug Section.root
let get_debug () = Section.root.Section.level

let debug_fmt_ = Format.std_formatter

let debugf ?(section=Section.root) l msg k =
  if l <= Section.cur_level section then (
    let now = total_time_s() in
    if section == Section.root
    then Format.fprintf debug_fmt_ "@{<Black>@[<4>%.3f[]@}@ " now
    else Format.fprintf debug_fmt_ "@{<Black>@[<4>%.3f[%s]@}@ "
        now section.Section.full_name;
    k (Format.kfprintf
        (fun fmt ->
           Format.fprintf fmt "@]@.";
           if !break_on_debug then wait_user_input();
        )
        debug_fmt_ msg)
  )

let debug ?section l msg = debugf ?section l "%s" (fun k->k msg)

let ksprintf_noc ~f fmt =
  let buf = Buffer.create 32 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ -> Format.pp_print_flush out (); f (Buffer.contents buf)) out fmt

(* print error prefix *)
let pp_error_prefix out () = Format.fprintf out "@{<Red>Error@}: "

let err_spf fmt =
  Fmt.ksprintf fmt
    ~f:(fun s -> Fmt.sprintf "@[%a@,%s@]" pp_error_prefix () s)


let warn_fmt_ = Format.err_formatter

let warnf msg =
  Format.fprintf warn_fmt_ "@[<2>@{<Magenta>[Warning]@}: ";
  Format.kfprintf
    (fun out -> Format.fprintf out "@]@.")
    Format.err_formatter msg

let warn msg = warnf "%s" msg

exception Error of string * string

let () =
  Printexc.register_printer
    (function
      | Error (where,msg) ->
        Some (err_spf "error in %s:@ %s" where msg)
      | Invalid_argument msg ->
        Some (err_spf "@[<2>invalid_argument: %s@]" msg)
      | _ -> None)

let error ~where msg = raise (Error (where,msg))
let errorf ~where msg = Fmt.ksprintf ~f:(error ~where) msg

let pp_pos pos =
  let open Lexing in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

external set_memory_limit_stub : int -> unit = "logtk_set_memory_limit"

let set_memory_limit n =
  if n <= 0 then invalid_arg "set_memory_limit: expect positive arg";
  debugf 1 "limit memory to %d MB" (fun k-> k n);
  set_memory_limit_stub n

external set_time_limit_stub : int -> unit = "logtk_set_time_limit"

let set_time_limit n =
  if n <= 0 then invalid_arg "set_time_limit: expect positive arg";
  debugf 1 "limit time to %ds" (fun k->k n);
  set_time_limit_stub n

module Exn = struct
  let pp_stack buf d =
    Printf.bprintf buf "\nstack:\n%s"
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack d))

  let fmt_stack out d =
    Format.fprintf out "\nstack:\n%s"
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack d))

  let pp_backtrace buf () =
    if Printexc.backtrace_status () then (
      Buffer.add_string buf "\nbacktrace:\n";
      Buffer.add_string buf (Printexc.get_backtrace ())
    )

  let fmt_backtrace out () =
    if Printexc.backtrace_status () then
      Format.fprintf out "\nbacktrace:\n%s" (Printexc.get_backtrace ())

  let string_of_backtrace () =
    if Printexc.backtrace_status ()
    then "\nbacktrace:\n" ^ Printexc.get_backtrace ()
    else "<no backtrace>"
end

(** {2 profiling facilities} *)

(** A profiler (do not call recursively) *)
type profiler = {
  prof_name : string;
  mutable prof_total : timestamp; (* total time *)
  mutable prof_calls : int; (* number of calls *)
  mutable prof_max : timestamp; (* max time in the profiled function (ns) *)
  mutable prof_enter : timestamp; (* time at which we entered the profiler (ns) *)
}

let enable_profiling = ref false

let profilers = ref []

let mk_profiler name =
  let prof = {
    prof_name = name;
    prof_enter = 0.;
    prof_total = 0.;
    prof_calls = 0;
    prof_max = 0.;
  } in
  (* register profiler *)
  profilers := prof :: !profilers;
  prof

let enter_prof profiler =
  if !enable_profiling
  then profiler.prof_enter <- get_time_mon_ ()

let exit_prof profiler =
  if !enable_profiling then (
    let stop = get_time_mon_ () in
    let delta = timestamp_sub stop profiler.prof_enter in
    profiler.prof_total <- timestamp_add profiler.prof_total delta;
    profiler.prof_calls <- profiler.prof_calls + 1;
    if delta > profiler.prof_max then profiler.prof_max <- delta;
  )

let with_prof p f x =
  if !enable_profiling then (
    enter_prof p;
    try
      let y = f x in
      exit_prof p;
      y
    with e ->
      exit_prof p;
      raise e
  ) else f x

let show_profilers out () =
  Format.fprintf out "@[<v>";
  Format.fprintf out
    "@[%39s ---------- --------- --------- --------- ---------@]@,"
    (String.make 39 '-');
  Format.fprintf out
    "@[%-39s %10s %9s %9s %9s %9s@]@,"
    "function" "#calls" "total" "% total" "max" "average";
  (* sort profilers by decreasing total time *)
  let profilers =
    List.sort
      (fun p1 p2 -> - (timestamp_cmp p1.prof_total p2.prof_total))
      !profilers
  in
  let tot = total_time_s ()in
  List.iter
    (fun profiler -> if profiler.prof_calls > 0 then
        (* print content of the profiler *)
        Format.fprintf out "@[%-39s %10d %9.4f %9.2f %9.4f %9.4f@]@,"
          profiler.prof_name
          profiler.prof_calls
          profiler.prof_total
          (profiler.prof_total *. 100. /. tot)
          profiler.prof_max
          (profiler.prof_total /. (float_of_int profiler.prof_calls))
    )
    profilers;
  Format.fprintf out "@]";
  ()

(** Print profiling data upon exit *)
let () =
  at_exit
    (fun () ->
       if !enable_profiling then Format.eprintf "%a@." show_profilers ())

(** {2 Runtime statistics} *)

(* TODO cleanup, make hierarchic *)

type stat = string * int64 ref

let mk_stat, print_global_stats =
  let stats = ref [] in
  (* create a stat *)
  (fun name ->
     let stat = (name, ref 0L) in
     stats := stat :: !stats;
     stat),
  (* print stats *)
  (fun ~comment () ->
     let stats = List.sort (fun (n1,_)(n2,_) -> String.compare n1 n2) !stats in
     List.iter
       (fun (name, cnt) -> Format.printf "%sstat: %-35s ... %Ld@." comment name !cnt)
       stats)

let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)
let add_stat (_, count) num = count := Int64.add !count (Int64.of_int num) (** add to stat *)

(** {Flags as integers} *)

module Flag = struct
  type gen = int ref

  let create () = ref 1

  let get_new gen =
    let n = !gen in
    if n < 0 then failwith "Flag.get_new: too many flags allocated";
    gen := 2*n;
    n
end

(** {2 Others} *)

let finally ~do_ f =
  try
    let x = f () in
    do_ ();
    x
  with e ->
    do_ ();
    raise e

let pp_pair ?(sep=", ") pa pb out (a,b) =
  Format.fprintf out "@[%a%s%a@]" pa a sep pb b

let pp_sep sep out () = Format.fprintf out "%s@," sep
let pp_list ?(sep=", ") pp = Fmt.list ~sep:(pp_sep sep) pp
let pp_seq ?(sep=", ") pp = Fmt.seq ~sep:(pp_sep sep) pp

let pp_list0 ?(sep=" ") pp_x out = function
  | [] -> ()
  | l -> Format.fprintf out " %a" (pp_list ~sep pp_x) l

let tstp_needs_escaping s =
  assert (s<>"");
  s.[0] = '_' ||
  CCString.exists (function ' ' | '#' | '$' | '+' | '-' | '/' -> true | _ -> false) s

let pp_str_tstp out s =
  CCFormat.string out (if tstp_needs_escaping s then "'" ^ String.escaped s ^ "'" else s)

let pp_var_tstp out s = CCFormat.string out (CCString.capitalize_ascii s)

let ord_option c o1 o2 = match o1, o2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> c x1 x2

let take_drop_while f l = CCList.take_while f l, CCList.drop_while f l

(* cartesian product of lists of lists *)
let map_product ~f l =
  let product a b =
    List.fold_left
      (fun acc1 l1 -> List.fold_left
          (fun acc2 l2 -> (List.rev_append l1 l2) :: acc2)
          acc1 b)
      [] a
  in
  match l with
    | [] -> []
    | l1 :: tail ->
      List.fold_left
        (fun acc x -> product (f x) acc)
        (f l1)
        tail

let seq_map_l ~f l =
  let rec aux l yield = match l with
    | [] -> yield []
    | x :: tail ->
      let ys = f x in
      List.iter
        (fun y -> aux tail (fun l -> yield (y::l)))
        ys
  in
  aux l

let seq_zipi seq k =
  let i = ref 0 in
  seq (fun x -> k (!i, x); incr i)

let invalid_argf msg = Fmt.ksprintf msg ~f:invalid_arg
let failwithf msg = Fmt.ksprintf msg ~f:failwith

module Int_map = CCMap.Make(CCInt)
module Int_set = CCSet.Make(CCInt)

let escape_dot s =
  let b = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
       begin match c with
         | '|' | '\\' | '{' | '}' | '<' | '>' | '"' ->
           Buffer.add_char b '\\'; Buffer.add_char b c
         | '\n' -> Buffer.add_string b "\\l"; (* left justify *)
         | _ -> Buffer.add_char b c
       end)
    s;
  Buffer.contents b

(** {2 File utils} *)

type 'a or_error = ('a, string) CCResult.t

(** Call given command with given output, and return its output as a string *)
let popen ~cmd ~input : _ or_error =
  try
    let from, into = Unix.open_process cmd in
    (* send input to the subprocess *)
    output_string into input;
    close_out into;
    (* read ouput from the subprocess *)
    let output = CCIO.read_all from in
    (* wait for subprocess to terminate *)
    ignore (Unix.close_process (from, into));
    CCResult.return output
  with Unix.Unix_error (e, _, _) ->
    let msg = Unix.error_message e in
    CCResult.fail msg
