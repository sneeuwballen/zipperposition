(* This file is free software, part of Logtk. See file "license" for more details. *)
open CCFun.Infix

(** {1 Some helpers} *)

module Fmt = CCFormat

(** {2 Time facilities} *)

type timestamp = float

let start_ = Mtime_clock.now()

let get_time_mon_us () : timestamp =
  let t = Mtime_clock.now() in
  Mtime.Span.to_us (Mtime.span start_ t)

let total_time_s () = get_time_mon_us () *. 1e-6

(** {2 Misc} *)

let concat_view separator view = String.concat separator % List.map view

let superscript_table = Array.of_list(String.split_on_char ' '
"⁽ ⁾ * ⁺ , ⁻ ᐧ ᐟ ⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ : ; ᑉ ⁼ > ˀ @ ᴬ ᴮ ᕪ ᴰ ᴱ ᣘ ᴳ ᴴ ᴵ ᴶ ᴷ ᴸ ᴹ ᴺ ᴼ ᴾ ᶲ ᴿ ᔆ ᵀ ᵁ ⱽ ᵂ ᕁ ˠ ᙆ [ ᐠ ] ᣔ ᗮ ` ᵃ ᵇ ᶜ ᵈ ᵉ ᶠ ᵍ ʰ ⁱ ʲ ᵏ ˡ ᵐ ⁿ ᵒ ᵖ ᵠ ʳ ˢ ᵗ ᵘ ᵛ ʷ ˣ ʸ ᶻ")

let superscript = concat_view "" (fun c -> superscript_table.(Char.code c - 40)) % List.of_seq % String.to_seq

let caller_file_line d =
  let open String in
  (* get_callstack d can contain >d frames due to inlining *)
  let frame = map(function '\\'->'/' | c->c) (List.nth (split_on_char '\n' Printexc.(raw_backtrace_to_string(get_callstack(d+2)))) (d+1)) in
  (* s = [_ ^ c1 ^]? between c1 c2 s ^ c2 ^ _ searched backwards *)
  let between c1 c2 s =
    match rindex_opt s c2 with None -> "??"(* no stack trace *)  | Some j2 ->
    let j1 = match rindex_from_opt s (j2-1) c1 with None -> 0 | Some j -> j+1 in
    sub s j1 (j2-j1)
  in
  between '/' '.' (between '"' '"' frame) ^ between ',' ',' frame

(** Debug section *)
module Section = struct
  let null_level = -1 (* absence of level *)

  type t = {
    descr : descr;
    full_name : string;
    mutable level : int;
    mutable cur_level: int lazy_t; (* cached computed level *)
  }
  and descr =
    | Root
    | Sub of string * t * t list  (* name, parent, inheriting *)

  (* inlinable function *)
  let[@inline] cur_level s = Lazy.force s.cur_level

  (* recursive lookup of level, with inheritance from parent *)
  let compute_cur_level_ s =
    if s.level <> null_level then s.level
    else match s.descr with
      | Root -> 0
      | Sub (_, parent, []) -> cur_level parent
      | Sub (_, parent, [i]) -> max (cur_level parent) (cur_level i)
      | Sub (_, parent, inheriting) ->
        List.fold_left
          (fun m i -> max m (cur_level i))
          (cur_level parent) inheriting

  (* build a section *)
  let mk ?(level=null_level) descr full_name : t =
    let rec self = {
      descr; full_name; level; cur_level= lazy (compute_cur_level_ self);
    } in
    self

  let root : t = mk ~level:0 Root ""

  (* computes full name of section *)
  let compute_full_name (d:descr) =
    let buf = Buffer.create 15 in
    let rec add d = match d with
      | Root -> true
      | Sub (name, parent, _) ->
        let parent_is_root = add parent.descr in
        if not parent_is_root then Buffer.add_char buf '.';
        Buffer.add_string buf name;
        false
    in
    ignore (add d);
    Buffer.contents buf

  let full_name s = s.full_name

  (* full name -> section *)
  let section_table = Hashtbl.create 15

  (* reset all cached levels *)
  let invalidate_cache () =
    root.cur_level <- lazy (compute_cur_level_ root);
    Hashtbl.iter (fun _ s -> s.cur_level <- lazy (compute_cur_level_ s)) section_table

  let set_debug s i = assert (i>=0); s.level <- i; invalidate_cache ()
  let clear_debug s = s.level <- null_level; invalidate_cache()
  let get_debug s =
    if s.level=null_level then None else Some s.level

  let make ?(parent=root) ?(inheriting=[]) name =
    if name="" then invalid_arg "Section.make: empty name";
    let descr = Sub(name, parent, inheriting) in
    let name' = compute_full_name descr in
    try
      Hashtbl.find section_table name'
    with Not_found ->
      (* new section! register it, add an option to set its level *)
      let sec = mk descr name' in
      Hashtbl.add section_table name' sec;
      sec

  let iter yield =
    yield ("", root);
    Hashtbl.iter (fun name sec -> yield (name,sec)) section_table
end

let break_on_debug = ref false

(* wait for user input *)
let wait_user_input () =
  ignore (input_line stdin)

let set_debug = Section.set_debug Section.root
let get_debug () = Section.root.Section.level

let debug_fmt_ = Format.std_formatter

let debugf_real ~section msg k =
  let now = total_time_s() in
  if section == Section.root
  then Format.fprintf debug_fmt_ "@{<blue>@[<4>%.3f[]@}@ " now
  else Format.fprintf debug_fmt_ "@{<blue>@[<4>%.3f[%s]@}@ "
      now section.Section.full_name;
  k (Format.kfprintf
       (fun fmt ->
          Format.fprintf fmt "@]@.";
          if !break_on_debug then wait_user_input();
       )
       debug_fmt_ msg)

let[@inline] debugf ?(section=Section.root) l msg k =
  if l <= Section.cur_level section then (
    debugf_real ~section msg k
  )

let[@inline] debug ?section l msg = debugf ?section l "%s" (fun k->k msg)

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

module UntypedPrint = struct
  open Obj
  type any (* Typing technicality: Dynamic tests lead to many free type variables which are problematic with the value restriction of OCaml. Hence there's "any" to replace some. *)

  (* Internal list associating string converters to dynamic types. Modified by the below add_pp only. *)
  let string_printers: ((any -> bool) * (any -> string)) list ref = ref[]

  let add_pp type_test to_string = string_printers := (type_test, to_string % magic) :: !string_printers

  let str x =
    let rec loop = function [] -> raise(Failure "Printers from TypeTests.ml are uninitialized!")
    | (type_test, to_string)::printers ->
      if is_int(repr x) then string_of_int(magic x) (* prioritize int's *)
      else if type_test x then to_string x
      else loop printers
    in loop(magic !string_printers)

  (* Print message msg preceded by FILE line LINE ⛓️CALL-DEPTH of the caller's caller. *)
  let print_with_caller msg =
    print_string(caller_file_line 2 ^" ⛓️"^ str Printexc.(raw_backtrace_length(get_callstack 9999)) ^"\t");
    flush_all(); (* Show at least location if printing triggers segmentation fault. *)
    let msg = msg() in print_endline(msg ^ if String.length msg < 55(*arbitrary*) then "" else "\n")

  (* Sprinkle these in front of expressions to trace them—often without rebracketing! *)
  let (~<)x = print_with_caller(fun()-> str x); x
  let (|<) info x = print_with_caller(fun()-> info ^" "^ str x); x
end


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

let pp_stat out (name, count) =
  Format.fprintf out "%s-%d" name (CCInt64.to_int !count)

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
let pp_iter ?(sep=", ") pp = Fmt.iter ~sep:(pp_sep sep) pp

let pp_list0 ?(sep=" ") pp_x out = function
  | [] -> ()
  | l -> Format.fprintf out " %a" (pp_list ~sep pp_x) l

let tstp_needs_escaping s =
  assert (s<>"");
  s.[0] = '_' ||
  CCString.exists (function ' ' | '#' | '$' | '+' | '-' | '/' -> true | _ -> false) s

let pp_str_tstp out s =
  CCFormat.string out (if tstp_needs_escaping s then "'" ^ String.escaped s ^ "'" else s)

let pp_var_tstp out s = pp_str_tstp out (CCString.capitalize_ascii s)

let ord_option c o1 o2 = match o1, o2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x1, Some x2 -> c x1 x2

let for_all_2 p x y = CCList.(length x = length y && for_all2 p x y)

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
    (* read output from the subprocess *)
    let output = CCIO.read_all from in
    (* wait for subprocess to terminate *)
    ignore (Unix.close_process (from, into));
    CCResult.return output
  with Unix.Unix_error (e, _, _) ->
    let msg = Unix.error_message e in
    CCResult.fail msg
