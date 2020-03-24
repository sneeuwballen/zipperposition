
(** {1 profiling facilities} *)

type timestamp = float
let[@inline] timestamp_sub a b = a -. b
let[@inline] timestamp_add a b = a +. b
let[@inline] timestamp_cmp a b = CCFloat.compare a b

(** A profiler (do not call recursively) *)
type t = {
  prof_name : string;
  mutable prof_total : timestamp; (* total time *)
  mutable prof_calls : int; (* number of calls *)
  mutable prof_max : timestamp; (* max time in the profiled function (ns) *)
  mutable prof_enter : timestamp; (* time at which we entered the profiler (ns) *)
}

(* NOTE: profiling:
   enables compile-time branch cutting.
   if you want profiling, change this to [true]. *)
let __prof = false

let active_ = ref false

let enable_profiling b =
  if b && not __prof then (
    Util.errorf ~where:"zprof"
      "profiling has been deactivated at compile time. \
       Change `ZProf.__prof` and recompile."
  )

let profilers = ref []

let make name =
  let prof = {
    prof_name = name;
    prof_enter = 0.;
    prof_total = 0.;
    prof_calls = 0;
    prof_max = 0.;
  } in
  (* register profiler *)
  if __prof then (
    profilers := prof :: !profilers;
  );
  prof

let[@inline] enter_prof profiler =
  if __prof && !active_ then (
    profiler.prof_enter <- Util.get_time_mon_ ()
  )

let[@inline never] exit_prof_ profiler =
  let stop = Util.get_time_mon_ () in
  let delta = timestamp_sub stop profiler.prof_enter in
  profiler.prof_total <- timestamp_add profiler.prof_total delta;
  profiler.prof_calls <- profiler.prof_calls + 1;
  if delta > profiler.prof_max then profiler.prof_max <- delta;
  ()

let[@inline] exit_prof profiler =
  if __prof && !active_ then (
    exit_prof_ profiler
  )

let with_prof_ p f x =
  enter_prof p;
  try
    let y = f x in
    exit_prof p;
    y
  with e ->
    exit_prof p;
    raise e

let[@inline] with_prof p f x =
  if __prof && !active_ then (
    with_prof_ p f x
  ) else (
    f x
  )

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
  let tot = Util.total_time_s ()in
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
  if __prof then (
    Options.add_opts [
      "--profile", Arg.Unit (fun ()->enable_profiling true), " enable profiling probes";
      "--no-profile", Arg.Unit (fun ()->enable_profiling false), " disable profiling probes";
    ];
    at_exit
      (fun () ->
         if !active_ then Format.eprintf "%a@." show_profilers ())
  )
