
(** {1 profiling facilities} *)

type timestamp = float

(** A profiler (do not call recursively) *)
type t = {
  prof_name : string;
  mutable prof_total : timestamp; (* total time *)
  mutable prof_calls : int; (* number of calls *)
  mutable prof_max : timestamp; (* max time in the profiled function (us) *)
}

type span =
  | No_span
  | Span of {
      start: timestamp;
      prof: t;
    }

(* NOTE: profiling:
   enables compile-time branch cutting.
   if you want profiling, set this to [true]. *)
let __prof = true

(* do we produce traces in the
   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/
   format? *)
let tef_ = lazy (
  __prof &&
  match Sys.getenv_opt "TEF" with
  | Some ("1"|"true") -> true
  | _ -> false
)

module TEF = struct
  let first_ = ref true
  let closed_ = ref false

  let out_ : out_channel option ref = ref None

  let teardown_ oc =
    if not !closed_ then (
      closed_ := true;
      output_char oc ']'; (* close array *)
      flush oc;
      close_out_noerr oc
    )

  let setup_ = lazy (
    if Lazy.force tef_ then (
      let oc = Unix.open_process_out "zstd --fast -q -z -f - -o trace.json.zst" in
      output_char oc '[';
      at_exit (fun () -> teardown_ oc);
      out_ := Some oc
    )
  )

  let setup () = Lazy.force setup_ (* only once *)

  let emit_sep_ oc =
    if !first_ then (
      first_ := false;
    ) else (
      output_string oc ",\n";
    )

  let emit_duration_event oc (prof:t) ~start ~end_ () : unit =
    let dur = end_ -. start in
    let ts = start in
    let pid = Unix.getpid() in
    emit_sep_ oc;
    Printf.fprintf oc
      {json|{"pid": %d,"tid":1,"dur": %.2f,"ts": %.2f,"name":"%s","ph":"X"}|json}
      pid dur ts prof.prof_name;
    ()

  let emit_instant_event oc ~ts ~msg () : unit =
    let pid = Unix.getpid() in
    emit_sep_ oc;
    Printf.fprintf oc
      {json|{"pid": %d,"tid":1,"ts": %.2f,"name":%S,"ph":"I"}|json}
      pid ts msg;
    ()

  let teardown () =
    match !out_ with
    | None -> ()
    | Some oc -> out_ := None; teardown_ oc;
end

let prof_active_ = lazy (
  match Sys.getenv_opt "PROF" with
  | Some ("1"|"true") -> true
  | _ -> false
)

let active_ = lazy (
  Lazy.force tef_ || Lazy.force prof_active_
)

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
    prof_total = 0.;
    prof_calls = 0;
    prof_max = 0.;
  } in
  (* register profiler *)
  if __prof then (
    profilers := prof :: !profilers;
  );
  prof

let enter_prof_active_ profiler : span =
  Span {start=Util.get_time_mon_us (); prof=profiler}

let[@inline] enter_prof profiler : span =
  if __prof && Lazy.force active_ then (
    enter_prof_active_ profiler
  ) else (
    No_span
  )

let[@inline never] exit_with_ profiler start : unit =
  let stop = Util.get_time_mon_us () in
  if Lazy.force prof_active_ then (
    let delta = stop -. start in
    profiler.prof_total <- profiler.prof_total +. delta;
    profiler.prof_calls <- profiler.prof_calls + 1;
    if delta > profiler.prof_max then profiler.prof_max <- delta;
  );
  begin match !TEF.out_ with
    | None -> ()
    | Some oc -> TEF.emit_duration_event oc profiler ~start ~end_:stop ();
  end;
  ()

let[@inline] exit_prof span =
  match span with
  | No_span -> ()
  | Span {start; prof} -> exit_with_ prof start

let message_real_ f =
  match !TEF.out_ with
  | None -> ()
  | Some oc ->
    let ts = Util.get_time_mon_us() in
    let msg = f() in
    TEF.emit_instant_event oc ~ts ~msg ()

let[@inline] message f =
  if __prof && Lazy.force active_ then (
    message_real_ f
  )

let with_prof_active_ p f x =
  let span = enter_prof p in
  try
    let y = f x in
    exit_prof span;
    y
  with e ->
    exit_prof span;
    raise e

let[@inline] with_prof_ p f x =
  if Lazy.force active_ then (
    with_prof_active_ p f x
  ) else (
    f x
  )

let[@inline] with_prof p f x =
  if __prof && Lazy.force active_ then (
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
    "function" "#calls" "total (s)" "% total" "max (us)" "average (us)";
  (* sort profilers by decreasing total time *)
  let profilers =
    List.sort
      (fun p1 p2 -> - (CCFloat.compare p1.prof_total p2.prof_total))
      !profilers
  in
  let tot = Util.total_time_s ()in
  List.iter
    (fun profiler ->
       if profiler.prof_calls > 0 then (
        (* print content of the profiler *)
         let total_s = profiler.prof_total *. 1e-6 in
        Format.fprintf out "@[%-39s %10d %9.3f %9.2f %9.1f %9.1f@]@,"
          profiler.prof_name
          profiler.prof_calls
          total_s
          (total_s *. 100. /. tot)
          profiler.prof_max
          (profiler.prof_total /. (float_of_int profiler.prof_calls))
      )
    )
    profilers;
  Format.fprintf out "@]";
  ()

(** Print profiling data upon exit *)
let pp_prof () =
  if Lazy.force prof_active_ then (
    Format.eprintf "%a@." show_profilers ()
  );
  TEF.teardown ()

let setup () =
  TEF.setup ();
  at_exit pp_prof
