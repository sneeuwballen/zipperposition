module StringMap = Map.Make (String)

type impl = (module Term.S)

let impls : impl StringMap.t =
  let open StringMap in
  empty
  |> add "old"
       (module struct
         include Term.Make (Hashcons_old.Make)
       end : Term.S)
  |> add "spinlock"
       (module struct
         include Term.Make (Hashcons_spinlock.Make)
       end : Term.S)
  |> add "mutex"
       (module struct
         include Term.Make (Hashcons_mutex.Make)
       end : Term.S)

let n_iters = ref 1
let n_consts = ref 200
let max_depth = ref 7
let impl_key = ref "spinlock"

let spec =
  [
    "--iters", Arg.Set_int n_iters, " Number of iterations (default: 1)";
    "--consts", Arg.Set_int n_consts, " Number of base constants (default: 200)";
    "--depth", Arg.Set_int max_depth, " Max term depth (default: 7)";
  ]

let anon_fun s = impl_key := s

module Bench (T : Term.S) = struct
  let build_terms ~seed =
    let f = T.const "f" in
    let g = T.const "g" in
    let a =
      let seed = seed mod !n_consts in
      Array.init !n_consts (fun i ->
          let i = (i + seed) mod !n_consts in
          T.const (Printf.sprintf "a%d" i))
    in
    let bases = Array.to_list a in
    let all = ref bases in
    let cur = ref bases in
    for _depth = 1 to !max_depth do
      let next =
        List.concat_map (fun t -> [ T.app f [ t ]; T.app g [ t ] ]) !cur
      in
      all := !all @ next;
      cur := next
    done;
    List.length !all

  let run () =
    let total_terms = ref 0 in
    let counter = Mtime_clock.counter () in
    for iter = 0 to !n_iters - 1 do
      let n = build_terms ~seed:iter in
      total_terms := !total_terms + n
    done;
    let span = Mtime_clock.count counter in
    let secs = Mtime.Span.to_float_ns span /. 1_000_000_000. in
    total_terms := !total_terms;
    Printf.printf
      "impl=%s iters=%d consts=%d depth=%d total_terms=%d time=%.3fs\n%!"
      !impl_key !n_iters !n_consts !max_depth !total_terms secs
end

let run () =
  let impl =
    match StringMap.find_opt !impl_key impls with
    | Some m -> m
    | None ->
      Printf.eprintf
        "Unknown implementation '%s'. Choose: old, spinlock, mutex\n" !impl_key;
      exit 1
  in
  let (module M : Term.S) = impl in
  let module B = Bench (M) in
  B.run ()

let () =
  Arg.parse spec anon_fun
    "Usage: bench.exe [old|spinlock|mutex] [options]\nOptions:";
  Printexc.record_backtrace true;
  run ()
