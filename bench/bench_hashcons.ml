open Logtk

let n_domains = ref 1
let n_consts = ref 200
let max_depth = ref 7
let stats = ref false

let spec =
  [
    "--domains", Arg.Set_int n_domains, " Number of domains (default: 1)";
    "--consts", Arg.Set_int n_consts, " Number of base constants (default: 200)";
    "--depth", Arg.Set_int max_depth, " Max term depth (default: 7)";
    "--stats", Arg.Set stats, " show stats at the end";
  ]

let build_terms ?(seed = 0) () =
  let t = Type.tType in
  let f = Term.const ~ty:(Type.arrow [ t ] t) (Name.make "f") in
  let g = Term.const ~ty:(Type.arrow [ t ] t) (Name.make "g") in
  let a =
    let seed = seed mod !n_consts in
    Array.init !n_consts (fun i ->
        (* shift so we're not all producing terms in the same order *)
        let i = (i + seed) mod !n_consts in
        Term.const ~ty:t (Name.make (Printf.sprintf "a%d" i)))
  in
  let apply fn arg = Term.app fn [ arg ] in
  let bases = Array.to_list a in
  let all = ref bases in
  let cur = ref bases in
  for _depth = 1 to !max_depth do
    let next = List.concat_map (fun t -> [ apply f t; apply g t ]) !cur in
    all := !all @ next;
    cur := next
  done;
  let arr = Array.of_list !all in
  Array.to_list arr

let[@inline] span_to_s span = Mtime.Span.to_float_ns span /. 1_000_000_000.

let print_stats () =
  let sizes = InnerTerm.hashcons_shard_sizes () in
  let n_shards = Array.length sizes in
  let sum = Array.fold_left ( + ) 0 sizes in
  let min_s = Array.fold_left min max_int sizes in
  let max_s = Array.fold_left max 0 sizes in
  let avg = float_of_int sum /. float_of_int n_shards in
  Printf.printf
    "\nShard distribution (%d shards, %d terms, min=%d avg=%.1f max=%d):\n%!"
    n_shards sum min_s avg max_s;
  let n_buckets = 16 in
  let bucket_size = (max_s + n_buckets - 1) / n_buckets in
  let buckets = Array.make n_buckets 0 in
  Array.iter
    (fun s ->
      let b =
        if bucket_size = 0 then
          0
        else
          min (s / bucket_size) (n_buckets - 1)
      in
      buckets.(b) <- buckets.(b) + 1)
    sizes;
  let width = 50 in
  let max_count = max 1 (Array.fold_left max 0 buckets) in
  for b = 0 to n_buckets - 1 do
    let lo = b * bucket_size in
    let hi =
      if b = n_buckets - 1 then
        max_s
      else
        ((b + 1) * bucket_size) - 1
    in
    let bar = String.make (buckets.(b) * width / max_count) '#' in
    Printf.printf "  %4d..%-4d [%2d shards] %s\n%!" lo hi buckets.(b) bar
  done;
  let total_width = 50 in
  let scale = max 1 max_s in
  for i = 0 to n_shards - 1 do
    let bar = String.make (sizes.(i) * total_width / scale) '#' in
    Printf.printf "  %2d: %-5d %s\n%!" i sizes.(i) bar
  done;
  let shard_stats = InnerTerm.hashcons_shard_stats () in
  Printf.printf "\nPer-shard weak table stats (legend: entries=#terms, load=entries/table_len):\n%!";
  let bar_w = 40 in
  let max_entries = ref 0 in
  Array.iter (fun (_, n, _, _, _, _) -> max_entries := max !max_entries n) shard_stats;
  let max_entries = max 1 !max_entries in
  for i = 0 to n_shards - 1 do
    let table_len, num_entries, sum_bucket_len, minb, _medb, maxb =
      shard_stats.(i)
    in
    let avg_bkt =
      if num_entries = 0 then 0 else sum_bucket_len / num_entries
    in
    let load_pct =
      if table_len = 0 then 0
      else (num_entries * 100) / table_len
    in
    let bar = String.make (num_entries * bar_w / max_entries) '#' in
    Printf.printf "  %2d: entries=%-5d load=%-3d%% avg_bkt=%-4d min=%-4d max=%-4d %s\n%!"
      i num_entries load_pct avg_bkt minb maxb bar
  done

let main () =
  let barrier = Atomic.make 0 in
  let results = Array.make !n_domains [] in
  let start_span = Atomic.make Mtime.Span.zero in
  Printf.printf "Spawning %d domains...\n%!" !n_domains;
  let domains =
    Array.init !n_domains (fun dom_id ->
        Domain.spawn (fun () ->
            Atomic.incr barrier;
            while Atomic.get barrier < !n_domains do
              Domain.cpu_relax ()
            done;
            if dom_id = 0 then Atomic.set start_span (Mtime_clock.elapsed ());
            let counter = Mtime_clock.counter () in
            let terms = build_terms ~seed:dom_id () |> List.sort Term.compare in
            results.(dom_id) <- terms;
            Mtime_clock.count counter))
  in
  Printf.printf "Spawned. Joining %d...\n%!" !n_domains;
  Array.iteri
    (fun dom_id d ->
      match Domain.join d with
      | span ->
        Printf.printf "Domain %d (seed %d): %.3fs (%d terms)\n" dom_id dom_id
          (span_to_s span)
          (List.length results.(dom_id))
      | exception e ->
        Printf.printf "Domain %d: EXCEPTION %s\n%!" dom_id
          (Printexc.to_string e))
    domains;
  let end_span = Mtime_clock.elapsed () in
  let total =
    span_to_s (Mtime.Span.abs_diff end_span (Atomic.get start_span))
  in
  Printf.printf "Wall clock: %.3fs\n" total;
  let ref_terms = results.(0) in
  (try
     let seen = Term.Tbl.create (List.length ref_terms) in
     List.iter (fun t -> Term.Tbl.add seen t ()) ref_terms;
     Array.iteri
       (fun dom_id terms ->
         if dom_id = 0 then
           ()
         else
           List.iter
             (fun t ->
               if not (Term.Tbl.mem seen t) then
                 Printf.printf "MISMATCH: domain %d term not in domain 0 set!\n"
                   dom_id)
             terms)
       results;
     Printf.printf "All %d terms shared across %d domains (== check passed).\n"
       (List.length ref_terms) !n_domains
   with e -> Printf.printf "Sharing check failed: %s\n" (Printexc.to_string e));
  if !stats then print_stats ()

let () =
  Arg.parse spec (fun _ -> ()) "Usage: bench_hashcons [options]";
  Printf.printf "START\n%!";
  Printexc.record_backtrace true;
  try main ()
  with e ->
    Printf.printf "FATAL: %s\n%s\n%!" (Printexc.to_string e)
      (Printexc.get_backtrace ())
