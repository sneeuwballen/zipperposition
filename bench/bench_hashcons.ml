open Logtk

let n_domains = 1
let n_consts = 300
let max_depth = 7

let build_terms () =
  let t = Type.tType in
  let f = Term.const ~ty:(Type.arrow [ t ] t) (Name.make "f") in
  let g = Term.const ~ty:(Type.arrow [ t ] t) (Name.make "g") in
  let a =
    Array.init n_consts (fun i ->
        Term.const ~ty:t (Name.make (Printf.sprintf "a%d" i)))
  in
  let apply fn arg = Term.app fn [ arg ] in
  let bases = Array.to_list a in
  let all = ref bases in
  let cur = ref bases in
  for _depth = 1 to max_depth do
    let next = List.concat_map (fun t -> [ apply f t; apply g t ]) !cur in
    all := !all @ next;
    cur := next
  done;
  !all

let[@inline] span_to_s span = Mtime.Span.to_float_ns span /. 1_000_000_000.

let () =
  let semaphore = Semaphore.Counting.make 0 in
  let results = Array.make n_domains [] in
  let start_span = Atomic.make Mtime.Span.zero in
  let domains =
    Array.init n_domains (fun dom_id ->
        Domain.spawn (fun () ->
            Semaphore.Counting.acquire semaphore;
            if dom_id = 0 then Atomic.set start_span (Mtime_clock.elapsed ());
            let counter = Mtime_clock.counter () in
            let terms = build_terms () in
            results.(dom_id) <- terms;
            Mtime_clock.count counter))
  in
  for _ = 1 to n_domains do
    Semaphore.Counting.release semaphore
  done;
  let _ =
    Array.mapi
      (fun dom_id d ->
        let span = Domain.join d in
        Printf.printf "Domain %d: %.3fs (%d terms)\n" dom_id (span_to_s span)
          (List.length results.(dom_id));
        span)
      domains
  in
  let end_span = Mtime_clock.elapsed () in
  let total =
    span_to_s (Mtime.Span.abs_diff end_span (Atomic.get start_span))
  in
  Printf.printf "Wall clock: %.3fs\n" total;
  let ref_terms = results.(0) in
  Array.iter
    (fun terms ->
      List.iter2 (fun t_ref t -> assert (t_ref == t)) ref_terms terms)
    (Array.sub results 1 (n_domains - 1));
  Printf.printf "All %d terms shared across %d domains (== check passed).\n"
    (List.length ref_terms) n_domains
