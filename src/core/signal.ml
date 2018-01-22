
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic signal} *)

type handler_response =
  | ContinueListening
  | StopListening

type 'a t = {
  mutable n : int;  (* how many handlers? *)
  mutable handlers : ('a -> handler_response) array;
  mutable alive : keepalive;   (* keep some signal alive *)
} (** Signal of type 'a *)

and keepalive =
  | Keep : 'a t -> keepalive
  | NotAlive : keepalive

let _exn_handler = ref (fun _ -> ())

let nop_handler _ = ContinueListening

let create () =
  let s = {
    n = 0;
    handlers = Array.make 3 nop_handler;
    alive = NotAlive;
  } in
  s

(* remove handler at index i *)
let remove s i =
  assert (s.n > 0 && i >= 0);
  if i < s.n - 1  (* erase handler with the last one *)
  then s.handlers.(i) <- s.handlers.(s.n - 1);
  s.handlers.(s.n - 1) <- nop_handler; (* free handler *)
  s.n <- s.n - 1;
  ()

let send s x =
  for i = 0 to s.n - 1 do
    while
      begin try match s.handlers.(i) x with
        | ContinueListening -> false  (* keep *)
        | StopListening -> true
        with e ->
          !_exn_handler e;
          false  (* be conservative, keep... *)
      end
    do
      remove s i  (* i-th handler is done, remove it *)
    done
  done

let on s f =
  (* resize handlers if needed *)
  if s.n = Array.length s.handlers
  then begin
    let handlers = Array.make (s.n + 4) nop_handler in
    Array.blit s.handlers 0 handlers 0 s.n;
    s.handlers <- handlers
  end;
  s.handlers.(s.n) <- f;
  s.n <- s.n + 1

let on_every s f =
  on s (fun x -> ignore (f x); ContinueListening)

let once s f =
  on s (fun x -> ignore (f x); StopListening)

let propagate a b = on_every a (send b)

(** {2 Combinators} *)

let map signal f =
  let signal' = create () in
  (* weak ref *)
  let r = Weak.create 1 in
  Weak.set r 0 (Some signal');
  on signal (fun x ->
    match Weak.get r 0 with
      | None -> StopListening
      | Some signal' -> send signal' (f x); ContinueListening);
  signal'.alive <- Keep signal;
  signal'

let filter signal p =
  let signal' = create () in
  (* weak ref *)
  let r = Weak.create 1 in
  Weak.set r 0 (Some signal');
  on signal (fun x ->
    match Weak.get r 0 with
      | None -> StopListening
      | Some signal' -> (if p x then send signal' x); ContinueListening);
  signal'.alive <- Keep signal;
  signal'

let set_exn_handler h = _exn_handler := h
