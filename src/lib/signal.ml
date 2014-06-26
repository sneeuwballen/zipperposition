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

let nop_handler x = ContinueListening

let create () =
  let s = {
    n = 0;
    handlers = Array.create 3 nop_handler;
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
      let handlers = Array.create (s.n + 4) nop_handler in
      Array.blit s.handlers 0 handlers 0 s.n;
      s.handlers <- handlers
    end;
  s.handlers.(s.n) <- f;
  s.n <- s.n + 1

let once s f =
  on s (fun x -> ignore (f x); StopListening)

let propagate a b =
  on a (fun x -> send b x; ContinueListening)

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
