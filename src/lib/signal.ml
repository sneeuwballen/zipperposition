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

type 'a t = {
  mutable n : int;  (* how many handlers? *)
  mutable handlers : ('a -> bool) array;
} (** Signal of type 'a *)

let nop_handler x = true

let create () =
  let s = { 
    n = 0;
    handlers = Array.create 3 nop_handler;
  } in
  s

(* remove handler at index i *)
let remove s i =
  (if i < s.n - 1  (* erase handler with the last one *)
    then s.handlers.(i) <- s.handlers.(s.n - 1));
  s.handlers.(s.n - 1) <- nop_handler; (* free handler *)
  s.n <- s.n - 1;
  ()

let send s x =
  for i = 0 to s.n - 1 do
    while not (try s.handlers.(i) x with _ -> false) do
      remove s i  (* i-th handler is done, remove it *)
    done
  done

let on s f =
  (* resize handlers if needed *)
  (if s.n = Array.length s.handlers
    then begin
      let handlers = Array.create (s.n + 4) nop_handler in
      Array.blit s.handlers 0 handlers 0 s.n;
      s.handlers <- handlers
    end);
  s.handlers.(s.n) <- f;
  s.n <- s.n + 1

let once s f =
  on s (fun x -> ignore (f x); false)

let propagate a b =
  on a (fun x -> send b x; true)

(** {2 Combinators} *)

let map signal f =
  let signal' = create () in
  (* weak ref *)
  let r = Weak.create 1 in
  Weak.set r 0 (Some signal');
  on signal (fun x ->
    match Weak.get r 0 with
    | None -> false
    | Some signal' -> send signal' (f x); true);
  signal'

let filter signal p =
  let signal' = create () in
  (* weak ref *)
  let r = Weak.create 1 in
  Weak.set r 0 (Some signal');
  on signal (fun x ->
    match Weak.get r 0 with
    | None -> false
    | Some signal' -> (if p x then send signal' x); true);
  signal'
