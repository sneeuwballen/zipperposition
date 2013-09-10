(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Compact clause representation} *)

open Logtk

type t = {
  lits : Literal.t array;
  id : int;
}

let eq c1 c2 =
  c1.id = c2.id && Literal.Arr.eq c1.lits c2.lits

let hash c = c.id

let create id lits = { id; lits; }

let id c = c.id

let lits c = c.lits

let iter c f = Array.iter f c.lits

let to_seq c = Sequence.from_iter (fun k -> iter c k)

let pp buf c =
  Printf.bprintf buf "[%a]" Literal.Arr.pp c.lits

let to_string c =
  Util.on_buffer pp c

let fmt fmt c =
  Format.pp_print_string fmt (to_string c)

let bij ~ord =
  Bij.(map
    ~inject:(fun c -> c.id, c.lits)
    ~extract:(fun (id,lits) -> create id lits)
    (pair int_ (Literal.Arr.bij ~ord)))
