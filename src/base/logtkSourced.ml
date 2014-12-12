
(*
Copyright (c) 2013-2014, Simon Cruanes
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

(** {1 Sourced Statements} *)

type 'a t = {
  content : 'a;
  file : string;
  name : string;
  is_conjecture : bool;
}

let get x = x.content
let file x = x.file
let name x = x.name
let is_conjecture x = x.is_conjecture

let make ?(is_conjecture=false) ~name ~file content = {
  name; file; content; is_conjecture;
}

let map f x = {x with content=f x.content; }

let pp pp_x buf x =
  Printf.bprintf buf "%a [at %s in %s (conj: %B)]"
    pp_x x.content x.name x.file x.is_conjecture

let to_string pp_x x =
  CCPrint.to_string (pp pp_x) x

let fmt pp_x fmt x =
  Format.fprintf fmt "%a [at %s in %s (conj: %B)]"
    pp_x x.content x.name x.file x.is_conjecture
