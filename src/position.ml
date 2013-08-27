
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

(** {1 Positions in terms, clauses...} *)

type t = int list
  (** A position is a path in a tree *)

type position = t

let left_pos = 0
let right_pos = 1

let compare = Pervasives.compare
let eq p1 p2 = compare p1 p2 = 0
let hash p = Hash.hash_list (fun x -> x) 29 p

(** Opposite position in a literal *)
let opp p = match p with
  | _ when p = left_pos -> right_pos
  | _ when p = right_pos -> left_pos
  | _ -> assert false

let pp buf pos = match pos with
  | [] -> Buffer.add_string buf "ε"
  | _::_ -> Util.pp_list ~sep:"." (fun b i -> Printf.bprintf buf "%d" i) buf pos

let to_string pos =
  let b = Buffer.create 16 in
  pp b pos;
  Buffer.contents b

let fmt fmt pos =
  Format.pp_print_string fmt (to_string pos)

(** {2 Position builder} *)

module Build = struct
  type t =
    | P of position
    | L of position Lazy.t
  
  let empty = P []

  let of_pos pos = P pos

  let cons i t = match t with
    | P pos -> P (i :: pos)
    | L pos -> P (i :: Lazy.force pos)

  let add t i = match t with
    | P [] -> P [i]
    | P (([_] | [_;_]) as pos) -> P (pos @ [i])
    | P pos -> L (lazy (pos @ [i]))
    | L pos -> L (lazy (Lazy.force pos @ [i]))

  let append t pos = match t with
    | P [] -> P pos
    | P (([_] | [_;_]) as pos') -> P (pos' @ pos)
    | P pos' -> L (lazy (pos' @ pos))
    | L pos' -> L (lazy (Lazy.force pos' @ pos))

  let to_pos = function
    | P pos -> pos
    | L pos -> Lazy.force pos

  let pp buf t = pp buf (to_pos t)
  let fmt formatter t = fmt formatter (to_pos t)
end
