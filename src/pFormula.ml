
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

(** {6 Formulas with Proofs} *)

open Logtk

module F = Formula

type t = {
  form : Formula.t;
  proof : Proof.t;
}

type pform = t

let eq t1 t2 = F.eq t1.form t2.form && Proof.eq t1.proof t2.proof
let hash t = Hash.hash_int2 (F.hash t.form) (Proof.hash t.proof)
let cmp t1 t2 =
  let c = F.compare t1.form t2.form in
  if c <> 0 then c else Proof.cmp t1.proof t2.proof

let create form proof = { form; proof; }

let get_form t = t.form

let get_proof t = t.proof

let of_sourced (f, file,name) =
  let proof = Proof.mk_f_axiom f ~file ~name in
  create f proof

let to_sourced t =
  match t.proof with
  | Proof.InferForm (f, {Proof.parents=[|Proof.Axiom (file,name)|]}) ->
    Some ((t.form, file, name))
  | _ -> None

let signature t = F.signature t.form

let signature_seq ?init seq =
  F.signature_seq ?signature:init (Sequence.map get_form seq)

let pp buf t = F.pp buf t.form
let to_string t = F.to_string t.form
let fmt fmt t = F.fmt fmt t.form

(** {2 Set of formulas} *)

module FSet = struct
  module H = Hashtbl.Make(struct
    type t = pform
    let equal = eq
    let hash = hash
  end)

  type t = unit H.t

  let create () = H.create 15

  let eq s1 s2 =
    H.length s1 = H.length s2 &&
    try
      H.iter
        (fun pf () -> if not (H.mem s2 pf) then raise Exit)
        s1;
      true
    with Exit -> false

  let add set pf = H.replace set pf ()

  let remove set pf = H.remove set pf

  let flatMap set f =
    let s' = H.create (H.length set) in
    H.iter
      (fun pf () ->
        let l = f pf in
        List.iter (fun pf -> add s' pf) l)
      set;
    s'

  let iter set k = H.iter (fun pf () -> k pf) set

  let of_seq ?(init=create ()) seq =
    Sequence.iter (add init) seq;
    init

  let to_seq set =
    Sequence.from_iter (fun k -> iter set k)

  let of_list l =
    let s = create () in
    List.iter (add s) l;
    s

  let to_list set =
    let l = ref [] in
    iter set (fun x -> l := x :: !l);
    !l

  let size set = H.length set
end

(** {2 Transformations} *)

module TransformDag = Transform.MakeDAG(struct
  type t = pform

  let to_form pf = pf.form

  let of_form ~rule ~parents form =
    let proof = match parents with
    | [] -> Proof.mk_f_axiom form ~file:"" ~name:rule
    | _::_ ->
      let premises = List.map get_proof parents in
      Proof.mk_f_step ~esa:true form ~rule premises
    in
    { form; proof; }
end)
