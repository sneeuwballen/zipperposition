
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
  mutable simpl_to : t option;
}

type pform = t

let eq t1 t2 = F.eq t1.form t2.form && Proof.eq t1.proof t2.proof
let hash t = Hash.hash_int2 (F.hash t.form) (Proof.hash t.proof)
let cmp t1 t2 =
  let c = F.compare t1.form t2.form in
  if c <> 0 then c else Proof.cmp t1.proof t2.proof

let eq_noproof t1 t2 = F.eq t1.form t2.form

let cmp_noproof t1 t2 = F.compare t1.form t2.form

let create form proof = { form; proof; simpl_to=None; }

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

let rec follow_simpl pf = match pf.simpl_to with
  | None -> pf
  | Some pf' -> follow_simpl pf'

let simpl_to ~from ~into =
  let from = follow_simpl from in
  if eq from into
    then ()
    else from.simpl_to <- Some into

let signature t = F.signature t.form

let signature_seq ?init seq =
  F.signature_seq ?signature:init (Sequence.map get_form seq)

let pp buf t = F.pp buf t.form
let pp_tstp buf t = F.pp_tstp buf t.form
let to_string t = F.to_string t.form
let fmt fmt t = F.fmt fmt t.form

let bij ~ord = Bij.(map
  ~inject:(fun pf -> pf.form, pf.proof)
  ~extract:(fun (form,proof) -> {form; proof; simpl_to=None; })
  (pair F.bij (Proof.bij ~ord)))

(** {2 Set of formulas} *)

module Set = struct
  include Sequence.Set.Make(struct
    type t = pform
    let compare = cmp_noproof
  end)

  let signature ?signature set =
    F.signature_seq ?signature (Sequence.map get_form (to_seq set))
end
