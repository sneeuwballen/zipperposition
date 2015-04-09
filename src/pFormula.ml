
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

module Hash = CCHash
module F = Formula.FO

type form = F.t

type t = {
  form : F.t;
  proof : Proof.t;
  is_conjecture : bool;
  mutable id : int;
  mutable simpl_to : t option;
}

type pform = t

let eq t1 t2 = F.eq t1.form t2.form && Proof.eq t1.proof t2.proof
let cmp t1 t2 =
  let c = F.cmp t1.form t2.form in
  if c <> 0 then c else Proof.cmp t1.proof t2.proof

let hash_fun t h = F.hash_fun t.form (Proof.hash_fun t.proof h)
let hash t = Hash.apply hash_fun t

let eq_noproof t1 t2 = F.eq t1.form t2.form

let cmp_noproof t1 t2 = F.cmp t1.form t2.form

module H = Hashcons.Make(struct
  type t = pform
  let equal pf1 pf2 = F.eq pf1.form pf2.form
  let hash pf = F.hash pf.form
  let tag i pf = pf.id <- i
end)

let form t = t.form
let proof t = t.proof
let id t = t.id
let is_conjecture t = Proof.is_conjecture t.proof

let to_sourced t =
  let module F = Proof.FileInfo in
  match Proof.kind t.proof with
  | Proof.File {F.filename=file; F.name; } ->
      Some (Sourced.make ~file ~name ~is_conjecture:(is_conjecture t) t.form)
  | Proof.Inference _
  | Proof.Simplification _
  | Proof.Esa _
  | Proof.Trivial  -> None

let rec _follow_simpl n pf =
  if n > 10_000
    then failwith (Util.sprintf "follow_simpl loops on %a" F.pp pf.form);
  match pf.simpl_to with
  | None -> pf
  | Some pf' -> _follow_simpl (n+1) pf'

let follow_simpl pf = _follow_simpl 0 pf

let create ?(is_conjecture=false) ?(follow=false) form proof =
  let pf = H.hashcons { form; proof; id= ~-1; is_conjecture; simpl_to=None; } in
  if follow
    then follow_simpl pf
    else pf

let of_sourced ?(role="axiom") src =
  let open Sourced in
  let conjecture = src.is_conjecture in
  let proof = Proof.mk_f_file ~conjecture
    ~role ~file:src.name ~name:src.name src.content in
  create src.content proof

let simpl_to ~from ~into =
  let from = follow_simpl from in
  let into' = follow_simpl into in
  if not (eq from into')
    then from.simpl_to <- Some into

let symbols ?init f = F.symbols ?init f.form

let pp buf t = F.pp buf t.form
let pp_tstp buf t = F.TPTP.pp buf t.form
let to_string t = F.to_string t.form
let fmt fmt t = F.fmt fmt t.form

(*
let bij = Bij.(map
  ~inject:(fun pf -> pf.form, pf.proof)
  ~extract:(fun (form,proof) -> create form proof)
  (pair F.bij Proof.bij))
*)

(** {2 Set of formulas} *)

module Set = struct
  include Sequence.Set.Make(struct
    type t = pform
    let compare = cmp_noproof
  end)

  let symbols ?(init=Symbol.Set.empty) set =
    fold
      (fun f set -> symbols ~init:set f)
      set init
end
