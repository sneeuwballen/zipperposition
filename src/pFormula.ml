
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

module F = Formula.FO
type form = F.t

type t = {
  form : F.t;
  proof : Proof.t;
  mutable id : int;
  mutable simpl_to : t option;
}

type pform = t

let eq t1 t2 = F.eq t1.form t2.form && Proof.eq t1.proof t2.proof
let hash t = Hash.hash_int2 (F.hash t.form) (Proof.hash t.proof)
let cmp t1 t2 =
  let c = F.cmp t1.form t2.form in
  if c <> 0 then c else Proof.cmp t1.proof t2.proof

let eq_noproof t1 t2 = F.eq t1.form t2.form

let cmp_noproof t1 t2 = F.cmp t1.form t2.form

module H = Hashcons.Make(struct
  type t = pform
  let equal pf1 pf2 = F.eq pf1.form pf2.form
  let hash pf = F.hash pf.form
  let tag i pf = pf.id <- i
end)

let get_form t = t.form

let get_proof t = t.proof

let to_sourced t =
  match t.proof.Proof.kind with
  | Proof.File (_, file, name) -> Some (t.form, file, name)
  | _ -> None

let rec _follow_simpl n pf =
  if n > 10_000 then failwith (Util.sprintf "follow_simpl loops on %a" F.pp pf.form);
  match pf.simpl_to with
  | None -> pf
  | Some pf' -> _follow_simpl (n+1) pf'

let follow_simpl pf = _follow_simpl 0 pf

let create ?(follow=false) form proof =
  let pf = H.hashcons { form; proof; id= ~-1; simpl_to=None; } in
  if follow
    then follow_simpl pf
    else pf

let of_sourced ?(role="axiom") (f, file,name) =
  let proof = Proof.mk_f_file ~role ~file ~name f in
  create f proof

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
