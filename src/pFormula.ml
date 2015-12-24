
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {6 Formulas with Proofs} *)

open Logtk

module Hash = CCHash
module F = TypedSTerm

type form = TypedSTerm.t

type t = {
  form : form;
  proof : Proof.t;
  is_conjecture : bool;
  mutable id : int;
  mutable simpl_to : t option;
}

type pform = t

let equal t1 t2 = F.equal t1.form t2.form && Proof.equal t1.proof t2.proof
let compare t1 t2 =
  let c = F.compare t1.form t2.form in
  if c <> 0 then c else Proof.compare t1.proof t2.proof

let hash_fun t h = F.hash_fun t.form (Proof.hash_fun t.proof h)
let hash t = Hash.apply hash_fun t

let equal_noproof t1 t2 = F.equal t1.form t2.form

let compare_noproof t1 t2 = F.compare t1.form t2.form

module H = Hashcons.Make(struct
    type t = pform
    let equal pf1 pf2 = F.equal pf1.form pf2.form
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
  then failwith (CCFormat.sprintf "@[<2>follow_simpl loops on@ @[%a@]@]" F.pp pf.form);
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
  if not (equal from into') then from.simpl_to <- Some into

let symbols ?init f =
  assert false (* TODO F.symbols ?init f.form *)

let pp out t = F.pp out t.form
let pp_tstp out t = F.TPTP.pp out t.form
let to_string t = F.to_string t.form

(** {2 Set of formulas} *)

module Set = struct
  include CCSet.Make(struct
      type t = pform
      let compare = compare_noproof
    end)

  let symbols ?(init=ID.Set.empty) set =
    fold
      (fun f set -> symbols ~init:set f)
      set init
end
