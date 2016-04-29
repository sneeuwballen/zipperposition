
(** {1 Simple Clause} *)

open Libzipperposition

type flag = int

type t = {
  id : int; (** unique ID of the clause *)
  lits : Literal.t array; (** the literals *)
  trail : Trail.t; (** boolean trail *)
  mutable flags : flag; (** boolean flags for the clause *)
}

let id_count_ = ref 0

(** {2 Basics} *)

let make ~trail lits =
  let id = !id_count_ in
  incr id_count_;
  { lits; trail; id; flags=0; }

let equal c1 c2 = c1.id = c2.id
let compare c1 c2 = c1.id - c2.id
let id c = c.id
let lits c = c.lits
let trail c = c.trail
let length c = Array.length c.lits
let is_empty c = length c = 0 && Trail.is_empty c.trail

let update_trail f c = make ~trail:(f c.trail) c.lits

(** {2 Flags} *)

let new_flag =
  let flag_gen = Util.Flag.create () in
  fun () -> Util.Flag.get_new flag_gen

let flag_lemma = new_flag ()
let flag_persistent = new_flag ()
let flag_redundant = new_flag ()
let flag_backward_simplified = new_flag()

let set_flag flag c truth =
  if truth
  then c.flags <- c.flags lor flag
  else c.flags <- c.flags land (lnot flag)

let get_flag flag c = (c.flags land flag) != 0

let mark_redundant c = set_flag flag_redundant c true
let is_redundant c = get_flag flag_redundant c
let mark_backward_simplified c = set_flag flag_backward_simplified c true
let is_backward_simplified c = get_flag flag_backward_simplified c

(** {2 IO} *)

(* list of free variables *)
let vars_ lits =
  Literals.Seq.vars lits
  |> FOTerm.VarSet.of_seq
  |> FOTerm.VarSet.to_list

let pp_trail out trail =
  if not (Trail.is_empty trail)
  then
    Format.fprintf out "@ @<2>← @[<hv>%a@]"
      (CCFormat.seq ~start:"" ~stop:"" ~sep:" ⊓ " BBox.pp)
      (Trail.to_seq trail)

let pp out c =
  let pp_vars out = function
    | [] -> ()
    | l ->
      Format.fprintf out "forall @[%a@].@ "
        (Util.pp_list ~sep:" " Type.pp_typed_var) l
  in
  Format.fprintf out "@[%a@[<2>%a%a@]@]"
    pp_vars (vars_ c.lits) Literals.pp c.lits pp_trail c.trail;
  ()

let rec pp_lits_tstp out lits = match lits with
  | [| |] -> CCFormat.string out "$false"
  | [| l |] -> Literal.pp_tstp out l
  | _ -> Format.fprintf out "(%a)" Literals.pp_tstp lits
(* print quantified literals *)
and pp_closed_lits_tstp out lits =
  let pp_vars out = function
    | [] -> ()
    | l ->
      Format.fprintf out "![@[%a@]]:@ "
        (Util.pp_list ~sep:", " Type.TPTP.pp_typed_var) l
  in
  Format.fprintf out "@[<2>%a%a@]" pp_vars (vars_ lits) pp_lits_tstp lits

(* print a trail in TPTP *)
and pp_trail_tstp out trail =
  (* print a single boolean box *)
  let pp_box_unsigned out b = match BBox.payload b with
    | BBox.Case (l, r) ->
      let l = Ind_cst.cst_to_term l in
      let r = Ind_cst.case_to_term r in
      pp_lits_tstp out [| Literal.mk_eq l r |]
    | BBox.Clause_component lits ->
      CCFormat.within "(" ")" pp_closed_lits_tstp out lits
    | BBox.Fresh -> failwith "cannot print <fresh> boolean box"
  in
  let pp_box out b =
    if BBox.Lit.sign b then pp_box_unsigned out b
    else Format.fprintf out "@[~@ %a@]" pp_box_unsigned b
  in
  Format.fprintf out "@[<hv>%a@]"
    (CCFormat.seq ~start:"" ~stop:"" ~sep:" & " pp_box)
    (Trail.to_seq trail)

let pp_tstp out c =
  if Trail.is_empty c.trail
  then pp_closed_lits_tstp out c.lits
  else
    Format.fprintf out "@[<2>(@[%a@])@ <= (%a)@]"
      pp_closed_lits_tstp c.lits pp_trail_tstp c.trail

(* TODO: if all vars are [:term] and trail is empty, use CNF; else use TFF *)
let pp_tstp_full out c =
  Format.fprintf out "@[<2>tff(%d, plain,@ %a).@]" c.id pp_tstp c
