
(** {1 Simple Clause} *)

open Logtk

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
let compare c1 c2 = Pervasives.compare c1.id c2.id
let id c = c.id
let hash c = Hashtbl.hash c.id
let lits c = c.lits
let trail c = c.trail
let length c = Array.length c.lits
let is_empty c = length c = 0 && Trail.is_empty c.trail

let update_trail f c = make ~trail:(f c.trail) c.lits

let add_trail_ trail f =
  let module F = TypedSTerm.Form in
  if Trail.is_empty trail
  then f
  else F.imply (Trail.to_s_form trail) f

let to_s_form ?allow_free_db ?(ctx=Term.Conv.create()) c =
  let module F = TypedSTerm.Form in
  Literals.Conv.to_s_form ?allow_free_db ~ctx (lits c)
  |> add_trail_ (trail c)
  |> F.close_forall

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

let pp_trail out trail =
  if not (Trail.is_empty trail)
  then
    Format.fprintf out "@ @<2>← @[<hv>%a@]"
      (Util.pp_seq ~sep:" ⊓ " BBox.pp) (Trail.to_seq trail)

let pp_vars out c =
  let pp_vars out = function
    | [] -> ()
    | l ->
      Format.fprintf out "forall @[%a@].@ "
        (Util.pp_list ~sep:" " Type.pp_typed_var) l
  in
  pp_vars out (Literals.vars c.lits)

let pp out c =
  Format.fprintf out "@[%a@[<2>%a%a@]@]"
    pp_vars c Literals.pp c.lits pp_trail c.trail;
  ()

let pp_trail_zf out trail =
  Format.fprintf out "@[<hv>%a@]"
    (Util.pp_seq ~sep:" && " BBox.pp_zf) (Trail.to_seq trail)

let pp_zf out c =
  if Trail.is_empty c.trail
  then Literals.pp_zf_closed out c.lits
  else
    Format.fprintf out "@[<2>(%a)@ => (%a)@]"
      pp_trail_zf c.trail Literals.pp_zf_closed c.lits

(* print a trail in TPTP *)
let pp_trail_tstp out trail =
  (* print a single boolean box *)
  let pp_box_unsigned out b = match BBox.payload b with
    | BBox.Case p ->
      let lits = List.map Cover_set.Case.to_lit p |> Array.of_list in
      Literals.pp_tstp out lits
    | BBox.Clause_component lits ->
      CCFormat.within "(" ")" Literals.pp_tstp_closed out lits
    | BBox.Lemma f ->
      CCFormat.within "(" ")" Cut_form.pp_tstp out f
    | BBox.Fresh -> failwith "cannot print <fresh> boolean box"
  in
  let pp_box out b =
    if BBox.Lit.sign b then pp_box_unsigned out b
    else Format.fprintf out "@[~@ %a@]" pp_box_unsigned b
  in
  Format.fprintf out "@[<hv>%a@]"
    (Util.pp_seq ~sep:" & " pp_box)
    (Trail.to_seq trail)

let pp_tstp out c =
  if Trail.is_empty c.trail
  then Literals.pp_tstp_closed out c.lits
  else
    Format.fprintf out "@[<2>(@[%a@])@ <= (%a)@]"
      Literals.pp_tstp_closed c.lits pp_trail_tstp c.trail

(* TODO: if all vars are [:term] and trail is empty, use CNF; else use TFF *)
let pp_tstp_full out c =
  Format.fprintf out "@[<2>tff(%d, plain,@ %a).@]" c.id pp_tstp c

let pp_in = function
  | Output_format.O_zf -> pp_zf
  | Output_format.O_tptp -> pp_tstp
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent

(** {2 Proofs} *)

exception E_proof of t

(* conversion to simple formula after instantiation, including the
   substitution used for instantiating *)
let to_s_form_subst ~ctx subst c : _ * _ Var.Subst.t =
  let module F = TypedSTerm.Form in
  let module SP = Subst.Projection in
  let f =
    Literals.apply_subst (SP.renaming subst) (SP.subst subst) (lits c,SP.scope subst)
    |> Literals.Conv.to_s_form ~allow_free_db:true ~ctx
    |> add_trail_ (trail c)
    |> F.close_forall
  and inst_subst =
    SP.as_inst ~allow_free_db:true ~ctx subst (Literals.vars (lits c))
  in
  f, inst_subst

let proof_tc =
  Proof.Result.make_tc
    ~of_exn:(function | E_proof c -> Some c | _ -> None)
    ~to_exn:(fun c -> E_proof c)
    ~compare:compare
    ~flavor:(fun c ->
      if Literals.is_absurd (lits c)
      then if Trail.is_empty (trail c) then `Proof_of_false
        else `Absurd_lits
      else `Vanilla)
    ~to_form:(fun ~ctx c -> to_s_form ~allow_free_db:true ~ctx c)
    ~to_form_subst:to_s_form_subst
    ~pp_in
    ()

let mk_proof_res = Proof.Result.make proof_tc

let adapt p c = Proof.S.adapt p (mk_proof_res c)
