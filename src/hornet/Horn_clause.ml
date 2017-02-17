
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module Pos = Position
module PW = Position.With
module BV = CCBV

type t = Hornet_types.horn_clause
type horn_clause = t

(** {2 Basics} *)

let make =
  let n_ = ref 0 in
  fun ~trail ~constr ~unordered_depth ~label head body proof ->
    let hc_id = !n_ in
    incr n_;
    { hc_id;
      hc_head=head;
      hc_unordered_depth=unordered_depth;
      hc_body=body;
      hc_proof=proof;
      hc_trail=trail;
      hc_constr=constr;
      hc_label=label;
      hc_status=(HC_dead,0);
    }

let equal a b = a.hc_id = b.hc_id
let hash a = Hash.int a.hc_id
let compare a b = CCInt.compare a.hc_id b.hc_id

let pp = Hornet_types_util.pp_hclause
let to_string = Fmt.to_string pp

let head c = c.hc_head
let body c = c.hc_body
let proof c = c.hc_proof
let trail c = c.hc_trail
let label c = c.hc_label
let constr c = c.hc_constr
let unordered_depth c = c.hc_unordered_depth
let status c = c.hc_status

let set_status c new_st new_cycle =
  let old_st, old_cycle = c.hc_status in
  assert (old_cycle <= new_cycle);
  begin match old_st, new_st with
    | HC_dead, HC_alive when old_cycle < new_cycle ->
      () (* only fine for new cycle *)
    | HC_alive, HC_dead
    | HC_dead, HC_dead -> ()
    | _ ->
      Util.errorf
        ~where:"HC.set_status"
        "for `@[%a@]`,@ wrong change@ @[<2>`%a`[time %d]@ -> `%a`[time %d]@]"
        pp c
        Hornet_types_util.pp_hc_status old_st old_cycle
        Hornet_types_util.pp_hc_status new_st new_cycle
  end;
  c.hc_status <- (new_st,new_cycle);
  ()

let body_seq c = IArray.to_seq (body c)
let body_l c = IArray.to_list (body c)

let body_len c = IArray.length (body c)

let body0 c =
  if IArray.length (body c) = 0
  then None
  else Some (IArray.get (body c) 0)

let body0_exn c = match body0 c with
  | Some c -> c
  | None ->
    Util.errorf ~where:"Horn_clause.body0_exn" "empty body in `@[%a@]`" pp c

let body_get c n =
  if n < 0 || n >= IArray.length (body c) then (
    Util.errorf ~where:"Horn.body_get" "%d in `@[%a@]`" n pp c;
  );
  IArray.get (body c) n

let body_tail c =
  let n = IArray.length (body c) in
  if n = 0 then Util.errorf ~where:"Horn_clause.body_tail" "empty body `@[%a@]`" pp c;
  IArray.init (n-1) (fun i -> IArray.get (body c) (i+1))

let head_pos c = PW.make (head c) Pos.(head stop)
let body_pos n c = PW.make (body_get c n) Pos.(arg n @@ body @@ stop)
let body0_pos = body_pos 0

(** {2 Helpers} *)

let is_trivial c =
  let res =
    Lit.is_trivial (head c) ||
    IArray.exists Lit.is_absurd (body c) ||
    Trail.is_absurd (trail c) ||
    Constraint.is_absurd (constr c) ||
    Label.has_no_ground_instance (label c)
  in
  if res then (
    Util.debugf 5 "(@[<2>is_trivial %a@])" (fun k->k pp c);
  );
  res

(* NOTE: some constraints will have to be solved all at once
   to obtain an actual substitution *)
let constr_are_sat (c:c_constraint): bool = not (Constraint.is_absurd c)

let is_absurd c =
  Lit.is_absurd (head c) &&
  body_len c = 0 &&
  not (Trail.is_absurd (trail c)) &&
  not (Label.has_no_ground_instance (label c)) &&
  constr_are_sat (constr c)

let is_ground c =
  Lit.is_ground (head c) &&
  IArray.for_all Lit.is_ground (body c)

let is_unit_pos c =
  not (Lit.is_absurd (head c)) &&
  IArray.length (body c) = 0

let vars_seq = Hornet_types_util.vars_of_hclause

(** {2 Unification} *)

let prof_variant = Util.mk_profiler "hornet.horn_clause_variant"

let variant_ subst (c1,sc1) (c2,sc2) : Subst.t Sequence.t =
  let variant_constr subst (c1,sc1)(c2,sc2) =
    Constraint.variant ~subst (c1,sc1) (c2,sc2)
  in
  let {
    hc_unordered_depth=_;
    hc_body=a1;
    hc_head=h1;
    hc_constr=c1;
    hc_trail=tr1;
    hc_id=id1;
    hc_status=_;
    hc_label=lab1;
    hc_proof=_;
  } = c1
  and {
    hc_unordered_depth=_;
    hc_body=a2;
    hc_head=h2;
    hc_constr=c2;
    hc_trail=tr2;
    hc_id=id2;
    hc_label=lab2;
    hc_status=_;
    hc_proof=_;
  } = c2 in
  if id1=id2 then Sequence.return subst
  else if Hornet_types_util.equal_bool_trail tr1 tr2 then (
    Lit.variant ~subst (h1,sc1)(h2,sc2)
    |> Sequence.flat_map
      (fun subst ->
         Unif.unif_array_com subst
           (IArray.to_array_unsafe a1,sc1)
           (IArray.to_array_unsafe a2,sc2)
           ~op:(fun subst x y -> Lit.variant ~subst x y))
    |> Sequence.flat_map
      (fun subst -> variant_constr subst (c1,sc1)(c2,sc2))
    |> Sequence.flat_map
      (fun subst -> Label.variant ~subst (lab1,sc1)(lab2,sc2))
  ) else Sequence.empty

let variant ?(subst=Subst.empty) a b k =
  Util.with_prof prof_variant (fun k -> variant_ subst a b k) k

let equal_mod_alpha (c1:t) (c2:t) : bool =
  not (variant (c1,0)(c2,1) |> Sequence.is_empty)

let hash_mod_alpha c: int =
  Hash.combine5 42
    (Lit.hash_mod_alpha (head c))
    (IArray.hash_comm Lit.hash_mod_alpha (body c))
    (Hash.list_comm
       (fun (lazy b_lit) -> Hornet_types_util.hash_bool_lit b_lit)
       (trail c))
    (Label.hash_mod_alpha (label c))

(** {2 Containers} *)

module As_key = struct
  type t = horn_clause
  let equal = equal
  let hash = hash
end
module Tbl = CCHashtbl.Make(As_key)

module Tbl_mod_alpha = CCHashtbl.Make(struct
    type t = horn_clause
    let equal = equal_mod_alpha
    let hash = hash_mod_alpha
  end)

(** {2 Pairing with Position} *)

module With_pos = struct
  type t = horn_clause Position.With.t
  let compare = PW.compare compare
  let pp = PW.pp pp
  let to_string = Fmt.to_string pp
end
