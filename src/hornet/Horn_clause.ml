
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module Pos = Position
module PW = Position.With

type clause = Hornet_types.clause
type proof = Hornet_types.proof
type constraint_ = Hornet_types.c_constraint_

type t = Hornet_types.horn_clause
type horn_clause = t

(** {2 Basics} *)

let make =
  let n_ = ref 0 in
  fun ?(constr=[]) ?(unordered_depth=0) head body proof ->
    let hc_id = !n_ in
    incr n_;
    { hc_id;
      hc_head=head;
      hc_unordered_depth=unordered_depth;
      hc_body=body;
      hc_proof=proof;
      hc_constr=constr;
    }

let equal a b = a.hc_id = b.hc_id
let hash a = Hash.int a.hc_id
let compare a b = CCInt.compare a.hc_id b.hc_id

let head c = c.hc_head
let body c = c.hc_body
let proof c = c.hc_proof
let constr c = c.hc_constr
let unordered_depth c = c.hc_unordered_depth

let body_seq c = IArray.to_seq (body c)
let body_l c = IArray.to_list (body c)

let body_len c = IArray.length (body c)

let body0 c =
  if IArray.length (body c) = 0
  then None
  else Some (IArray.get (body c) 0)

let body0_exn c = match body0 c with
  | Some c -> c
  | None -> invalid_arg "Horn_clause.body0_exn: empty body"

let body_get c n =
  if n < 0 || n >= IArray.length (body c) then invalid_arg "Horn.body_get";
  IArray.get (body c) n

let body_tail c =
  let n = IArray.length (body c) in
  if n = 0 then invalid_arg "Horn_clause.body_tail: empty body";
  IArray.init (n-1) (fun i -> IArray.get (body c) (i-1))

let pp = Hornet_types_util.pp_hclause
let to_string = Fmt.to_string pp

let head_pos c = PW.make (head c) Pos.(head stop)
let body_pos n c = PW.make (body_get c n) Pos.(arg n @@ body @@ stop)
let body0_pos = body_pos 0

(** {2 Helpers} *)

let is_trivial c =
  Lit.is_trivial (head c) ||
  IArray.exists Lit.is_absurd (body c) ||
  List.exists
    (function
      | C_dismatch d -> Dismatching_constr.is_absurd d)
    (constr c)

(* NOTE: some constraints will have to be solved all at once
   to obtain an actual substitution *)
let constr_are_sat (l:constraint_ list): bool =
  List.for_all
    (function
      | C_dismatch d -> not (Dismatching_constr.is_absurd d))
    l

let is_absurd c =
  Lit.is_absurd (head c) &&
  body_len c = 0 &&
  constr_are_sat (constr c)

let is_ground c =
  Lit.is_ground (head c) &&
  IArray.for_all Lit.is_ground (body c)

(** {2 Containers} *)

module As_key = struct
  type t = horn_clause
  let equal = equal
  let hash = hash
end
module Tbl = CCHashtbl.Make(As_key)

(** {2 Pairing with Position} *)

module With_pos = struct
  type t = horn_clause Position.With.t
  let compare = PW.compare compare
  let pp = PW.pp pp
  let to_string = Fmt.to_string pp
end

(** {2 Substitutions} *)

let apply_subst_constr ~renaming subst (c,sc) = match c with
  | C_dismatch d ->
    C_dismatch (Dismatching_constr.apply_subst ~renaming subst (d,sc))

let apply_subst_constr_l ~renaming subst (l,sc) =
  List.map
    (fun c -> apply_subst_constr ~renaming subst (c,sc))
    l
