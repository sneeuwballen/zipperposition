
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Literal} *)

module T = TypedSTerm
module F = T.Form

type form = TypedSTerm.t
type term = TypedSTerm.t

exception NotALit of form

let () = Printexc.register_printer
    (function
      | NotALit f -> Some (CCFormat.sprintf "@[<2>formula@ @[%a@]@ is not a lit@]" T.pp f)
      | _ -> None)

let not_lit f = raise (NotALit f)

type +'t t =
  | True
  | False
  | Atom of 't * bool
  | Eq of 't * 't
  | Neq of 't * 't

type 'a lit = 'a t

let map ~f = function
  | True -> True
  | False -> False
  | Atom (t, b) -> Atom (f t, b)
  | Eq (a,b) -> Eq (f a, f b)
  | Neq (a,b) -> Neq (f a, f b)

let fold f acc = function
  | True
  | False -> acc
  | Atom (t, _) -> f acc t
  | Eq (a,b)
  | Neq (a,b) -> f (f acc a) b

let iter ~f = function
  | True
  | False -> ()
  | Atom (t, _) -> f t
  | Eq (a,b)
  | Neq (a,b) -> f a; f b

let to_seq l f = match l with
  | True
  | False -> ()
  | Atom (t, _) -> f t
  | Eq (a,b)
  | Neq (a,b) -> f a; f b

let equal eq a b = match a, b with
  | True, True
  | False, False -> true
  | Atom (t1,b1), Atom (t2,b2) -> b1=b2 && eq t1 t2
  | Eq (a1,a2), Eq (b1,b2)
  | Neq (a1,a2), Neq (b1,b2) ->
    (eq a1 b1 && eq a2 b2) ||
    (eq a1 b2 && eq a2 b1)
  | True, _
  | False, _
  | Atom _, _
  | Eq _, _
  | Neq _, _ -> false

let true_ = True
let false_ = False
let eq a b = Eq (a,b)
let neq a b = Neq (a,b)
let atom a b = Atom (a,b)
let atom_true a = atom a true
let atom_false a = atom a false

let is_true = function True -> true | _ -> false
let is_false = function False -> true | _ -> false

let sign = function
  | True
  | Eq _ -> true
  | Atom (_, b) -> b
  | Neq _
  | False -> false

let is_pos l = sign l
let is_neg l = not (sign l)

let negate = function
  | True -> False
  | False -> True
  | Atom (t,sign) -> atom t (not sign)
  | Eq (a,b) -> neq a b
  | Neq (a,b) -> eq a b

let fpf = Format.fprintf

let to_form = function
  | True -> F.true_
  | False -> F.false_
  | Atom (t, true) -> t
  | Atom (t, false) -> F.not_ t
  | Eq (a,b) -> F.eq a b
  | Neq (a,b) -> F.neq a b

let of_form f = match F.view f with
  | F.Not f' ->
    begin match F.view f' with
      | F.Atom t -> Atom (t, false)
      | _ -> not_lit f
    end
  | F.Eq (t1,t2) -> Eq (t1,t2)
  | F.Neq (t1,t2) -> Neq (t1,t2)
  | F.Atom t -> Atom (t,true)
  | F.True -> True
  | F.False -> False
  | F.Or _
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> not_lit f

let pp ppt out = function
  | True -> fpf out "true"
  | False -> fpf out "false"
  | Atom (t, true) -> ppt out t
  | Atom (t, false) -> fpf out "@[<2>¬@ @[%a@]@]" ppt t
  | Eq (t1,t2) -> fpf out "@[%a@ =@ %a@]" ppt t1 ppt t2
  | Neq (t1,t2) -> fpf out "@[%a@ ≠@ %a@]" ppt t1 ppt t2

let to_string ppt = CCFormat.to_string (pp ppt)

module TPTP = struct
  let pp ppt out = function
    | True -> fpf out "$true"
    | False -> fpf out "$false"
    | Atom (t, true) -> ppt out t
    | Atom (t, false) -> fpf out "@[<2>~@ @[%a@]@]" ppt t
    | Eq (t1,t2) -> fpf out "@[%a@ =@ %a@]" ppt t1 ppt t2
    | Neq (t1,t2) -> fpf out "@[%a@ !=@ %a@]" ppt t1 ppt t2

  let to_string ppt = CCFormat.to_string (pp ppt)
end

module ZF = struct
  let pp ppt out = function
    | True -> fpf out "true"
    | False -> fpf out "false"
    | Atom (t, true) -> ppt out t
    | Atom (t, false) -> fpf out "@[<2>~@ @[%a@]@]" ppt t
    | Eq (t1,t2) -> fpf out "@[%a@ =@ %a@]" ppt t1 ppt t2
    | Neq (t1,t2) -> fpf out "@[%a@ !=@ %a@]" ppt t1 ppt t2

  let to_string ppt = CCFormat.to_string (pp ppt)
end

let pp_in o pp_x = match o with
  | Output_format.O_zf -> ZF.pp pp_x
  | Output_format.O_normal -> pp pp_x
  | Output_format.O_tptp -> TPTP.pp pp_x
  | Output_format.O_none -> (fun _ _ -> ())
