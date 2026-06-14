(* This file is free software, part of Logtk. See file "license" for more details. *)
(** {1 Builtin Objects} *)

let _t_bigger_false = ref false

type t = Hstring.t (* "private" in .mli *)

type view_t = Builtin_gen.view_t =
  | Not
  | And
  | Or
  | Imply
  | Equiv
  | Xor
  | Eq
  | Neq
  | HasType
  | True
  | False
  | Arrow
  | Wildcard
  | Multiset
  | TType
  | Prop
  | Term
  | ForallConst
  | ExistsConst
  | ChoiceConst
  | Grounding
  | TyInt
  | TyRat
  | TyReal
  | Floor
  | Ceiling
  | Truncate
  | Round
  | Prec
  | Succ
  | Sum
  | Difference
  | Uminus
  | Product
  | Quotient
  | Quotient_e
  | Quotient_t
  | Quotient_f
  | Remainder_e
  | Remainder_t
  | Remainder_f
  | Is_int
  | Is_rat
  | To_int
  | To_rat
  | Less
  | Lesseq
  | Greater
  | Greatereq
  | Box_opaque
  | BComb
  | CComb
  | IComb
  | KComb
  | SComb
  | Distinct

type fixity = Builtin_gen.fixity =
  | Infix_binary
  | Infix_nary
  | Prefix

type payload_view =
  | Int of Z.t
  | Rat of Q.t
  | Real of string
  | Pseudo_de_bruijn of int

(* ── Generated delegation ── *)
let of_hstring h = h
let view = Builtin_gen.view
let make_view = Builtin_gen.make_view
let view_order = Builtin_gen.view_order
let fixity = Builtin_gen.fixity
let is_prefix = Builtin_gen.is_prefix
let is_infix = Builtin_gen.is_infix
let is_logical_op = Builtin_gen.is_logical_op
let is_logical_binop = Builtin_gen.is_logical_binop
let is_flattened_logical = Builtin_gen.is_flattened_logical
let is_quantifier = Builtin_gen.is_quantifier
let is_combinator = Builtin_gen.is_combinator
let is_arith = Builtin_gen.is_arith
let is_not_numeric = Builtin_gen.is_not_numeric
let to_string = Builtin_gen.to_string
let pp = Builtin_gen.pp
let to_int_ = Builtin_gen.to_int_

(* Constants *)
let true_ = Builtin_gen.true_
let false_ = Builtin_gen.false_
let not_ = Builtin_gen.not_
let and_ = Builtin_gen.and_
let or_ = Builtin_gen.or_
let eq = Builtin_gen.eq
let neq = Builtin_gen.neq
let imply = Builtin_gen.imply
let equiv = Builtin_gen.equiv
let xor = Builtin_gen.xor
let arrow = Builtin_gen.arrow
let has_type = Builtin_gen.has_type
let tType = Builtin_gen.tType
let prop = Builtin_gen.prop
let term = Builtin_gen.term
let wildcard = Builtin_gen.wildcard
let multiset = Builtin_gen.multiset
let grounding = Builtin_gen.grounding
let forallConst = Builtin_gen.forallConst
let existsConst = Builtin_gen.existsConst
let choiceConst = Builtin_gen.choiceConst
let ty_int = Builtin_gen.ty_int
let ty_rat = Builtin_gen.ty_rat
let ty_real = Builtin_gen.ty_real
let floor_ = Builtin_gen.floor_
let ceiling_ = Builtin_gen.ceiling_
let truncate_ = Builtin_gen.truncate_
let round_ = Builtin_gen.round_
let prec_ = Builtin_gen.prec_
let succ_ = Builtin_gen.succ_
let sum_ = Builtin_gen.sum_
let difference_ = Builtin_gen.difference_
let uminus_ = Builtin_gen.uminus_
let product_ = Builtin_gen.product_
let quotient_ = Builtin_gen.quotient_
let quotient_e = Builtin_gen.quotient_e
let quotient_t = Builtin_gen.quotient_t
let quotient_f = Builtin_gen.quotient_f
let remainder_e = Builtin_gen.remainder_e
let remainder_t = Builtin_gen.remainder_t
let remainder_f = Builtin_gen.remainder_f
let is_int_ = Builtin_gen.is_int_
let is_rat_ = Builtin_gen.is_rat_
let to_int_c = Builtin_gen.to_int_c
let to_rat_c = Builtin_gen.to_rat_c
let less_ = Builtin_gen.less_
let lesseq_ = Builtin_gen.lesseq_
let greater_ = Builtin_gen.greater_
let greatereq_ = Builtin_gen.greatereq_
let box_opaque = Builtin_gen.box_opaque
let bComb = Builtin_gen.bComb
let cComb = Builtin_gen.cComb
let iComb = Builtin_gen.iComb
let kComb = Builtin_gen.kComb
let sComb = Builtin_gen.sComb
let distinct = Builtin_gen.distinct

module Arith = struct
  let floor = floor_
  let ceiling = ceiling_
  let truncate = truncate_
  let round = round_
  let prec = prec_
  let succ = succ_
  let sum = sum_
  let difference = difference_
  let uminus = uminus_
  let product = product_
  let quotient = quotient_
  let quotient_e = quotient_e
  let quotient_t = quotient_t
  let quotient_f = quotient_f
  let remainder_e = remainder_e
  let remainder_t = remainder_t
  let remainder_f = remainder_f
  let is_int = is_int_
  let is_rat = is_rat_
  let to_int = to_int_c
  let to_rat = to_rat_c
  let less = less_
  let lesseq = lesseq_
  let greater = greater_
  let greatereq = greatereq_
end

(* ── Payload handling ── *)

let make_payload (p : payload_view) : t =
  match p with
  | Int n -> Hstring.make ("$int$" ^ Z.to_string n)
  | Rat q -> Hstring.make ("$rat$" ^ Q.to_string q)
  | Real s -> Hstring.make ("$real$" ^ s)
  | Pseudo_de_bruijn i -> Hstring.make ("$pdb$" ^ string_of_int i)

let is_payload (b : t) : payload_view option =
  let s = Hstring.to_string b in
  let len = String.length s in
  if len >= 6 then (
    let prefix = String.sub s 0 5 in
    let rest = String.sub s 5 (len - 5) in
    match prefix with
    | "$int$" -> (try Some (Int (Z.of_string rest)) with _ -> None)
    | "$rat$" -> (try Some (Rat (Q.of_string rest)) with _ -> None)
    | "$real$" -> Some (Real rest)
    | "$pdb$" ->
      (try Some (Pseudo_de_bruijn (int_of_string rest)) with _ -> None)
    | _ -> None
  ) else
    None

let payload_code (b : t) : int =
  match is_payload b with
  | Some (Int _) -> 16
  | Some (Rat _) -> 17
  | Some (Real _) -> 71
  | Some (Pseudo_de_bruijn _) -> 100
  | None ->
    (match view b with
    | Some v -> view_order v
    | None -> -1)

(* ── Comparison / hashing ── *)

let compare a b =
  match is_payload a, is_payload b with
  | Some (Int i), Some (Int j) -> Z.compare i j
  | Some (Rat i), Some (Rat j) -> Q.compare i j
  | Some (Real s1), Some (Real s2) -> String.compare s1 s2
  | Some (Pseudo_de_bruijn i1), Some (Pseudo_de_bruijn i2) -> Int.compare i1 i2
  | Some _, Some _ -> payload_code a - payload_code b
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None ->
    (match view a, view b with
    | Some va, Some vb -> view_order va - view_order vb
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> Hstring.compare a b)

let equal a b = compare a b = 0

let hash b =
  match is_payload b with
  | Some (Int n) -> Hash.combine2 1 (Z.hash n)
  | Some (Rat r) -> Hash.combine2 2 (Hash.string (Q.to_string r))
  | Some (Real s) -> Hash.combine2 5 (Hash.string s)
  | Some (Pseudo_de_bruijn i) -> Hash.combine2 4 (Hash.int i)
  | None ->
    (match view b with
    | Some v -> Hash.combine2 3 (Hash.int (view_order v))
    | None -> Hash.combine2 3 (Hstring.hash b))

(* ── Numeric helpers ── *)

let ty b =
  match is_payload b with
  | Some (Int _) -> `Int
  | Some (Rat _) -> `Rat
  | _ -> `Other

let mk_int s = make_payload (Int s)
let of_int i = mk_int (Z.of_int i)
let int_of_string s = mk_int (Z.of_string s)
let mk_rat s = make_payload (Rat s)
let of_rat i j = mk_rat (Q.of_ints i j)
let rat_of_string s = mk_rat (Q.of_string s)

let is_int b =
  match is_payload b with
  | Some (Int _) -> true
  | _ -> false

let is_rat b =
  match is_payload b with
  | Some (Rat _) -> true
  | _ -> false

let is_numeric b =
  match is_payload b with
  | Some (Int _) | Some (Rat _) -> true
  | _ -> false

let as_int b = payload_code b

(* ── Collections ── *)

module Map = Iter.Map.Make (struct
  type t_ = t
  type t = t_

  let compare = compare
end)

module Set = Iter.Set.Make (struct
  type t_ = t
  type t = t_

  let compare = compare
end)

module Tbl = Hashtbl.Make (struct
  type t_ = t
  type t = t_

  let equal = equal
  let hash = hash
end)

(* ── Tag ── *)

module Tag = struct
  type t =
    | T_lia
    | T_lra
    | T_ho
    | T_live_cnf
    | T_ho_norm
    | T_dont_increase_depth
    | T_ext
    | T_ind
    | T_data
    | T_distinct
    | T_ac of Name.t
    | T_cannot_orphan

  let compare = Stdlib.compare

  let pp out = function
    | T_lia -> CCFormat.string out "lia"
    | T_lra -> CCFormat.string out "lra"
    | T_ho -> CCFormat.string out "ho"
    | T_live_cnf -> CCFormat.string out "live_cnf"
    | T_ho_norm -> CCFormat.string out "ho_norm"
    | T_dont_increase_depth -> CCFormat.string out "dont_increase_depth"
    | T_ext -> CCFormat.string out "extensionality"
    | T_ind -> CCFormat.string out "ind"
    | T_data -> CCFormat.string out "data"
    | T_distinct -> CCFormat.string out "distinct_constants"
    | T_ac id -> CCFormat.fprintf out "(ac %a)" Name.pp_full id
    | T_cannot_orphan -> CCFormat.fprintf out "cannot orphan"
end

(* ── ArithOp ── *)

module ArithOp = struct
  exception TypeMismatch of string

  let _ty_mismatch fmt =
    CCFormat.ksprintf ~f:(fun msg -> raise (TypeMismatch msg)) fmt

  let sign b =
    match is_payload b with
    | Some (Int n) -> Z.sign n
    | Some (Rat n) -> Q.sign n
    | _ -> _ty_mismatch "cannot compute sign" pp b

  type arith_view =
    [ `Int of Z.t
    | `Rat of Q.t
    | `Other of t
    ]

  let view b =
    match is_payload b with
    | Some (Int i) -> `Int i
    | Some (Rat n) -> `Rat n
    | _ -> `Other b

  let parse_num s =
    if String.contains s '/' then
      make_payload (Rat (Q.of_string s))
    else
      make_payload (Int (Z.of_string s))

  let one_i = make_payload (Int Z.one)
  let zero_i = make_payload (Int Z.zero)
  let one_rat = make_payload (Rat Q.one)
  let zero_rat = make_payload (Rat Q.zero)

  let zero_of_ty = function
    | `Rat -> zero_rat
    | `Int -> zero_i

  let one_of_ty = function
    | `Rat -> one_rat
    | `Int -> one_i

  let is_zero b =
    match is_payload b with
    | Some (Int n) -> Z.sign n = 0
    | Some (Rat n) -> Q.sign n = 0
    | _ -> _ty_mismatch "not a number" pp b

  let is_one b =
    match is_payload b with
    | Some (Int n) -> Z.equal n Z.one
    | Some (Rat n) -> Q.equal n Q.one
    | _ -> _ty_mismatch "not a number" pp b

  let is_minus_one b =
    match is_payload b with
    | Some (Int n) -> Z.equal n Z.minus_one
    | Some (Rat n) -> Q.equal n Q.minus_one
    | _ -> _ty_mismatch "not a number" pp b

  let floor b =
    match is_payload b with
    | Some (Int _) -> b
    | Some (Rat n) -> make_payload (Int (Q.to_bigint n))
    | _ -> _ty_mismatch "not numeric" pp b

  let ceiling b =
    match is_payload b with
    | Some (Int _) -> b
    | Some (Rat _) -> failwith "Q.ceiling: not implemented"
    | _ -> _ty_mismatch "not numeric" pp b

  let truncate b =
    match is_payload b with
    | Some (Int _) -> b
    | Some (Rat n) when Q.sign n >= 0 -> make_payload (Int (Q.to_bigint n))
    | Some (Rat _) -> failwith "Q.truncate: not implemented"
    | _ -> _ty_mismatch "not numeric" pp b

  let round b =
    match is_payload b with
    | Some (Int _) -> b
    | Some (Rat _) -> failwith "Q.round: not implemented"
    | _ -> _ty_mismatch "not numeric" pp b

  let prec b =
    match is_payload b with
    | Some (Int n) -> make_payload (Int Z.(n - one))
    | Some (Rat n) -> make_payload (Rat Q.(n - one))
    | _ -> _ty_mismatch "not numeric" pp b

  let succ b =
    match is_payload b with
    | Some (Int n) -> make_payload (Int Z.(n + one))
    | Some (Rat n) -> make_payload (Rat Q.(n + one))
    | _ -> _ty_mismatch "not numeric" pp b

  let err2_ b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int _), Some (Rat _) | Some (Rat _), Some (Int _) ->
      _ty_mismatch "incompatible numeric types" pp b1 pp b2
    | _ -> _ty_mismatch "not numeric" pp b1 pp b2

  let sum b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int Z.(n1 + n2))
    | Some (Rat n1), Some (Rat n2) -> make_payload (Rat Q.(n1 + n2))
    | _ -> err2_ b1 b2

  let difference b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int Z.(n1 - n2))
    | Some (Rat n1), Some (Rat n2) -> make_payload (Rat Q.(n1 - n2))
    | _ -> err2_ b1 b2

  let uminus b =
    match is_payload b with
    | Some (Int n) -> make_payload (Int (Z.neg n))
    | Some (Rat n) -> make_payload (Rat (Q.neg n))
    | _ -> _ty_mismatch "not numeric" pp b

  let product b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int Z.(n1 * n2))
    | Some (Rat n1), Some (Rat n2) -> make_payload (Rat Q.(n1 * n2))
    | _ -> err2_ b1 b2

  let quotient b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) ->
      let q, r = Z.div_rem n1 n2 in
      if Z.sign r = 0 then
        make_payload (Int q)
      else
        _ty_mismatch "non-exact integral division" pp b1 pp b2
    | Some (Rat n1), Some (Rat n2) ->
      if Q.sign n2 = 0 then
        raise Division_by_zero
      else
        make_payload (Rat (Q.div n1 n2))
    | _ -> err2_ b1 b2

  let quotient_e b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.div n1 n2))
    | _ ->
      if sign b2 > 0 then
        floor (quotient b1 b2)
      else
        ceiling (quotient b1 b2)

  let quotient_t b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.div n1 n2))
    | _ -> truncate (quotient b1 b2)

  let quotient_f b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.div n1 n2))
    | _ -> floor (quotient b1 b2)

  let remainder_e b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.rem n1 n2))
    | _ -> difference b1 (product (quotient_e b1 b2) b2)

  let remainder_t b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.rem n1 n2))
    | _ -> difference b1 (product (quotient_t b1 b2) b2)

  let remainder_f b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> make_payload (Int (Z.rem n1 n2))
    | _ -> difference b1 (product (quotient_f b1 b2) b2)

  let to_int b =
    match is_payload b with
    | Some (Int _) -> b
    | _ -> floor b

  let to_rat b =
    match is_payload b with
    | Some (Int n) -> make_payload (Rat (Q.of_bigint n))
    | Some (Rat _) -> b
    | _ -> _ty_mismatch "not numeric" pp b

  let abs b =
    match is_payload b with
    | Some (Int n) -> make_payload (Int (Z.abs n))
    | Some (Rat n) -> make_payload (Rat (Q.abs n))
    | _ -> _ty_mismatch "not numeric" pp b

  let divides a b =
    match is_payload a, is_payload b with
    | Some (Rat i), Some (Rat _) -> Q.sign i <> 0
    | Some (Int a), Some (Int b) -> Z.sign a <> 0 && Z.sign (Z.rem b a) = 0
    | _ -> _ty_mismatch "divides" pp a pp b

  let gcd a b =
    match is_payload a, is_payload b with
    | Some (Rat _), Some (Rat _) -> one_rat
    | Some (Int a), Some (Int b) -> make_payload (Int (Z.gcd a b))
    | _ -> _ty_mismatch "gcd" pp a pp b

  let lcm a b =
    match is_payload a, is_payload b with
    | Some (Rat _), Some (Rat _) -> one_rat
    | Some (Int a), Some (Int b) -> make_payload (Int (Z.lcm a b))
    | _ -> _ty_mismatch "lcm" pp a pp b

  let less b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> Z.lt n1 n2
    | Some (Rat n1), Some (Rat n2) -> Q.lt n1 n2
    | _ -> err2_ b1 b2

  let lesseq b1 b2 =
    match is_payload b1, is_payload b2 with
    | Some (Int n1), Some (Int n2) -> Z.leq n1 n2
    | Some (Rat n1), Some (Rat n2) -> Q.leq n1 n2
    | _ -> err2_ b1 b2

  let greater b1 b2 = less b2 b1
  let greatereq b1 b2 = lesseq b2 b1

  let divisors n =
    if Z.leq n Z.zero then
      raise (Invalid_argument "divisors: expected number > 0")
    else (
      match Z.to_int_exn n with
      | n ->
        let l = ref [] in
        for i = 2 to n / 2 do
          if i < n && n mod i = 0 then l := i :: !l
        done;
        List.rev_map Z.of_int !l
      | exception _ -> []
    )
end

(* ── TPTP ── *)
module TPTP = struct
  let to_string b =
    match is_payload b with
    | Some (Int n) -> Z.to_string n
    | Some (Rat q) -> Q.to_string q
    | Some (Real r) -> r
    | Some (Pseudo_de_bruijn i) -> Printf.sprintf "$$db_%d" i
    | None -> Builtin_gen.TPTP.to_string b

  let pp out b = Format.pp_print_string out (to_string b)

  exception NotABuiltin

  let of_string_exn s =
    match Builtin_gen.TPTP.of_string s with
    | Ok v -> make_view v
    | Error _ when String.length s > 0 && s.[0] >= '0' && s.[0] <= '9' ->
      if String.contains s '/' then
        make_payload (Rat (Q.of_string s))
      else
        make_payload (Int (Z.of_string s))
    | Error _ -> raise NotABuiltin

  let of_string s = try Some (of_string_exn s) with NotABuiltin -> None
  let fixity = Builtin_gen.TPTP.fixity
  let is_prefix = Builtin_gen.TPTP.is_prefix
  let is_infix = Builtin_gen.TPTP.is_infix
  let connectives = Set.of_iter (Iter.of_list [ and_; or_; equiv; imply ])

  let is_connective b =
    match view b with
    | Some (And | Or | Equiv | Imply) -> true
    | _ -> false
end

(* ── ZF ── *)
module ZF = struct
  let to_string b =
    match is_payload b with
    | Some (Int n) -> Z.to_string n
    | Some (Rat q) -> Q.to_string q
    | Some (Real r) -> r
    | Some (Pseudo_de_bruijn i) -> Printf.sprintf "<db %d>" i
    | None -> Builtin_gen.ZF.to_string b

  let pp out b = Format.pp_print_string out (to_string b)
end
