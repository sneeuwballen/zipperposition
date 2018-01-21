
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Builtin Objects} *)

module Fmt = CCFormat

type t =
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
  | Multiset  (* type of multisets *)
  | TType (* type of types *)
  | Prop
  | Term
  | ForallConst (** constant for simulating forall *)
  | ExistsConst (** constant for simulating exists *)
  | Grounding (** used for inst-gen *)
  | TyInt
  | TyRat
  | TyReal
  | Int of Z.t
  | Rat of Q.t
  | Real of string
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
  | Box_opaque (** hint not to open this formula *)
  | Pseudo_de_bruijn of int (** magic to embed De Bruijn indices in normal terms *)

type t_ = t

let to_int_ = function
  | Not -> 0
  | And -> 1
  | Or -> 2
  | Imply -> 3
  | Equiv -> 4
  | Xor -> 5
  | Eq -> 6
  | Neq -> 7
  | HasType -> 8
  | False -> 10
  | True -> 11 (* bigger than false *)
  | Arrow -> 12
  | Wildcard -> 13
  | Multiset -> 14
  | TType -> 15
  | Int _ -> 16
  | Rat _ -> 17
  | Prop -> 18
  | Term -> 19
  | TyRat -> 20
  | TyInt -> 21
  | Floor -> 22
  | Ceiling -> 23
  | Truncate -> 24
  | Round -> 25
  | Prec -> 26
  | Succ -> 27
  | Sum -> 28
  | Difference -> 29
  | Uminus -> 30
  | Product -> 31
  | Quotient -> 32
  | Quotient_e -> 33
  | Quotient_t -> 34
  | Quotient_f -> 35
  | Remainder_e -> 36
  | Remainder_t -> 37
  | Remainder_f -> 38
  | Is_int -> 39
  | Is_rat -> 40
  | To_int -> 41
  | To_rat -> 42
  | Less -> 43
  | Lesseq -> 44
  | Greater -> 45
  | Greatereq -> 46
  | ForallConst -> 47
  | ExistsConst -> 48
  | Grounding -> 50
  | Box_opaque -> 60
  | TyReal -> 70
  | Real _ -> 71
  | Pseudo_de_bruijn _ -> 100

let compare a b = match a, b with
  | Int i, Int j -> Z.compare i j
  | Rat i, Rat j -> Q.compare i j
  | _ -> to_int_ a - to_int_ b

let equal a b = compare a b = 0

let hash s = match s with
  | Int i -> Hash.combine2 1 (Z.hash i)
  | Rat r -> Hash.combine2 2 (Hash.string (Q.to_string r))
  | c -> Hash.combine2 3 (Hashtbl.hash c)

module Map = Sequence.Map.Make(struct type t = t_ let compare = compare end)
module Set = Sequence.Set.Make(struct type t = t_ let compare = compare end)
module Tbl = Hashtbl.Make(struct type t = t_ let equal = equal let hash = hash end)

let is_int = function Int _ -> true | _ -> false
let is_rat = function Rat _ -> true | _ -> false
let is_numeric = function Int _ | Rat _ -> true | _ -> false
let is_not_numeric x = not (is_numeric x)

let is_arith = function
  | Int _ | Rat _ | Floor | Ceiling | Truncate | Round | Prec | Succ | Sum
  | Difference | Uminus | Product | Quotient | Quotient_e | Quotient_t
  | Quotient_f | Remainder_e | Remainder_t | Remainder_f | Is_int | Is_rat
  | To_int | To_rat | Less | Lesseq | Greater | Greatereq -> true
  | _ -> false

let to_string s = match s with
  | Int n -> Z.to_string n
  | Rat n -> Q.to_string n
  | Real r -> r
  | Not -> "¬"
  | And -> "∧"
  | Or -> "∨"
  | Imply -> "⇒"
  | Equiv -> "≡"
  | Xor -> "<~>"
  | Eq -> "="
  | Neq -> "≠"
  | HasType -> ":"
  | True -> "true"
  | False -> "false"
  | Arrow -> "→"
  | Wildcard -> "_"
  | Multiset -> "Ms"
  | TType -> "type"
  | Prop -> "prop"
  | Term -> "ι"
  | ForallConst -> "·∀"
  | ExistsConst -> "·∃"
  | Grounding -> "★"
  | TyInt -> "int"
  | TyRat -> "rat"
  | TyReal -> "real"
  | Floor -> "floor"
  | Ceiling -> "ceiling"
  | Truncate -> "truncate"
  | Round -> "round"
  | Prec -> "prec"
  | Succ -> "succ"
  | Sum -> "+"
  | Difference -> "-"
  | Uminus -> "uminus"
  | Product -> "×"
  | Quotient -> "/"
  | Quotient_e -> "quotient_e"
  | Quotient_t -> "quotient_t"
  | Quotient_f -> "quotient_f"
  | Remainder_e -> "remainder_e"
  | Remainder_t -> "remainder_t"
  | Remainder_f -> "remainder_f"
  | Is_int -> "is_int"
  | Is_rat -> "is_rat"
  | To_int -> "to_int"
  | To_rat -> "to_rat"
  | Less -> "<"
  | Lesseq -> "≤"
  | Greater -> ">"
  | Greatereq -> "≥"
  | Box_opaque -> "<box>"
  | Pseudo_de_bruijn i -> Printf.sprintf "db_%d" i

let pp out s = Format.pp_print_string out (to_string s)

type fixity =
  | Infix_binary
  | Infix_nary
  | Prefix

let fixity = function
  | And | Or ->
    Infix_nary
  | Imply | Equiv | Xor | Eq | Neq | HasType
  | Sum | Difference | Product
  | Quotient | Quotient_e | Quotient_f | Quotient_t
  | Remainder_e | Remainder_t | Remainder_f
  | Less | Lesseq | Greater | Greatereq ->
    Infix_binary
  | _ -> Prefix

let is_prefix o = fixity o = Prefix
let is_infix o = match fixity o with Infix_nary | Infix_binary -> true | Prefix -> false

let ty = function
  | Int _ -> `Int
  | Rat _ -> `Rat
  | _ -> `Other

let mk_int s = Int s
let of_int i = Int (Z.of_int i)
let int_of_string s = Int (Z.of_string s)

let mk_rat s = Rat s
let of_rat i j = Rat (Q.of_ints i j)
let rat_of_string s = Rat (Q.of_string s)

let true_ = True
let false_ = False
let wildcard = Wildcard
let and_ = And
let or_ = Or
let imply = Imply
let equiv = Equiv
let xor = Xor
let not_ = Not
let eq = Eq
let neq = Neq
let arrow = Arrow
let has_type = HasType
let tType = TType
let multiset = Multiset
let prop = Prop
let term = Term
let ty_int = TyInt
let ty_rat = TyRat
let grounding = Grounding

module Tag = struct
  type t =
    | T_lia (** integer arith *)
    | T_lra (** rational arith *)
    | T_ho (** higher order *)
    | T_ext (** extensionality *)
    | T_ind (** induction *)
    | T_data (** datatypes *)
    | T_distinct (** distinct constants *)
    | T_ac of ID.t (** AC symbols *)

  let compare = Pervasives.compare

  let pp out = function
    | T_lia -> Fmt.string out "lia"
    | T_lra -> Fmt.string out "lra"
    | T_ho -> Fmt.string out "ho"
    | T_ext -> Fmt.string out "extensionality"
    | T_ind -> Fmt.string out "ind"
    | T_data -> Fmt.string out "data"
    | T_distinct -> Fmt.string out "distinct_constants"
    | T_ac id -> Fmt.fprintf out "(ac %a)" ID.pp_full id
end


module Arith = struct
  let floor = Floor
  let ceiling = Ceiling
  let truncate = Truncate
  let round = Round
  let prec = Prec
  let succ = Succ
  let sum = Sum
  let difference = Difference
  let uminus = Uminus
  let product = Product
  let quotient = Quotient
  let quotient_e = Quotient_e
  let quotient_t = Quotient_t
  let quotient_f = Quotient_f
  let remainder_e = Remainder_e
  let remainder_t = Remainder_t
  let remainder_f = Remainder_f
  let is_int = Is_int
  let is_rat = Is_rat
  let to_int = To_int
  let to_rat = To_rat
  let less = Less
  let lesseq = Lesseq
  let greater = Greater
  let greatereq = Greatereq
end

module TPTP = struct
  let to_string = function
    | Eq -> "="
    | Neq -> "!="
    | And -> "&"
    | Or -> "|"
    | Not -> "~"
    | Imply -> "=>"
    | Equiv -> "<=>"
    | Xor -> "<~>"
    | HasType -> ":"
    | True -> "$true"
    | False -> "$false"
    | Arrow -> ">"
    | Wildcard -> "$_"
    | TType -> "$tType"
    | Term -> "$i"
    | Prop -> "$o"
    | Multiset -> failwith "cannot print this symbol in TPTP"
    | ForallConst -> "!!"
    | ExistsConst -> "??"
    | Grounding -> "$$ground"
    | TyInt -> "$int"
    | TyRat -> "$rat"
    | TyReal -> "$real"
    | Int x -> Z.to_string x
    | Rat x -> Q.to_string x
    | Real r -> r
    | Floor -> "$floor"
    | Ceiling -> "$ceiling"
    | Truncate -> "$truncate"
    | Round -> "$round"
    | Prec -> "$prec"
    | Succ -> "$succ"
    | Sum -> "$sum"
    | Difference -> "$diff"
    | Uminus -> "$uminus"
    | Product -> "$product"
    | Quotient -> "$quotient"
    | Quotient_e -> "$quotient_e"
    | Quotient_t -> "$quotient_t"
    | Quotient_f -> "$quotient_f"
    | Remainder_e -> "$remainder_e"
    | Remainder_t -> "$remainder_t"
    | Remainder_f -> "$remainder_f"
    | Is_int -> "$is_int"
    | Is_rat -> "$is_rat"
    | To_int -> "$to_int"
    | To_rat -> "$to_rat"
    | Less -> "$less"
    | Lesseq -> "$lesseq"
    | Greater -> "$greater"
    | Greatereq -> "$greatereq"
    | Box_opaque -> "$$box"
    | Pseudo_de_bruijn i -> Printf.sprintf "$$db_%d" i

  let pp out b = CCFormat.string out (to_string b)

  exception NotABuiltin

  let of_string_exn = function
    | "$true" -> True
    | "$false" -> False
    | "$_" -> Wildcard
    | "$tType" -> TType
    | "$i" -> Term
    | "$o" -> Prop
    | "!!" -> ForallConst
    | "??" -> ExistsConst
    | "$int" -> TyInt
    | "$rat" -> TyRat
    | "$floor" -> Floor
    | "$ceiling" -> Ceiling
    | "$truncate" -> Truncate
    | "$round" -> Round
    | "$prec" -> Prec
    | "$succ" -> Succ
    | "$sum" -> Sum
    | "$difference" -> Difference
    | "$uminus" -> Uminus
    | "$product" -> Product
    | "$quotient" -> Quotient
    | "$quotient_e" -> Quotient_e
    | "$quotient_t" -> Quotient_t
    | "$quotient_f" -> Quotient_f
    | "$remainder_e" -> Remainder_e
    | "$remainder_t" -> Remainder_t
    | "$remainder_f" -> Remainder_f
    | "$is_int" -> Is_int
    | "$is_rat" -> Is_rat
    | "$to_int" -> To_int
    | "$to_rat" -> To_rat
    | "$less" -> Less
    | "$lesseq" -> Lesseq
    | "$greater" -> Greater
    | "$greatereq" -> Greatereq
    | _ -> raise NotABuiltin

  let fixity = function
    | And | Or ->
      Infix_nary
    | Imply | Equiv | Xor | Eq | Neq | HasType ->
      Infix_binary
    | _ -> Prefix

  let is_prefix o = fixity o = Prefix
  let is_infix o = match fixity o with Infix_nary | Infix_binary -> true | Prefix -> false

  let of_string b =
    try Some (of_string_exn b)
    with NotABuiltin -> None

  (* TODO add the other ones *)
  let connectives = Set.of_seq
      (Sequence.of_list [ and_; or_; equiv; imply; ])

  let is_connective = function
    | Int _
    | Rat _ -> false
    | _ -> true
end

module ArithOp = struct
  exception TypeMismatch of string
  (** This exception is raised when Arith functions are called
      on non-numeric values (Cst). *)

  (* helper to raise errors *)
  let _ty_mismatch fmt =
    CCFormat.ksprintf ~f:(fun msg -> raise (TypeMismatch msg)) fmt

  let sign = function
    | Int n -> Z.sign n
    | Rat n -> Q.sign n
    | s -> _ty_mismatch "cannot compute sign of symbol %a" pp s

  type arith_view =
    [ `Int of Z.t
    | `Rat of Q.t
    | `Other of t
    ]

  let view = function
    | Int i -> `Int i
    | Rat n -> `Rat n
    | s -> `Other s

  let parse_num s =
    if String.contains s '/'
    then mk_rat (Q.of_string s)
    else mk_int (Z.of_string s)

  let one_i = mk_int Z.one
  let zero_i = mk_int Z.zero
  let one_rat = mk_rat Q.one
  let zero_rat = mk_rat Q.zero

  let zero_of_ty = function
    | `Rat -> zero_rat
    | `Int -> zero_i

  let one_of_ty = function
    | `Rat -> one_rat
    | `Int -> one_i

  let is_zero = function
    | Int n -> Z.sign n = 0
    | Rat n -> Q.sign n = 0
    | s -> _ty_mismatch "not a number: %a" pp s

  let is_one = function
    | Int n -> Z.equal n Z.one
    | Rat n -> Q.equal n Q.one
    | s -> _ty_mismatch "not a number: %a" pp s

  let is_minus_one = function
    | Int n -> Z.equal n Z.minus_one
    | Rat n -> Q.equal n Q.minus_one
    | s -> _ty_mismatch "not a number: %a" pp s

  let floor s = match s with
    | Int _ -> s
    | Rat n -> mk_int (Q.to_bigint n)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let ceiling s = match s with
    | Int _ -> s
    | Rat _ -> failwith "Q.ceiling: not implemented" (* TODO *)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let truncate s = match s with
    | Int _ -> s
    | Rat n when Q.sign n >= 0 -> mk_int (Q.to_bigint n)
    | Rat _ -> failwith "Q.truncate: not implemented" (* TODO *)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let round s = match s with
    | Int _ -> s
    | Rat _ -> failwith "Q.round: not implemented" (* TODO *)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let prec s = match s with
    | Int n -> mk_int Z.(n - one)
    | Rat n -> mk_rat Q.(n - one)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let succ s = match s with
    | Int n -> mk_int Z.(n + one)
    | Rat n -> mk_rat Q.(n + one)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let err2_ s1 s2 = match s1, s2 with
    | Int _, Rat _
    | Rat _, Int _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2
    | _ -> _ty_mismatch "not numeric constants: %a, %a" pp s1 pp s2

  let sum s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int Z.(n1 + n2)
    | Rat n1, Rat n2 -> mk_rat Q.(n1 + n2)
    | _ -> err2_ s1 s2

  let difference s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int Z.(n1 - n2)
    | Rat n1, Rat n2 -> mk_rat Q.(n1 - n2)
    | _ -> err2_ s1 s2

  let uminus s = match s with
    | Int n -> mk_int (Z.neg n)
    | Rat n -> mk_rat (Q.neg n)
    | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let product s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int Z.(n1 * n2)
    | Rat n1, Rat n2 -> mk_rat Q.(n1 * n2)
    | _ -> err2_ s1 s2

  let quotient s1 s2 = match s1, s2 with
    | Int n1, Int n2 ->
      let q, r = Z.div_rem n1 n2 in
      if Z.sign r = 0
      then mk_int q
      else _ty_mismatch "non-exact integral division: %a / %a" pp s1 pp s2
    | Rat n1, Rat n2 ->
      if Q.sign n2 = 0
      then raise Division_by_zero
      else mk_rat (Q.div n1 n2)
    | _ -> err2_ s1 s2

  let quotient_e s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.div n1 n2)
    | _ ->
      if sign s2 > 0
      then floor (quotient s1 s2)
      else ceiling (quotient s1 s2)

  let quotient_t s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.div n1 n2)
    | _ -> truncate (quotient s1 s2)

  let quotient_f s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.div n1 n2)
    | _ -> floor (quotient s1 s2)

  let remainder_e s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.rem n1 n2)
    | _ -> difference s1 (product (quotient_e s1 s2) s2)

  let remainder_t s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.rem n1 n2)
    | _ -> difference s1 (product (quotient_t s1 s2) s2)

  let remainder_f s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_int (Z.rem n1 n2)
    | _ -> difference s1 (product (quotient_f s1 s2) s2)

  let to_int s = match s with
    | Int _ -> s
    | _ -> floor s

  let to_rat s = match s with
    | Int n -> mk_rat (Q.of_bigint n)
    | Rat _ -> s
    | _ -> _ty_mismatch "not a numeric constant: %a" pp s

  let abs s = match s with
    | Int n -> mk_int (Z.abs n)
    | Rat n -> mk_rat (Q.abs n)
    | _ -> _ty_mismatch "not a numeric constant: %a" pp s

  let divides a b = match a, b with
    | Rat i, Rat _ -> Q.sign i <> 0
    | Int a, Int b ->
      Z.sign a <> 0 &&
      Z.sign (Z.rem b a) = 0
    | _ -> _ty_mismatch "divides: expected two numerical types"

  let gcd a b = match a, b with
    | Rat _, Rat _ -> one_rat
    | Int a, Int b -> mk_int (Z.gcd a b)
    | _ -> _ty_mismatch "gcd: expected two numerical types"

  let lcm a b = match a, b with
    | Rat _, Rat _ -> one_rat
    | Int a, Int b -> mk_int (Z.lcm a b)
    | _ -> _ty_mismatch "gcd: expected two numerical types"

  let less s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Z.lt n1 n2
    | Rat n1, Rat n2 -> Q.lt n1 n2
    | _ -> err2_ s1 s2

  let lesseq s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Z.leq n1 n2
    | Rat n1, Rat n2 -> Q.leq n1 n2
    | _ -> err2_ s1 s2

  let greater s1 s2 = less s2 s1

  let greatereq s1 s2 = lesseq s2 s1

  (* factorize [n] into a product of prime numbers. [n] must be positive *)
  let divisors n =
    if (Z.leq n Z.zero)
    then raise (Invalid_argument "prime_factors: expected number > 0")
    else try
        let n = Z.to_int n in
        let l = ref [] in
        for i = 2 to n/2 do
          if i < n && n mod i = 0 then l := i :: !l
        done;
        List.rev_map Z.of_int !l
      with Z.Overflow -> []  (* too big *)
end

module ZF = struct
  let to_string = function
    | Eq -> "="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
    | Not -> "~"
    | Imply -> "=>"
    | Equiv -> "<=>"
    | HasType -> ":"
    | True -> "true"
    | False -> "false"
    | Arrow -> ">"
    | Wildcard -> "_"
    | TType -> "type"
    | Prop -> "prop"
    | Term -> "term" (* XXX needs to be declared! *)
    | Xor
    | Multiset -> failwith "cannot print this symbol in ZF"
    | ForallConst -> "!!"
    | ExistsConst -> "??"
    | Grounding -> "$$grounding"
    | TyInt -> "int"
    | TyRat -> "rat"
    | TyReal -> "real"
    | Int x -> Z.to_string x
    | Rat x -> Q.to_string x
    | Real x -> x
    (* FIXME: update *)
    | Floor -> "$floor"
    | Ceiling -> "$ceiling"
    | Truncate -> "$truncate"
    | Round -> "$round"
    | Prec -> "$prec"
    | Succ -> "$succ"
    | Sum -> "+"
    | Difference -> "-"
    | Uminus -> "-"
    | Product -> "*"
    | Quotient -> "$quotient"
    | Quotient_e -> "/"
    | Quotient_t -> "$quotient_t"
    | Quotient_f -> "$quotient_f"
    | Remainder_e -> "mod"
    | Remainder_t -> "$remainder_t"
    | Remainder_f -> "$remainder_f"
    | Is_int -> "$is_int"
    | Is_rat -> "$is_rat"
    | To_int -> "$to_int"
    | To_rat -> "$to_rat"
    | Less -> "<"
    | Lesseq -> "<="
    | Greater -> ">"
    | Greatereq -> ">="
    | Box_opaque -> "<box>"
    | Pseudo_de_bruijn i -> Printf.sprintf "<db %d>" i

  let pp out b = CCFormat.string out (to_string b)
end
