(*
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

(** {1 Symbols} *)

type connective =
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
  | Exists
  | Forall
  | ForallTy
  | Lambda
  | Arrow
  | Wildcard
  | Multiset
  | FreshVar of int
  | TType

type const_symbol = {
  mutable cs_id : int;
  cs_name : string;
}

type t =
  | Conn of connective
  | Cst of const_symbol
  | Int of Z.t
  | Rat of Q.t

type sym = t

let __to_int = function
  | Conn _ -> 0
  | Int _ -> 1
  | Rat _ -> 2
  | Cst _ -> 3

let cmp a b = match a, b with
  | Cst s1, Cst s2 -> s1.cs_id - s2.cs_id
  | Int i, Int j -> Z.compare i j
  | Rat i, Rat j -> Q.compare i j
  | Conn c1, Conn c2 -> Pervasives.compare c1 c2
  | _ -> __to_int a - __to_int b

let eq a b = cmp a b = 0

let hash_fun s h = match s with
  | Cst s -> Hash.int_ s.cs_id h
  | Int i -> Hash.int_ (Z.hash i) h
  | c -> Hash.int_ (Hashtbl.hash c) h
let hash s = Hash.apply hash_fun s

module Map = Sequence.Map.Make(struct type t = sym let compare = cmp end)
module Set = Sequence.Set.Make(struct type t = sym let compare = cmp end)
module Tbl = Hashtbl.Make(struct type t = sym let equal = eq let hash = hash end)

let is_const = function | Cst _ -> true | _ -> false
let is_int = function | Int _ -> true | _ -> false
let is_rat = function | Rat _ -> true | _ -> false
let is_numeric = function | Int _ | Rat _ | _ -> false

let is_distinct s = match s with
  | Cst c ->
    let s = c.cs_name in
    s <> "" &&  s.[0] = '"' && s.[String.length s - 1] = '"'
  | _ -> false

module Seq = struct
  let add_set set =
    Sequence.fold (fun set s -> Set.add s set) set
end

let to_string s = match s with
  | Cst c -> c.cs_name
  | Int n -> Z.to_string n
  | Rat n -> Q.to_string n
  | Conn o ->
      begin match o with
      | Not -> "¬"
      | And -> "∧" 
      | Or -> "∨" 
      | Imply -> "→"
      | Equiv -> "<=>"
      | Xor -> "<~>"
      | Eq -> "="
      | Neq -> "≠"
      | HasType -> ":"
      | True -> "true"
      | False -> "false"
      | Exists -> "∃"
      | Forall -> "∀"
      | Lambda -> "λ"
      | ForallTy -> "Λ"
      | Arrow -> "->"
      | Wildcard -> "_"
      | Multiset -> "Ms"
      | FreshVar i -> "ν"^string_of_int i
      | TType -> "TType"
      end

let pp buf s = Buffer.add_string buf (to_string s)

let fmt fmt s = Format.pp_print_string fmt (to_string s)

let ty = function
  | Int _ -> `Int
  | Rat _ -> `Rat
  | _ -> `Other

(** {2 Cstructors} *)
module H = Hashcons.Make(struct
  type t = const_symbol
  let equal x y = x.cs_name = y.cs_name
  let hash c = Hash.(apply string_ c.cs_name)
  let tag i c = c.cs_id <- i
end)

let of_string name=
  let cs = {cs_name=name; cs_id= ~-1;} in
  let cs = H.hashcons cs in
  Cst cs

let mk_const = of_string

let mk_int s = Int s
let of_int i = Int (Z.of_int i)
let int_of_string s = Int (Z.of_string s)

let mk_rat s = Rat s
let of_rat i j = Rat (Q.of_ints i j)
let rat_of_string s = Rat (Q.of_string s)

module Base = struct
  let true_ = Conn True
  let false_ = Conn False
  let wildcard = Conn Wildcard
  let and_ = Conn And
  let or_ = Conn Or
  let imply = Conn Imply
  let equiv = Conn Equiv
  let xor = Conn Xor
  let not_ = Conn Not
  let eq = Conn Eq
  let neq = Conn Neq
  let forall = Conn Forall
  let exists = Conn Exists
  let lambda = Conn Lambda
  let forall_ty = Conn ForallTy
  let arrow = Conn Arrow
  let tType = Conn TType
  let multiset = Conn Multiset

  (* generate fresh symbols *)
  let fresh_var =
    let r = ref 0 in
    fun () ->
      let n = !r in incr r; Conn (FreshVar n)
end

(* TODO
  let __printers = Hashtbl.create 5

  let add_printer name pp =
    if Hashtbl.mem __printers name then failwith ("printer " ^ name ^ " already present");
    Hashtbl.add __printers name pp

  let _pp buf s = match s with
    | HCst (s, _) -> Buffer.add_string buf s
    | HInt n -> Buffer.add_string buf (Big_int.string_of_big_int n)
    | HRat n -> Buffer.add_string buf (Ratio.string_of_ratio n)
    | HReal f -> Buffer.add_string buf (string_of_float f)

  let () =
    add_printer "debug" _pp

  let __default_pp = ref _pp

  let pp buf s = !__default_pp buf s
  let to_string = Util.on_buffer pp
*)

(** {2 Generation of symbols} *)

let gensym =
  let n = ref 0 in
  fun ?(prefix="_logtk") () ->
    let n' = !n in
    incr n;
    mk_const (prefix ^ string_of_int n')

module TPTP = struct
  let pp buf = function
    | Cst s -> Buffer.add_string buf s.cs_name
    | Int i -> Buffer.add_string buf (Z.to_string i)
    | Rat n -> Buffer.add_string buf (Q.to_string n)
    | Conn o ->
        Buffer.add_string buf (match o with
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
          | Exists -> "?"
          | Forall -> "!"
          | ForallTy -> "!>"
          | Lambda -> "^"
          | Arrow -> ">"
          | Wildcard -> "$_"
          | TType -> "$tType"
          | Multiset
          | FreshVar _ -> failwith "cannot print this symbol in TPTP"
      )

  let to_string = Util.on_buffer pp
  let fmt fmt s = Format.pp_print_string fmt (to_string s)

  let i = mk_const "$i"
  let o = mk_const "$o"
  let int = mk_const "$int"
  let rat = mk_const "$rat"
  let real = mk_const "$real"

  (* TODO add the other ones *)
  let connectives = Set.of_seq
  (Sequence.of_list [ Base.and_; Base.or_; Base.equiv; Base.imply; ])

  let is_connective = function
    | Conn _ -> true
    | Cst _
    | Int _
    | Rat _ -> false

  (** {3 Arith} *)

  module Arith = struct

    let floor = mk_const "$floor"
    let ceiling = mk_const "$ceiling"
    let truncate = mk_const "$truncate"
    let round = mk_const "$round"

    let prec = mk_const "$prec"
    let succ = mk_const "$succ"

    let sum = mk_const  "$sum"
    let difference = mk_const   "$difference"
    let uminus = mk_const   "$uminus"
    let product = mk_const  "$product"
    let quotient = mk_const   "$quotient"

    let quotient_e = mk_const   "$quotient_e"
    let quotient_t = mk_const   "$quotient_t"
    let quotient_f = mk_const   "$quotient_f"
    let remainder_e = mk_const   "$remainder_e"
    let remainder_t = mk_const   "$remainder_t"
    let remainder_f = mk_const   "$remainder_f"

    let is_int = mk_const   "$is_int"
    let is_rat = mk_const   "$is_rat"

    let to_int = mk_const   "$to_int"
    let to_rat = mk_const   "$to_rat"

    let less = mk_const   "$less"
    let lesseq = mk_const   "$lesseq"
    let greater = mk_const   "$greater"
    let greatereq = mk_const   "$greatereq"

    let set =
      let l = [
        sum; difference; uminus; product; quotient;
        quotient_e; quotient_t; quotient_f;
        remainder_e; remainder_t; remainder_f;
        less; lesseq; greater; greatereq;
      ] in
      Set.of_seq (Sequence.of_list l)

    let symbols = Set.to_seq set

    let is_arith s = Set.mem s set
  end
end

module ArithOp = struct
  exception TypeMismatch of string
    (** This exception is raised when Arith functions are called
        on non-numeric values (Cst). *)

  (* helper to raise errors *)
  let _ty_mismatch fmt =
    let buf = Buffer.create 32 in
    Printf.kbprintf
      (fun _ -> raise (TypeMismatch (Buffer.contents buf)))
      buf
      fmt

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
  | Rat n -> failwith "Q.ceiling: not implemented" (* TODO *)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let truncate s = match s with
  | Int _ -> s
  | Rat n when Q.sign n >= 0 -> mk_int (Q.to_bigint n)
  | Rat n -> failwith "Q.truncate: not implemented" (* TODO *)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let round s = match s with
  | Int _ -> s
  | Rat n -> failwith "Q.round: not implemented" (* TODO *)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let prec s = match s with
  | Int n -> mk_int Z.(n - one)
  | Rat n -> mk_rat Q.(n - one)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let succ s = match s with
  | Int n -> mk_int Z.(n + one)
  | Rat n -> mk_rat Q.(n + one)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let sum s1 s2 = match s1, s2 with
  | Int n1, Int n2 -> mk_int Z.(n1 + n2)
  | Rat n1, Rat n2 -> mk_rat Q.(n1 + n2)
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

  let difference s1 s2 = match s1, s2 with
  | Int n1, Int n2 -> mk_int Z.(n1 - n2)
  | Rat n1, Rat n2 -> mk_rat Q.(n1 - n2)
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

  let uminus s = match s with
  | Int n -> mk_int (Z.neg n)
  | Rat n -> mk_rat (Q.neg n)
  | s -> _ty_mismatch "not a numeric constant: %a" pp s

  let product s1 s2 = match s1, s2 with
  | Int n1, Int n2 -> mk_int Z.(n1 * n2)
  | Rat n1, Rat n2 -> mk_rat Q.(n1 * n2)
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

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
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

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
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

  let lesseq s1 s2 = match s1, s2 with
  | Int n1, Int n2 -> Z.leq n1 n2
  | Rat n1, Rat n2 -> Q.leq n1 n2
  | (Cst _ | Conn _), _ -> _ty_mismatch "not a numeric constant: %a" pp s1
  | _, (Cst _ | Conn _) -> _ty_mismatch "not a numeric constant: %a" pp s2
  | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

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
