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

(** A symbol of TPTP *)
type t =
  | Const of string * const_info
  | Int of Big_int.big_int
  | Rat of Ratio.ratio
  | Real of float
and const_info = {
  mutable tag : int;
  mutable attrs : int;
} (** Additional information for hashconsed symbols *)

type symbol = t

(** {2 Boolean attributes} *)

type symbol_attribute = int

let attr_skolem = 1 lsl 0
let attr_split = 1 lsl 1
let attr_binder = 1 lsl 2
let attr_infix = 1 lsl 3
let attr_ac = 1 lsl 4
let attr_multiset = 1 lsl 5
let attr_fresh_const = 1 lsl 6
let attr_commut = 1 lsl 7
let attr_distinct = 1 lsl 8

let attrs s = match s with
  | Const (_, info) -> info.attrs
  | Int _
  | Rat _
  | Real _ -> 0

let has_attr attr s = (attrs s land attr) <> 0

(** Hashconsing *)

let eq_base s1 s2 = match s1, s2 with
  | Const (s1,_), Const (s2, _) -> s1 = s2
  | Int n1, Int n2 -> Big_int.eq_big_int n1 n2
  | Rat n1, Rat n2 -> Ratio.eq_ratio n1 n2
  | Real f1, Real f2 -> f1 = f2
  | _ -> false

let hash_base s = match s with
  | Const (s, _) -> Hash.hash_string s
  | Int n -> Hashtbl.hash n
  | Rat n -> Hashtbl.hash n
  | Real f -> Hashtbl.hash f

module H = Hashcons.Make(struct
  type t = symbol
  let equal s1 s2 = eq_base s1 s2
  let hash s = hash_base s
  let tag i s = match s with
    | Const (s, info) -> info.tag <- i
    | _ -> ()
end)

let is_used s =
  H.mem (Const (s, { tag= ~-1; attrs=0; }))

(** {2 Basic operations} *)

let compare s1 s2 =
  let __to_int = function
    | Const _ -> 0
    | Int _ -> 1
    | Rat _ -> 2
    | Real _ -> 3
  in
  match s1, s2 with
  | Const (_, info1), Const (_, info2) -> info1.tag - info2.tag
  | Int n1, Int n2 -> Big_int.compare_big_int n1 n2
  | Rat n1, Rat n2 -> Ratio.compare_ratio n1 n2
  | Real f1, Real f2 -> Pervasives.compare f1 f2
  | _, _ -> __to_int s1 - __to_int s2

let eq a b = compare a b = 0

let hash s = match s with
  | Const (_, info) -> info.tag
  | Int n -> Hashtbl.hash n
  | Rat n -> Hashtbl.hash n
  | Real f -> int_of_float f

let mk_const ?(attrs=0) s =
  let symb = Const (s, { tag= ~-1; attrs; }) in
  H.hashcons symb

let mk_distinct ?(attrs=0) s =
  let attrs = attrs lor attr_distinct in
  mk_const ~attrs s

let mk_bigint i = Int i

let mk_int i = Int (Big_int.big_int_of_int i)

let mk_ratio rat = Rat rat

let mk_rat i j =
  Rat (Ratio.create_ratio (Big_int.big_int_of_int i) (Big_int.big_int_of_int j))

let mk_real f = Real f

let parse_num str =
  let n = Num.num_of_string str in
  if Num.is_integer_num n
    then Int (Num.big_int_of_num n)
    else Rat (Num.ratio_of_num n)

let is_const s = match s with
  | Const _ -> true | _ -> false

let is_int s = match s with
  | Int _ -> true | _ -> false

let is_rat s = match s with
  | Rat _ -> true | _ -> false

let is_real s = match s with
  | Real f -> true | _ -> false

let is_numeric s = match s with
  | Real _ | Int _ | Rat _ -> true | _ -> false

let is_distinct s = match s with
  | Const _ -> has_attr attr_distinct s | _ -> false

module SHashtbl = Hashtbl.Make(struct
  type t = symbol
  let equal = eq
  let hash = hash
end)

module SMap = Sequence.Map.Make(struct type t = symbol let compare = compare end)

module SSet = Sequence.Set.Make(struct type t = symbol let compare = compare end)

(** {2 connectives} *)

let true_symbol = mk_const "$true"
let false_symbol = mk_const "$false"
let eq_symbol = mk_const ~attrs:(attr_infix lor attr_multiset lor
                                  attr_commut) "="
let exists_symbol = mk_const ~attrs:attr_binder "?"
let forall_symbol = mk_const ~attrs:attr_binder "!"
let lambda_symbol = mk_const ~attrs:attr_binder "^"
let not_symbol = mk_const "~"
let imply_symbol = mk_const ~attrs:attr_infix "=>"
let equiv_symbol = mk_const ~attrs:(attr_infix lor attr_commut) "<=>"
let and_symbol = mk_const ~attrs:(attr_infix lor attr_ac lor attr_multiset) "&"
let or_symbol = mk_const ~attrs:(attr_infix lor attr_ac lor attr_multiset) "|"

let connectives =
  [ true_symbol
  ; false_symbol
  ; eq_symbol
  ; equiv_symbol
  ; exists_symbol
  ; forall_symbol
  ; lambda_symbol
  ; not_symbol
  ; imply_symbol
  ; and_symbol
  ; or_symbol
  ]

let is_connective s = List.exists (fun s' -> eq s s') connectives

(** {2 Arith} *)

module Arith = struct
  exception TypeMismatch

  let sign s = match s with
  | Int n -> Big_int.sign_big_int n
  | Rat n -> Ratio.sign_ratio n
  | Real f when f > 0. -> 1
  | Real f when f < 0. -> -1
  | Real f -> 0
  | _ -> raise TypeMismatch

  let floor = mk_const "$floor"
  let ceiling = mk_const "$ceiling"
  let truncate = mk_const "$truncate"
  let round = mk_const "$round"

  let prec = mk_const "$prec"
  let succ = mk_const "$succ"

  let one_i = mk_int 1
  let zero_i = mk_int 0
  let one_rat = mk_rat 1 1
  let zero_rat = mk_rat 0 1
  let one_f = mk_real 1.
  let zero_f = mk_real 0.

  let is_zero s = match s with
  | Int n -> Big_int.sign_big_int n = 0
  | Rat n -> Ratio.sign_ratio n = 0
  | Real f -> f = 0.
  | _ -> raise TypeMismatch

  let sum = mk_const "$sum"
  let difference = mk_const "$difference"
  let uminus = mk_const "$uminus"
  let product = mk_const "$product"
  let quotient = mk_const "$quotient"

  let quotient_e = mk_const "$quotient_e"
  let quotient_t = mk_const "$quotient_t"
  let quotient_f = mk_const "$quotient_f"
  let remainder_e = mk_const "$remainder_e"
  let remainder_t = mk_const "$remainder_t"
  let remainder_f = mk_const "$remainder_f"

  let to_int = mk_const "$to_int"
  let to_rat = mk_const "$to_rat"
  let to_real = mk_const "$to_real"

  let less = mk_const "$less"
  let lesseq = mk_const "$lesseq"
  let greater = mk_const "$greater"
  let greatereq = mk_const "$greatereq"

  let set =
    let l = [
      sum; difference; uminus; product; quotient;
      quotient_e; quotient_t; quotient_f;
      remainder_e; remainder_t; remainder_f;
      less; lesseq; greater; greatereq;
    ] in
    SSet.of_seq (Sequence.of_list l)

  let is_arith s = SSet.mem s set

  module Op = struct
    let floor s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.floor_ratio n)
    | Real f -> mk_real (Pervasives.floor f)
    | _ -> raise TypeMismatch

    let ceiling s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.ceiling_ratio n)
    | Real f -> mk_real (Pervasives.ceil f)
    | _ -> raise TypeMismatch

    let truncate s = match s with
    | Int _ -> s
    | Rat n when Ratio.sign_ratio n >= 0 -> mk_bigint (Ratio.floor_ratio n)
    | Rat n -> mk_bigint (Big_int.minus_big_int (Ratio.floor_ratio (Ratio.abs_ratio n)))
    | Real f -> mk_int (Pervasives.truncate f)
    | _ -> raise TypeMismatch

    let round s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.round_ratio n)
    | Real f ->
      let f' = Pervasives.floor f in
      let i = if f -. f' > 0.5 then int_of_float f' else (int_of_float f') + 1 in
      mk_int i
    | _ -> raise TypeMismatch

    let prec s = match s with
    | Int n -> mk_bigint (Big_int.pred_big_int n)
    | Rat n -> mk_ratio (Ratio.add_int_ratio (-1) n)
    | Real f -> mk_real (f -. 1.)
    | _ -> raise TypeMismatch

    let succ s = match s with
    | Int n -> mk_bigint (Big_int.succ_big_int n)
    | Rat n -> mk_ratio (Ratio.add_int_ratio 1 n)
    | Real f -> mk_real (f +. 1.)
    | _ -> raise TypeMismatch

    let sum s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.add_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.add_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 +. f2)
    | _ -> raise TypeMismatch

    let difference s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.sub_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.sub_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 -. f2)
    | _ -> raise TypeMismatch

    let uminus s = match s with
    | Int n -> mk_bigint (Big_int.minus_big_int n)
    | Rat n -> mk_ratio (Ratio.minus_ratio n)
    | Real f -> mk_real (~-. f)
    | _ -> raise TypeMismatch

    let product s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.mult_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.mult_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 *. f2)
    | _ -> raise TypeMismatch

    let quotient s1 s2 = match s1, s2 with
    | Int _, Int _ -> raise TypeMismatch
    | Rat n1, Rat n2 ->
      (try mk_ratio (Ratio.div_ratio n1 n2) with Failure _ -> raise Division_by_zero)
    | Real f1, Real f2 ->
      let f = f1 /. f2 in if f == infinity then raise Division_by_zero else mk_real f
    | _ -> raise TypeMismatch

    let quotient_e s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (fst (Big_int.quomod_big_int n1 n2))
    | _ ->
      if sign s2 > 0
        then floor (quotient s1 s2)
        else ceiling (quotient s1 s2)

    let quotient_t s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (fst (Big_int.quomod_big_int n1 n2))
    | _ -> truncate (quotient s1 s2)

    let quotient_f s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (fst (Big_int.quomod_big_int n1 n2))
    | _ -> floor (quotient s1 s2)

    let remainder_e s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (fst (Big_int.quomod_big_int n1 n2))
    | _ -> difference s1 (product (quotient_e s1 s2) s2)

    let remainder_t s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (snd (Big_int.quomod_big_int n1 n2))
    | _ -> difference s1 (product (quotient_t s1 s2) s2)

    let remainder_f s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (snd (Big_int.quomod_big_int n1 n2))
    | _ -> difference s1 (product (quotient_f s1 s2) s2)

    let to_int s = match s with
    | Int _ -> s
    | _ -> floor s

    let to_rat s = match s with
    | Int n -> mk_ratio (Ratio.ratio_of_big_int n)
    | Rat _ -> s
    | _ -> raise TypeMismatch (* XXX not fully specified... *)

    let to_real s = match s with
    | Int n -> mk_real (Big_int.float_of_big_int n)
    | Rat n -> mk_real (Ratio.float_of_ratio n)
    | Real _ -> s
    | _ -> raise TypeMismatch

    let less s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.lt_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.lt_ratio n1 n2
    | Real f1, Real f2 -> f1 < f2
    | _ -> raise TypeMismatch

    let lesseq s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.le_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.le_ratio n1 n2
    | Real f1, Real f2 -> f1 <= f2
    | _ -> raise TypeMismatch

    let greater s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.gt_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.gt_ratio n1 n2
    | Real f1, Real f2 -> f1 > f2
    | _ -> raise TypeMismatch

    let greatereq s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.ge_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.ge_ratio n1 n2
    | Real f1, Real f2 -> f1 >= f2
    | _ -> raise TypeMismatch
  end
end

(** {2 "Magic" symbols} *)

(** pseudo symbol kept for locating bound vars in precedence. Bound
    vars are grouped in the precedence together w.r.t other symbols,
    but compare to each other by their index. *)
let db_symbol = mk_const "$$db_magic_cookie"

(** pseudo symbol for locating split symbols in precedence. Split
    symbols compare lexicographically with other split symbols,
    but are in a fixed location in precedence w.r.t other kinds of
    symbols. *)
let split_symbol = mk_const "$$split_magic_cookie"

(** pseudo symbol for locating magic constants in precedence.
    This is useful for keeping the precedence finite while managing
    an infinite set of fresh constants, that are used for
    testing terms for ground joinability (replacing variables
    with such constants) *)
let const_symbol = mk_const "$$const_magic_cookie"

(** pseudo symbol to locate numbers in the precedence *)
let num_symbol = mk_const "$$num_magic_cookie"

(** Infinite set of symbols, accessed by index, that will not collide with
    the signature of the problem *)
let mk_fresh_const i =
  mk_const ~attrs:attr_fresh_const ("$$c_" ^ string_of_int i)

(** {2 IO} *)

let to_string s = match s with
  | Const (s,_) -> s
  | Int n -> Big_int.string_of_big_int n
  | Rat n -> Ratio.string_of_ratio n
  | Real f -> string_of_float f

let pp buf s = Buffer.add_string buf (to_string s)

let to_string_tstp s = match s with
  | _ when eq s not_symbol -> "~"
  | _ when eq s eq_symbol -> "="
  | _ when eq s lambda_symbol -> "^"
  | _ when eq s exists_symbol -> "?"
  | _ when eq s forall_symbol -> "!"
  | _ when eq s and_symbol -> "&"
  | _ when eq s or_symbol -> "|"
  | _ when eq s imply_symbol -> "=>"
  | _ -> to_string s (* default *)

let pp_tstp buf s = Buffer.add_string buf (to_string_tstp s)

let fmt fmt s = Format.pp_print_string fmt (to_string s)

let bij =
  Bij.switch
    ~inject:(fun s -> match s with
      | Const (s,info) -> 'c', Bij.(BranchTo (pair string_ int_, (s, info.attrs)))
      | Int n -> 'i', Bij.(BranchTo (string_, Big_int.string_of_big_int n))
      | Rat n -> 'r', Bij.(BranchTo (string_, Ratio.string_of_ratio n))
      | Real f -> 'f', Bij.(BranchTo (float_, f)))
    ~extract:(fun c -> match c with
      | 'c' -> Bij.(BranchFrom (pair string_ int_, fun (s,attrs) -> mk_const ~attrs s))
      | 'i' -> Bij.(BranchFrom (string_, (fun n -> mk_bigint (Big_int.big_int_of_string n))))
      | 'r' -> Bij.(BranchFrom (string_, (fun n -> mk_ratio (Ratio.ratio_of_string n))))
      | 'f' -> Bij.(BranchFrom (float_, mk_real))
      | c -> raise (Bij.DecodingError "expected symbol"))

(** {2 Generation of symbols} *)

module Gensym = struct
  type t = {
    prefix : string;
    mutable count : int;
  }

  let create ?(prefix="logtk") () =
    (if prefix = "" then failwith "Symbol.Gensym: need a non-empty prefix");
    { prefix;
      count = 0;
    }

  let new_ gensym =
    let n = gensym.count in
    gensym.count <- n + 1;
    let s = Util.sprintf "%s%d" gensym.prefix n in
    mk_const s
end
