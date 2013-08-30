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
  | Num of Num.num
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
  | Num _
  | Real _ -> 0

let has_attr attr s = (attrs s land attr) <> 0

(** Hashconsing *)

let eq_base s1 s2 = match s1, s2 with
  | Const (s1,_), Const (s2, _) -> s1 = s2
  | Num n1, Num n2 -> Num.eq_num n1 n2
  | Real f1, Real f2 -> f1 = f2
  | _ -> false

let hash_base s = match s with
  | Const (s, _) -> Hash.hash_string s
  | Num n -> Hashtbl.hash n
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

let compare s1 s2 = match s1, s2 with
  | Const (_, info1), Const (_, info2) -> info1.tag - info2.tag
  | Num n1, Num n2 -> Num.compare_num n1 n2
  | Real f1, Real f2 -> Pervasives.compare f1 f2
  | Const _, _ -> 1
  | _, Const _ -> -1
  | Num _, _ -> 1
  | _, Num _ -> -1

let eq a b = compare a b = 0

let hash s = match s with
  | Const (_, info) -> info.tag
  | Num n -> Hashtbl.hash n
  | Real f -> int_of_float f

let mk_const ?(attrs=0) s =
  let symb = Const (s, { tag= ~-1; attrs; }) in
  H.hashcons symb

let mk_distinct ?(attrs=0) s =
  let attrs = attrs lor attr_distinct in
  mk_const ~attrs s

let mk_num n = Num n

let parse_num str =
  let n = Num.num_of_string str in
  Num n

let mk_int i = Num (Num.num_of_int i)

let mk_real f = Real f

let is_const s = match s with
  | Const _ -> true | _ -> false

let is_int s = match s with
  | Num n -> Num.is_integer_num n | _ -> false

let is_rat s = match s with
  | Num n -> not (Num.is_integer_num n) | _ -> false

let is_real s = match s with
  | Real f -> true | _ -> false

let is_numeric s = match s with
  | Real _ | Num _ -> true | _ -> false

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

  open Num

  let sign s = match s with
  | Num n -> sign_num n
  | Real f when f > 0. -> 1
  | Real f when f < 0. -> -1
  | Real f -> 0
  | _ -> raise TypeMismatch

  let floor s = match s with
  | Num n -> mk_num (floor_num n)
  | Real f -> mk_real (floor f)
  | _ -> raise TypeMismatch

  let ceiling s = match s with
  | Num n -> mk_num (ceiling_num n)
  | Real f -> mk_real (ceil f)
  | _ -> raise TypeMismatch

  let truncate s = match s with
  | Num n when sign_num n >= 0 -> mk_num (floor_num n)
  | Num n -> mk_num (minus_num (floor_num (abs_num n)))
  | Real f -> mk_num (num_of_int (truncate f))
  | _ -> raise TypeMismatch

  let round s = match s with
  | Num n -> mk_num (round_num n)
  | Real f ->
    let f' = Pervasives.floor f in
    let i = if f -. f' > 0.5 then int_of_float f' else (int_of_float f') + 1 in
    mk_num (num_of_int i)
  | _ -> raise TypeMismatch

  let prec s = match s with
  | Num n -> mk_num (pred_num n)
  | Real f -> mk_real (f -. 1.)
  | _ -> raise TypeMismatch

  let succ s = match s with
  | Num n -> mk_num (succ_num n)
  | Real f -> mk_real (f +. 1.)
  | _ -> raise TypeMismatch

  let one_i = mk_int 1
  let zero_i = mk_int 0
  let one_f = mk_real 1.
  let zero_f = mk_real 0.

  let is_zero s = match s with
  | Num n -> Num.sign_num n = 0
  | Real f -> f = 0.
  | _ -> raise TypeMismatch

  let sum s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> mk_num (n1 +/ n2)
  | Real f1, Real f2 -> mk_real (f1 +. f2)
  | _ -> raise TypeMismatch

  let difference s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> mk_num (n1 -/ n2)
  | Real f1, Real f2 -> mk_real (f1 -. f2)
  | _ -> raise TypeMismatch

  let uminus s = match s with
  | Num n -> mk_num (minus_num n)
  | Real f -> mk_real (~-. f)
  | _ -> raise TypeMismatch

  let product s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> mk_num (n1 */ n2)
  | Real f1, Real f2 -> mk_real (f1 *. f2)
  | _ -> raise TypeMismatch

  let quotient s1 s2 = match s1, s2 with
  | Num n1, Num n2 ->
    (try mk_num (n1 // n2) with Failure _ -> raise Division_by_zero)
  | Real f1, Real f2 ->
    let f = f1 /. f2 in if f == infinity then raise Division_by_zero else mk_real f
  | _ -> raise TypeMismatch

  let quotient_e s1 s2 =
    if sign s2 >= 0 then floor (quotient s1 s2) else ceiling (quotient s1 s2)

  let quotient_t s1 s2 = truncate (quotient s1 s2)

  let quotient_f s1 s2 = floor (quotient s1 s2)

  let remainder_e s1 s2 = difference s1 (product (quotient_e s1 s2) s2)

  let remainder_t s1 s2 = difference s1 (product (quotient_t s1 s2) s2)

  let remainder_f s1 s2 = difference s1 (product (quotient_f s1 s2) s2)

  let to_int s = floor s

  let to_rat s = s
    (* XXX not fully specified... *)

  let to_real s = match s with
  | Num n -> mk_real (float_of_num n)
  | Real _ -> s
  | _ -> raise TypeMismatch

  let less s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> n1 </ n2
  | Real f1, Real f2 -> f1 < f2
  | _ -> raise TypeMismatch

  let lesseq s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> n1 <=/ n2
  | Real f1, Real f2 -> f1 <= f2
  | _ -> raise TypeMismatch

  let greater s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> n1 >/ n2
  | Real f1, Real f2 -> f1 > f2
  | _ -> raise TypeMismatch

  let greatereq s1 s2 = match s1, s2 with
  | Num n1, Num n2 -> n1 >=/ n2
  | Real f1, Real f2 -> f1 >= f2
  | _ -> raise TypeMismatch
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
  | Num n -> Num.string_of_num n
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
  let open Bij in
  switch
    ~inject:(fun s -> match s with
      | Const (s,info) -> 'c', BranchTo (pair string_ int_, (s, info.attrs))
      | Num n -> 'n', BranchTo (string_, Num.string_of_num n)
      | Real f -> 'f', BranchTo (float_, f))
    ~extract:(fun c -> match c with
      | 'c' -> BranchFrom (pair string_ int_, fun (s,attrs) -> mk_const ~attrs s)
      | 'n' -> BranchFrom (string_, (fun n -> mk_num (Num.num_of_string n)))
      | 'f' -> BranchFrom (float_, mk_real)
      | c -> raise (DecodingError "expected symbol"))

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
