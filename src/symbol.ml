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
  mutable flags : int;
  ty : Type.t;
} (** Additional information for hashconsed symbols *)

type symbol = t

(** {2 Boolean flags} *)

type flag = int

let flag_skolem = 1 lsl 0
let flag_split = 1 lsl 1
let flag_binder = 1 lsl 2
let flag_infix = 1 lsl 3
let flag_ac = 1 lsl 4
let flag_multiset = 1 lsl 5
let flag_fresh_const = 1 lsl 6
let flag_commut = 1 lsl 7
let flag_distinct = 1 lsl 8

let flags s = match s with
  | Const (_, info) -> info.flags
  | Int _
  | Rat _
  | Real _ -> 0

let has_flag flag s = (flags s land flag) <> 0
let add_flag flag s = match s with
  | Const (_, info) -> info.flags <- info.flags lor flag
  | _ -> failwith "cannot add_flag to numeric symbol"

(** Hashconsing *)

let eq_base s1 s2 = match s1, s2 with
  | Const (s1,i1), Const (s2, i2) -> s1 = s2 && Type.eq i1.ty i2.ty
  | Int n1, Int n2 -> Big_int.eq_big_int n1 n2
  | Rat n1, Rat n2 -> Ratio.eq_ratio n1 n2
  | Real f1, Real f2 -> f1 = f2
  | _ -> false

let hash_base s = match s with
  | Const (s, i) -> Hash.combine (Hash.hash_string s) (Type.hash i.ty)
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

(** {2 Basic operations} *)

let compare s1 s2 =
  let __to_int = function
    | Int _ -> 1
    | Rat _ -> 2
    | Real _ -> 3
    | Const _ -> 4  (* const are bigger! *)
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
  | Int n -> Hash.hash_string (Big_int.string_of_big_int n)
  | Rat n -> Hash.hash_string (Ratio.string_of_ratio n)
  | Real f -> int_of_float f

let mk_const ?(flags=0) ~ty s =
  begin match Type.free_vars ty with
    | [] -> ()
    | _::_ ->
      let msg = Util.sprintf "Symbol.mk_const (%s): type %a has free variables" s Type.pp ty in
      raise (Type.Error msg)
  end;
  let info = { tag= ~-1; flags; ty; } in
  let symb = Const (s, info) in
  let symb' = H.hashcons symb in
  if symb' == symb then begin
    (* check syntactically whether it's a distinct symbol or not *)
    let is_distinct = s <> "" &&  s.[0] = '"' && s.[String.length s - 1] = '"' in
    if is_distinct then add_flag flag_distinct symb';
    end;
  symb'

let mk_distinct ?(flags=0) ?(ty=Type.i) s =
  let flags = flags lor flag_distinct in
  mk_const ~flags ~ty s

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

let of_basic ?(ty=Type.i) s = match s with
  | Basic.Sym.Int n -> mk_bigint n
  | Basic.Sym.Rat n -> mk_ratio n
  | Basic.Sym.Real n -> mk_real n
  | Basic.Sym.Const s -> mk_const ~ty s

let to_basic = function
  | Int n -> Basic.Sym.mk_bigint n
  | Rat n -> Basic.Sym.mk_ratio n
  | Real n -> Basic.Sym.mk_real n
  | Const (s,_) -> Basic.Sym.mk_const s

let is_const s = match s with
  | Const _ -> true | _ -> false

let is_int s = match s with
  | Int _ -> true | _ -> false

let is_rat s = match s with
  | Int _
  | Rat _ -> true
  | _ -> false

let is_real s = match s with
  | Real f -> true | _ -> false

let is_numeric s = match s with
  | Real _ | Int _ | Rat _ -> true | _ -> false

let is_distinct s = match s with
  | Const _ -> has_flag flag_distinct s | _ -> false

let ty s = match s with
  | Int _ -> Type.int
  | Rat _ -> Type.rat
  | Real _ -> Type.real
  | Const (_, i) -> i.ty

module Tbl = Hashtbl.Make(struct
  type t = symbol
  let equal = eq
  let hash = hash
end)

module Map = Sequence.Map.Make(struct
  type t = symbol
  let compare = compare
end)

module Set = Sequence.Set.Make(struct
  type t = symbol
  let compare = compare
end)

(** {2 connectives} *)

let x = Type.var 0

let true_symbol = mk_const ~ty:Type.o "$true"
let false_symbol = mk_const ~ty:Type.o "$false"

let eq_symbol =
  let flags = flag_infix lor flag_multiset lor flag_commut in
  let ty = Type.(forall [x] (o <== [x;x])) in
  mk_const ~ty ~flags "="

let ty_binder = Type.(forall [x] (o <=. (o <=. x)))  (* ('a -> o) -> o *)

let exists_symbol = mk_const ~flags:flag_binder ~ty:ty_binder "?"
let forall_symbol = mk_const ~flags:flag_binder ~ty:ty_binder "!"

let not_symbol = mk_const ~ty:Type.(o <=. o) "~"

let ty_bin_op = Type.(o <== [o;o])

let imply_symbol =
  mk_const ~flags:flag_infix ~ty:ty_bin_op "=>"

let equiv_symbol =
  mk_const ~flags:(flag_infix lor flag_commut) ~ty:ty_bin_op "<=>"

let and_symbol =
  mk_const ~flags:(flag_infix lor flag_ac lor flag_multiset) ~ty:ty_bin_op "&"

let or_symbol =
  mk_const ~flags:(flag_infix lor flag_ac lor flag_multiset) ~ty:ty_bin_op "|"

let connectives =
  [ true_symbol
  ; false_symbol
  ; eq_symbol
  ; equiv_symbol
  ; exists_symbol
  ; forall_symbol
  ; not_symbol
  ; imply_symbol
  ; and_symbol
  ; or_symbol
  ]

let is_connective s = List.exists (fun s' -> eq s s') connectives

(* wildcard symbol, a universal type *)
let wildcard_symbol =
  mk_const ~ty:Type.(forall [x] x) "$_"

(** {2 IO} *)

let to_string_debug s = match s with
  | Const (s,i) -> Util.sprintf "%s:%a" s Type.pp i.ty
  | Int n -> Big_int.string_of_big_int n
  | Rat n -> Ratio.string_of_ratio n
  | Real f -> string_of_float f

let pp_debug buf s = Buffer.add_string buf (to_string_debug s)

let to_string_tstp s = match s with
  | _ when eq s not_symbol -> "~"
  | _ when eq s eq_symbol -> "="
  | _ when eq s exists_symbol -> "?"
  | _ when eq s forall_symbol -> "!"
  | _ when eq s and_symbol -> "&"
  | _ when eq s or_symbol -> "|"
  | _ when eq s imply_symbol -> "=>"
  | Const (s, _) -> s
  | Int n -> Big_int.string_of_big_int n
  | Rat n -> Ratio.string_of_ratio n
  | Real f -> string_of_float f

let pp_tstp buf s = Buffer.add_string buf (to_string_tstp s)

let to_string = to_string_tstp

let __default_pp = ref pp_tstp
let pp buf s = !__default_pp buf s

let set_default_pp pp = __default_pp := pp

let fmt fmt s = Format.pp_print_string fmt (to_string_debug s)

let bij =
  let bij_const = Bij.(triple string_ int_ Type.bij) in
  Bij.switch
    ~inject:(fun s -> match s with
      | Const (s,info) -> "const", Bij.(BranchTo (bij_const, (s, info.flags, info.ty)))
      | Int n -> "int", Bij.(BranchTo (string_, Big_int.string_of_big_int n))
      | Rat n -> "rat", Bij.(BranchTo (string_, Ratio.string_of_ratio n))
      | Real f -> "real", Bij.(BranchTo (float_, f)))
    ~extract:(fun c -> match c with
      | "const" -> Bij.(BranchFrom (bij_const, fun (s,flags,ty) -> mk_const ~flags ~ty s))
      | "int" -> Bij.(BranchFrom (string_, (fun n -> mk_bigint (Big_int.big_int_of_string n))))
      | "rat" -> Bij.(BranchFrom (string_, (fun n -> mk_ratio (Ratio.ratio_of_string n))))
      | "real" -> Bij.(BranchFrom (float_, mk_real))
      | c -> raise (Bij.DecodingError "expected symbol"))

(** {2 Arith} *)

module Arith = struct
  exception TypeMismatch of string

  (* type helpers *)

  let ty_1 = Type.(forall [x] (x <=. x))
  let ty_2 = Type.(forall [x] (x <== [x;x]))
  let ty_2_int = Type.(forall [x] (int <== [x;x]))
  let ty_cast into = Type.(forall [x] (into <=. x))
  let ty_check = Type.(forall [x] (o <=. x)) 
  let ty_2_o = Type.(forall [x] (o <== [x;x]))

  (* helper to raise errors *)
  let _ty_mismatch fmt =
    let buf = Buffer.create 32 in
    Printf.kbprintf
      (fun _ -> raise (TypeMismatch (Buffer.contents buf)))
      buf
      fmt

  let sign s = match s with
  | Int n -> Big_int.sign_big_int n
  | Rat n -> Ratio.sign_ratio n
  | Real f when f > 0. -> 1
  | Real f when f < 0. -> -1
  | Real f -> 0
  | _ -> _ty_mismatch "cannot compute sign of symbol %a" pp s

  let floor = mk_const ~ty:(ty_cast Type.int) "$floor"
  let ceiling = mk_const ~ty:(ty_cast Type.int) "$ceiling"
  let truncate = mk_const ~ty:(ty_cast Type.int) "$truncate"
  let round = mk_const ~ty:(ty_cast Type.int) "$round"

  let prec = mk_const ~ty:ty_1 "$prec"
  let succ = mk_const ~ty:ty_1 "$succ"

  let one_i = mk_int 1
  let zero_i = mk_int 0
  let one_rat = mk_rat 1 1
  let zero_rat = mk_rat 0 1
  let one_f = mk_real 1.
  let zero_f = mk_real 0.

  let zero_of_ty ty =
    if Type.eq ty Type.int then zero_i
    else if Type.eq ty Type.rat then zero_rat
    else if Type.eq ty Type.real then zero_f
    else _ty_mismatch "bad arith type %a for zero_of_ty" Type.pp ty

  let one_of_ty ty =
    if Type.eq ty Type.int then one_i
    else if Type.eq ty Type.rat then one_rat
    else if Type.eq ty Type.real then one_f
    else _ty_mismatch "bad arith type %a for one_of_ty" Type.pp ty

  let is_zero s = match s with
  | Int n -> Big_int.sign_big_int n = 0
  | Rat n -> Ratio.sign_ratio n = 0
  | Real f -> f = 0.
  | Const _ -> false

  let __one_i = Big_int.big_int_of_int 1
  let __m_one_i = Big_int.big_int_of_int ~-1
  let __one_rat = Ratio.ratio_of_int 1
  let __m_one_rat = Ratio.ratio_of_int ~-1

  let is_one s = match s with
  | Int n -> Big_int.eq_big_int n __one_i
  | Rat n -> Ratio.eq_ratio n __one_rat
  | Real f -> f = 1.
  | Const _ -> false

  let is_minus_one s = match s with
  | Int n -> Big_int.eq_big_int n __m_one_i
  | Rat n -> Ratio.eq_ratio n __m_one_rat
  | Real f -> f = 1.
  | Const _ -> false

  let sum = mk_const ~ty:ty_2 "$sum"
  let difference = mk_const ~ty:ty_2 "$difference"
  let uminus = mk_const ~ty:ty_1 "$uminus"
  let product = mk_const ~ty:ty_2 "$product"
  let quotient = mk_const ~ty:ty_2 "$quotient"

  let quotient_e = mk_const ~ty:ty_2_int "$quotient_e"
  let quotient_t = mk_const ~ty:ty_2_int "$quotient_t"
  let quotient_f = mk_const ~ty:ty_2_int "$quotient_f"
  let remainder_e = mk_const ~ty:ty_2_int "$remainder_e"
  let remainder_t = mk_const ~ty:ty_2_int "$remainder_t"
  let remainder_f = mk_const ~ty:ty_2_int "$remainder_f"

  let is_int = mk_const ~ty:ty_check "$is_int"
  let is_rat = mk_const ~ty:ty_check "$is_rat"
  let is_real = mk_const ~ty:ty_check "$is_real"

  let to_int = mk_const ~ty:(ty_cast Type.int) "$to_int"
  let to_rat = mk_const ~ty:(ty_cast Type.rat) "$to_rat"
  let to_real = mk_const ~ty:(ty_cast Type.real) "$to_real"

  let less = mk_const ~ty:ty_2_o "$less"
  let lesseq = mk_const ~ty:ty_2_o "$lesseq"
  let greater = mk_const ~ty:ty_2_o "$greater"
  let greatereq = mk_const ~ty:ty_2_o "$greatereq"

  let set =
    let l = [
      sum; difference; uminus; product; quotient;
      quotient_e; quotient_t; quotient_f;
      remainder_e; remainder_t; remainder_f;
      less; lesseq; greater; greatereq;
    ] in
    Set.of_seq (Sequence.of_list l)

  let is_arith s = Set.mem s set

  module Op = struct
    let floor s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.floor_ratio n)
    | Real f ->
      begin try
        let i = int_of_float (Pervasives.floor f) in 
        mk_int i
      with _ -> s  (* keep *)
      end
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let ceiling s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.ceiling_ratio n)
    | Real f -> mk_real (Pervasives.ceil f)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let truncate s = match s with
    | Int _ -> s
    | Rat n when Ratio.sign_ratio n >= 0 -> mk_bigint (Ratio.floor_ratio n)
    | Rat n -> mk_bigint (Big_int.minus_big_int (Ratio.floor_ratio (Ratio.abs_ratio n)))
    | Real f -> mk_int (Pervasives.truncate f)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let round s = match s with
    | Int _ -> s
    | Rat n -> mk_bigint (Ratio.round_ratio n)
    | Real f ->
      let f' = Pervasives.floor f in
      let i = if f -. f' > 0.5 then int_of_float f' else (int_of_float f') + 1 in
      mk_int i
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let prec s = match s with
    | Int n -> mk_bigint (Big_int.pred_big_int n)
    | Rat n -> mk_ratio (Ratio.add_int_ratio (-1) n)
    | Real f -> mk_real (f -. 1.)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let succ s = match s with
    | Int n -> mk_bigint (Big_int.succ_big_int n)
    | Rat n -> mk_ratio (Ratio.add_int_ratio 1 n)
    | Real f -> mk_real (f +. 1.)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let sum s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.add_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.add_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 +. f2)
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let difference s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.sub_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.sub_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 -. f2)
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let uminus s = match s with
    | Int n -> mk_bigint (Big_int.minus_big_int n)
    | Rat n -> mk_ratio (Ratio.minus_ratio n)
    | Real f -> mk_real (~-. f)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let product s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> mk_bigint (Big_int.mult_big_int n1 n2)
    | Rat n1, Rat n2 -> mk_ratio (Ratio.mult_ratio n1 n2)
    | Real f1, Real f2 -> mk_real (f1 *. f2)
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let quotient s1 s2 = match s1, s2 with
    | Int n1, Int n2 ->
      let q, r = Big_int.quomod_big_int n1 n2 in
      if Big_int.sign_big_int r = 0
        then mk_bigint q
        else _ty_mismatch "non-exact integral division: %a / %a" pp s1 pp s2
    | Rat n1, Rat n2 ->
      begin try mk_ratio (Ratio.div_ratio n1 n2)
      with Failure _ -> raise Division_by_zero
      end
    | Real f1, Real f2 ->
      let f = f1 /. f2 in
      if f == infinity then raise Division_by_zero else mk_real f
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

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
    | Int n1, Int n2 -> mk_bigint (snd (Big_int.quomod_big_int n1 n2))
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
    | Real _ -> _ty_mismatch "cannot compute to_rat of real %a" pp s
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let to_real s = match s with
    | Int n -> mk_real (Big_int.float_of_big_int n)
    | Rat n -> mk_real (Ratio.float_of_ratio n)
    | Real _ -> s
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let abs s = match s with
    | Int n -> mk_bigint (Big_int.abs_big_int n)
    | Rat n -> mk_ratio (Ratio.abs_ratio n)
    | Real f -> mk_real (Pervasives.abs_float f)
    | Const _ -> _ty_mismatch "not a numeric constant: %a" pp s

    let divides a b = match a, b with
    | Rat i, Rat _ -> Ratio.sign_ratio i <> 0
    | Real f, Real _ -> f <> 0.
    | Int a, Int b ->
      Big_int.sign_big_int a <> 0 &&
      Big_int.sign_big_int (Big_int.mod_big_int b a) = 0
    | _ -> _ty_mismatch "divides: expected two numerical types"

    let gcd a b = match a, b with
    | Rat _, Rat _ -> one_rat
    | Real _, Real _ -> one_f
    | Int a, Int b -> mk_bigint (Big_int.gcd_big_int a b)
    | _ -> _ty_mismatch "gcd: expected two numerical types"

    let less s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.lt_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.lt_ratio n1 n2
    | Real f1, Real f2 -> f1 < f2
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let lesseq s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.le_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.le_ratio n1 n2
    | Real f1, Real f2 -> f1 <= f2
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let greater s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.gt_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.gt_ratio n1 n2
    | Real f1, Real f2 -> f1 > f2
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2

    let greatereq s1 s2 = match s1, s2 with
    | Int n1, Int n2 -> Big_int.ge_big_int n1 n2
    | Rat n1, Rat n2 -> Ratio.ge_ratio n1 n2
    | Real f1, Real f2 -> f1 >= f2
    | Const _, _ -> _ty_mismatch "not a numeric constant: %a" pp s1
    | _, Const _ -> _ty_mismatch "not a numeric constant: %a" pp s2
    | _ -> _ty_mismatch "incompatible numeric types: %a and %a" pp s1 pp s2
  end
end

(** {2 "Magic" symbols} *)

(** pseudo symbol kept for locating bound vars in precedence. Bound
    vars are grouped in the precedence together w.r.t other symbols,
    but compare to each other by their index. *)
let db_symbol = mk_const ~ty:Type.i "$$db_magic_cookie"

(** pseudo symbol for locating split symbols in precedence. Split
    symbols compare lexicographically with other split symbols,
    but are in a fixed location in precedence w.r.t other kinds of
    symbols. *)
let split_symbol = mk_const ~ty:Type.i "$$split_magic_cookie"

(** pseudo symbol for locating magic constants in precedence.
    This is useful for keeping the precedence finite while managing
    an infinite set of fresh constants, that are used for
    testing terms for ground joinability (replacing variables
    with such constants) *)
let const_symbol = mk_const ~ty:Type.i "$$const_magic_cookie"

(** pseudo symbol to locate numbers in the precedence *)
let num_symbol = mk_const ~ty:Type.i "$$num_magic_cookie"

(** Infinite set of symbols, accessed by index, that will not collide with
    the signature of the problem *)
let mk_fresh_const i ~ty =
  mk_const ~flags:flag_fresh_const ~ty ("$$c_" ^ string_of_int i)

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

  let new_ gensym ~ty =
    let n = gensym.count in
    gensym.count <- n + 1;
    let s = Util.sprintf "%s%d" gensym.prefix n in
    mk_const ~ty s
end
