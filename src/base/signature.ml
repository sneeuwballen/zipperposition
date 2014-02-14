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

(** {1 Signature} *)

module SMap = Sequence.Map.Make(String)
module SSet = Symbol.Set

type t = Symbol.t SMap.t
  (** A signature maps symbols to their sort *)

let empty = SMap.empty

let mem signature s = SMap.mem s signature

let find signature s =
  SMap.find s signature

let find_type signature s =
  Symbol.ty (find signature s)

let declare signature s symb =
  try
    let symb' = find signature s in
    if Symbol.eq symb symb'
      then signature  (* ok *)
      else
        let ty_s, ty_s' = Symbol.ty symb, Symbol.ty symb' in
        let msg = Util.sprintf "type error for %s: %a and %a are incompatible"
          s Type.pp ty_s Type.pp ty_s'
        in
        raise (Invalid_argument msg)
  with Not_found ->
    SMap.add s symb signature

let declare_ty signature s ty =
  let symb = Symbol.mk_const ~ty s in
  declare signature s symb

let declare_sym signature s = match s with
  | Symbol.Const (n,_) -> declare signature n s
  | _ -> raise (Invalid_argument "Signature.declare_sym: expected string")

let cardinal signature = SMap.cardinal signature

let arity signature s =
  let symb = find signature s in
  Type.arity (Symbol.ty symb)

let is_ground signature =
  SMap.for_all (fun _ symb -> Type.is_ground (Symbol.ty symb)) signature

let is_bool signature s =
  let symb = find signature s in
  match Symbol.ty symb with
  | {Type.ty=Type.Fun (ret, _)} when Type.eq ret Type.o -> true
  | ty when Type.eq ty Type.o -> true
  | _ -> false

let is_not_bool signature s =
  not (is_bool signature s)

let to_seq signature = SMap.to_seq signature
let of_seq seq = SMap.of_seq seq

let to_list signature = Sequence.to_rev_list (to_seq signature)
let of_list l = SMap.of_seq (Sequence.of_list l)

let merge s1 s2 =
  SMap.merge
    (fun s s1 s2 -> match s1, s2 with
      | None, None -> None (* ?? *)
      | Some s1, Some s2 ->
        if Symbol.eq s1 s2
          then Some s1
          else
            let msg =
              Util.sprintf "Signature.merge: incompatible types for %s: %a and %a"
              s Type.pp (Symbol.ty s1) Type.pp (Symbol.ty s2)
            in
            failwith msg
      | Some s1, None -> Some s1
      | None, Some s2 -> Some s2)
    s1 s2

let map s f = SMap.mapi f s

let filter s p = SMap.filter p s

let diff s1 s2 =
  SMap.merge
    (fun s ty1 ty2 -> match ty1, ty2 with
      | Some ty1, None -> Some ty1
      | Some _, Some _
      | None, Some _
      | None, None -> None)
    s1 s2

let size s = SMap.cardinal s

let well_founded s =
  SMap.exists
    (fun _ sym -> snd (Type.arity (Symbol.ty sym)) = 0)
    s

let to_symbols signature =
  SMap.fold (fun _ s l -> s :: l) signature []

let to_set signature =
  SMap.fold (fun _ s set -> SSet.add s set) signature SSet.empty

let iter s f =
  SMap.iter f s

let fold s acc f =
  SMap.fold (fun s ty acc -> f acc s ty) s acc

(** {2 IO} *)

let pp buf s =
  let pp_pair buf (_,s) = Printf.bprintf buf "%a: %a" Symbol.pp s Type.pp (Symbol.ty s) in
  Printf.bprintf buf "{";
  Util.pp_seq pp_pair buf (to_seq s);
  Printf.bprintf buf "}";
  ()

let to_string s =
  Util.sprintf "%a" pp s

let fmt fmt s =
  Format.pp_print_string fmt (to_string s)

let bij =
  let open Bij in
  map
    ~inject:(fun signature -> Sequence.to_list (to_seq signature))
    ~extract:(fun l -> of_seq (Sequence.of_list l))
    (list_ (pair string_ Symbol.bij))

(** {2 Pre-defined symbols} *)

let arith_table =
  [ Symbol.Arith.less
  ; Symbol.Arith.lesseq
  ; Symbol.Arith.greater
  ; Symbol.Arith.greatereq
  ; Symbol.Arith.uminus
  ; Symbol.Arith.sum
  ; Symbol.Arith.difference
  ; Symbol.Arith.product
  ; Symbol.Arith.quotient
  ; Symbol.Arith.quotient_e
  ; Symbol.Arith.quotient_f
  ; Symbol.Arith.quotient_t
  ; Symbol.Arith.remainder_e
  ; Symbol.Arith.remainder_f
  ; Symbol.Arith.remainder_t
  ; Symbol.Arith.floor
  ; Symbol.Arith.ceiling
  ; Symbol.Arith.round
  ; Symbol.Arith.truncate
  ; Symbol.Arith.to_int
  ; Symbol.Arith.to_rat
  ; Symbol.Arith.to_real
  ; Symbol.Arith.is_int
  ; Symbol.Arith.is_rat
  ; Symbol.Arith.is_real
  ]

let table =
  [ Symbol.true_symbol
  ; Symbol.false_symbol
  ; Symbol.eq_symbol
  ; Symbol.exists_symbol
  ; Symbol.forall_symbol
  ; Symbol.not_symbol
  ; Symbol.imply_symbol
  ; Symbol.and_symbol
  ; Symbol.or_symbol
  ; Symbol.equiv_symbol
  ; Symbol.wildcard_symbol
  (* special symbols
  ; Symbol.db_symbol
  ; Symbol.split_symbol
  ; Symbol.const_symbol
  ; Symbol.num_symbol
  ; *)
  ] @ arith_table

let of_list_sym l =
  List.fold_left
    (fun signature symb -> match symb with
      | Symbol.Const (s, _) -> SMap.add s symb signature
      | _ -> assert false)
    empty l

let base = of_list_sym table

let base_symbols = SSet.of_seq (Sequence.of_list table)

let is_base_symbol s = SSet.mem s base_symbols

let pp_no_base buf s =
  pp buf (diff s base)

(** {2 Arith} *)

module Arith = struct
  let operators = arith_table

  let is_operator s = List.exists (fun s' -> Symbol.eq s s') arith_table

  let signature = of_list_sym arith_table
end

