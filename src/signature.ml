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

module SMap = Symbol.SMap

type t = Type.t SMap.t
  (** A signature maps symbols to their sort *)

let empty = SMap.empty

let mem signature s = SMap.mem s signature


let find signature s =
  SMap.find s signature

let declare signature symb ty =
  try
    let ty' = find signature symb in
    if Type.alpha_equiv ty ty'
      then signature  (* ok *)
      else
        let msg = Util.sprintf "type error for %a: %a and %a are incompatible"
          Symbol.pp symb Type.pp ty Type.pp ty'
        in
        failwith msg
  with Not_found ->
    SMap.add symb ty signature

let cardinal signature = SMap.cardinal signature

let arity signature s =
  let ty = find signature s in
  Type.arity ty

let is_ground signature =
  SMap.for_all (fun _ ty -> Type.is_ground ty) signature

let is_bool signature s =
  match SMap.find s signature with
  | Type.Fun (ret, _) when Type.eq ret Type.o -> true
  | _ -> false

let is_not_bool signature s =
  not (is_bool signature s)

let to_seq signature = SMap.to_seq signature
let of_seq seq = SMap.of_seq seq

let merge s1 s2 =
  SMap.merge
    (fun s t1 t2 -> match t1, t2 with
      | None, None -> None (* ?? *)
      | Some t1, Some t2 ->
        if Type.alpha_equiv t1 t2
          then Some t1
          else
            let msg =
              Util.sprintf "Signature.merge: incompatible types for %a: %a and %a"
              Symbol.pp s Type.pp t1 Type.pp t2
            in
            failwith msg
      | Some t1, None -> Some t1
      | None, Some t2 -> Some t2)
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

let curry s = SMap.map Type.curry s

let uncurry s = SMap.map Type.uncurry s

let to_symbols signature =
  SMap.fold (fun s _ l -> s :: l) signature []

let to_set signature =
  SMap.fold (fun s _ set -> Symbol.SSet.add s set) signature Symbol.SSet.empty

let iter s f =
  SMap.iter f s

let fold s acc f =
  SMap.fold (fun s ty acc -> f acc s ty) s acc

(** {2 IO} *)

let pp buf s =
  let pp_pair buf (s,ty) = Printf.bprintf buf "%a: %a" Symbol.pp s Type.pp ty in
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
    (list_ (pair Symbol.bij Type.bij))

(** {2 Pre-defined symbols} *)

let table =
  let open Type.Infix in
  let x = Type.var "a" in
  let y = Type.var "b" in
  [ Symbol.true_symbol, Type.o;
    Symbol.false_symbol, Type.o;
    Symbol.eq_symbol, Type.o <== [x; x];
    Symbol.exists_symbol, Type.o <=. (Type.o <=. x);
    Symbol.forall_symbol, Type.o <=. (Type.o <=. x);
    Symbol.lambda_symbol, y <=. ((y <=. x) <=. x);  (* (x -> (x -> y)) -> y *)
    Symbol.not_symbol, Type.o <=. Type.o;
    Symbol.imply_symbol, Type.o <== [Type.o; Type.o];
    Symbol.and_symbol, Type.o <== [Type.o; Type.o];
    Symbol.or_symbol, Type.o <== [Type.o; Type.o];
    Symbol.equiv_symbol, Type.o <== [Type.o; Type.o];
    (* special symbols, used for precedence
    Symbol.db_symbol, Type.i;
    Symbol.split_symbol, Type.o;
    Symbol.const_symbol, Type.i;
    Symbol.num_symbol, Type.i;
    *)
  ]

(* TODO: arithmetic special symbols, especially polymorphic ones ($less, etc.) *)

(** default signature, containing predefined symbols with their arities and sorts *)
let base =
  List.fold_left
    (fun signature (symb,ty) -> SMap.add symb ty signature)
    empty table

(** Set of base symbols *)
let base_symbols =
  List.fold_left (fun set (s, _) -> Symbol.SSet.add s set)
    Symbol.SSet.empty table

let is_base_symbol s = Symbol.SSet.mem s base_symbols

let pp_no_base buf s =
  pp buf (diff s base)

(** {2 Arith} *)

module Arith = struct
  let table = 
    let open Type.Infix in
    let module S = Symbol in
    let x = Type.var "X" in
    [ S.mk_const "$less", Type.o <== [x; x]
    ; S.mk_const "$lesseq", Type.o <== [x; x]
    ; S.mk_const "$greater", Type.o <== [x; x]
    ; S.mk_const "$greatereq", Type.o <== [x; x]
    ; S.mk_const "$uminus", x <== [x]
    ; S.mk_const "$sum", x <== [x; x]
    ; S.mk_const "$difference", x <== [x; x]
    ; S.mk_const "$product", x <== [x; x]
    ; S.mk_const "$quotient", x <== [x; x]
    ; S.mk_const "$quotient_f", Type.int <== [x; x]
    ; S.mk_const "$quotient_t", Type.int <== [x; x]
    ; S.mk_const "$quotient_e", Type.int <== [x; x]
    ; S.mk_const "$remainder_f", Type.int <== [x; x]
    ; S.mk_const "$remainder_t", Type.int <== [x; x]
    ; S.mk_const "$remainder_e", Type.int <== [x; x]
    ; S.mk_const "$floor", Type.int <== [x]
    ; S.mk_const "$ceiling", Type.int <== [x]
    ; S.mk_const "$truncate", Type.int <== [x]
    ; S.mk_const "$round", Type.int <== [x]
    ; S.mk_const "$is_int", Type.o <== [x]
    ; S.mk_const "$is_rat", Type.o <== [x]
    ; S.mk_const "$to_int", Type.int <== [x]
    ; S.mk_const "$to_rat", Type.rat <== [x]
    ; S.mk_const "$to_real", Type.real <== [x]
    ]

  let operators = List.map fst table

  let is_operator s = List.exists (fun s' -> Symbol.eq s s') operators

  let signature =
    List.fold_left (fun sign (s, ty) -> declare sign s ty) empty table
end

