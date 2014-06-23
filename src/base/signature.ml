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

module SMap = Symbol.Map

type t = Type.t SMap.t
  (** A signature maps symbols to their sort *)

let empty = SMap.empty

let is_empty = SMap.is_empty

let singleton = SMap.singleton

let mem signature s = SMap.mem s signature

let find signature s =
  try Some (SMap.find s signature)
  with Not_found -> None

let find_exn signature s =
  SMap.find s signature

let declare signature symb ty =
  try
    let ty' = find_exn signature symb in
    if Type.eq ty ty'
      then signature  (* ok *)
      else
        let msg = Util.sprintf "type error for %a: %a and %a are incompatible"
          Symbol.pp symb Type.pp ty Type.pp ty'
        in
        raise (Type.Error msg)
  with Not_found ->
    if not (ScopedTerm.DB.closed (ty : Type.t :> ScopedTerm.t))
      then raise (Invalid_argument "Signature.declare: non-closed type");
    SMap.add symb ty signature

let cardinal signature = SMap.cardinal signature

let arity signature s =
  let ty = find_exn signature s in
  match Type.arity ty with
  | Type.NoArity ->
    failwith (Util.sprintf "symbol %a has ill-formed type %a" Symbol.pp s Type.pp ty)
  | Type.Arity (a,b) -> a, b

let is_ground signature =
  SMap.for_all (fun _ ty -> Type.is_ground ty) signature

let merge s1 s2 =
  SMap.merge
    (fun s t1 t2 -> match t1, t2 with
      | None, None -> None (* ?? *)
      | Some ty1, Some ty2 ->
        if Type.eq ty1 ty2
          then Some ty1
          else
            let msg =
              Util.sprintf "Signature.merge: incompatible types for %a: %a and %a"
              Symbol.pp s Type.pp ty1 Type.pp ty2
            in
            raise (Type.Error msg)
      | Some s1, None -> Some s1
      | None, Some s2 -> Some s2)
    s1 s2

let filter s p = SMap.filter p s

let diff s1 s2 =
  SMap.merge
    (fun s ty1 ty2 -> match ty1, ty2 with
      | Some ty1, None -> Some ty1
      | Some _, Some _
      | None, Some _
      | None, None -> None)
    s1 s2

let well_founded s =
  SMap.exists
    (fun _ ty -> match Type.arity ty with
      | Type.Arity (_, 0) -> true
      | _ -> false)
    s

module Seq = struct
  let symbols s =
    SMap.to_seq s |> Sequence.map fst

  let types s =
    SMap.to_seq s |> Sequence.map snd

  let to_seq = SMap.to_seq

  let of_seq = SMap.of_seq
end

let to_set s = Seq.symbols s |> Symbol.Set.of_seq

let to_list s = Seq.to_seq s |> Sequence.to_rev_list

let of_list s = Sequence.of_list s |> Seq.of_seq

let iter s f =
  SMap.iter f s

let fold s acc f =
  SMap.fold (fun s ty acc -> f acc s ty) s acc

(** {2 IO} *)

let pp buf s =
  let pp_pair buf (s,ty) =
    Printf.bprintf buf "%a: %a" Symbol.pp s Type.pp ty in
  Printf.bprintf buf "{";
  Util.pp_seq pp_pair buf (Seq.to_seq s);
  Printf.bprintf buf "}";
  ()

let to_string s =
  Util.sprintf "%a" pp s

let fmt fmt s =
  Format.pp_print_string fmt (to_string s)

  (* TODO
let bij =
  let open Bij in
  map
    ~inject:(fun signature -> Sequence.to_list (to_seq signature))
    ~extract:(fun l -> of_seq (Sequence.of_list l))
    (list_ (pair string_ Symbol.bij))
    *)

module TPTP = struct
  let is_bool signature s =
    let rec _is_bool_ty ty = match Type.view ty with
      | Type.Fun (ret, _)  when Type.eq ret Type.TPTP.o -> true
      | _ when Type.eq ty Type.TPTP.o -> true
      | Type.Forall ty' -> _is_bool_ty ty'
      | _ -> false
    in _is_bool_ty (find_exn signature s)

  let is_not_bool signature s =
    not (is_bool signature s)

  (** {2 Pre-defined symbols} *)

  let x = Type.var 0

  let ty_1_to_int = Type.(forall [x] (Type.TPTP.int <=. x))
  let ty2op = Type.(forall [x] (x <== [x;x]))
  let ty2op_to_i = Type.(TPTP.(int <== [int;int]))
  let ty2op_to_o = Type.(TPTP.(o <== [int;int]))

  let arith_table =
    let open Type.TPTP in
    [ Symbol.TPTP.Arith.less, ty2op_to_o
    ; Symbol.TPTP.Arith.lesseq, ty2op_to_o
    ; Symbol.TPTP.Arith.greater, ty2op_to_o
    ; Symbol.TPTP.Arith.greatereq, ty2op_to_o
    ; Symbol.TPTP.Arith.uminus, Type.(forall [x] (x <=. x))
    ; Symbol.TPTP.Arith.sum, ty2op
    ; Symbol.TPTP.Arith.difference, ty2op
    ; Symbol.TPTP.Arith.product, ty2op
    ; Symbol.TPTP.Arith.quotient, ty2op
    ; Symbol.TPTP.Arith.quotient_e, ty2op_to_i
    ; Symbol.TPTP.Arith.quotient_f, ty2op_to_i
    ; Symbol.TPTP.Arith.quotient_t, ty2op_to_i
    ; Symbol.TPTP.Arith.remainder_e, ty2op_to_i
    ; Symbol.TPTP.Arith.remainder_f, ty2op_to_i
    ; Symbol.TPTP.Arith.remainder_t, ty2op_to_i
    ; Symbol.TPTP.Arith.floor, ty_1_to_int
    ; Symbol.TPTP.Arith.ceiling, ty_1_to_int
    ; Symbol.TPTP.Arith.round, ty_1_to_int
    ; Symbol.TPTP.Arith.truncate, ty_1_to_int
    ; Symbol.TPTP.Arith.to_int, Type.(forall [x] (int <=. x))
    ; Symbol.TPTP.Arith.to_rat, Type.(forall [x] (rat <=. x))
    ; Symbol.TPTP.Arith.is_int, Type.(forall [x] (o <=. x))
    ; Symbol.TPTP.Arith.is_rat, Type.(forall [x] (o <=. x))
    ]

  let table =
    let open Type.TPTP in
    [ Symbol.Base.true_, o
    ; Symbol.Base.false_, o
    ; Symbol.Base.eq, Type.(forall [x] (o <== [x;x]))
    ; Symbol.Base.neq, Type.(forall [x] (o <== [x;x]))
    ; Symbol.Base.exists, Type.(forall [x] (o <=. (o <=. x)))
    ; Symbol.Base.forall, Type.(forall [x] (o <=. (o <=. x)))
    ; Symbol.Base.not_, Type.(o <=. o)
    ; Symbol.Base.imply, Type.(o <== [o;o])
    ; Symbol.Base.and_, Type.(o <== [o;o])
    ; Symbol.Base.or_, Type.(o <== [o;o])
    ; Symbol.Base.equiv, Type.(o <== [o;o])
    ; Symbol.Base.xor, Type.(o <== [o;o])
    ; Symbol.Base.wildcard, Type.(forall [x] x)
    (* special symbols
    ; Symbol.db_symbol
    ; Symbol.split_symbol
    ; Symbol.const_symbol
    ; Symbol.num_symbol
    ; *)
    ]

  let base = of_list table

  let base_symbols = to_set base

  let is_base_symbol s = Symbol.Set.mem s base_symbols

  let pp_no_base buf s =
    pp buf (diff s base)

  (** {2 Arith} *)

  module Arith = struct
    let operators =
      Sequence.of_list arith_table
        |> Sequence.map fst
        |> Symbol.Set.of_seq

    let is_operator s = Symbol.Set.mem s operators

    let base = of_list arith_table
    let full = of_list (table @ arith_table)
  end
end
