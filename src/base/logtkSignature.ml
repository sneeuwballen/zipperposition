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

(** {1 LogtkSignature} *)

module SMap = LogtkSymbol.Map

type t = LogtkType.t SMap.t
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
    if LogtkType.eq ty ty'
      then signature  (* ok *)
      else
        let msg = LogtkUtil.sprintf "type error for %a: %a and %a are incompatible"
          LogtkSymbol.pp symb LogtkType.pp ty LogtkType.pp ty'
        in
        raise (LogtkType.Error msg)
  with Not_found ->
    if not (LogtkScopedTerm.DB.closed (ty : LogtkType.t :> LogtkScopedTerm.t))
      then raise (Invalid_argument "LogtkSignature.declare: non-closed type");
    SMap.add symb ty signature

let cardinal signature = SMap.cardinal signature

let arity signature s =
  let ty = find_exn signature s in
  match LogtkType.arity ty with
  | LogtkType.NoArity ->
    failwith (LogtkUtil.sprintf "symbol %a has ill-formed type %a" LogtkSymbol.pp s LogtkType.pp ty)
  | LogtkType.Arity (a,b) -> a, b

let is_ground signature =
  SMap.for_all (fun _ ty -> LogtkType.is_ground ty) signature

let merge s1 s2 =
  SMap.merge
    (fun s t1 t2 -> match t1, t2 with
      | None, None -> None (* ?? *)
      | Some ty1, Some ty2 ->
        if LogtkType.eq ty1 ty2
          then Some ty1
          else
            let msg =
              LogtkUtil.sprintf "LogtkSignature.merge: incompatible types for %a: %a and %a"
              LogtkSymbol.pp s LogtkType.pp ty1 LogtkType.pp ty2
            in
            raise (LogtkType.Error msg)
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
    (fun _ ty -> match LogtkType.arity ty with
      | LogtkType.Arity (_, 0) -> true
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

let to_set s = Seq.symbols s |> LogtkSymbol.Set.of_seq

let to_list s = Seq.to_seq s |> Sequence.to_rev_list

let of_list s = Sequence.of_list s |> Seq.of_seq

let iter s f =
  SMap.iter f s

let fold s acc f =
  SMap.fold (fun s ty acc -> f acc s ty) s acc

(** {2 IO} *)

let pp buf s =
  let pp_pair buf (s,ty) =
    Printf.bprintf buf "%a: %a" LogtkSymbol.pp s LogtkType.pp ty in
  Printf.bprintf buf "{";
  LogtkUtil.pp_seq pp_pair buf (Seq.to_seq s);
  Printf.bprintf buf "}";
  ()

let to_string s =
  LogtkUtil.sprintf "%a" pp s

let fmt out s =
  let pp_pair out (s,ty) =
    Format.fprintf out "@[%a:@ %a@]" LogtkSymbol.fmt s LogtkType.fmt ty
  in
  Format.fprintf out "@[<hv>{";
  Sequence.pp_seq pp_pair out (Seq.to_seq s);
  Format.fprintf out "}@]";
  ()

  (* TODO
let bij =
  let open Bij in
  map
    ~inject:(fun signature -> Sequence.to_list (to_seq signature))
    ~extract:(fun l -> of_seq (Sequence.of_list l))
    (list_ (pair string_ LogtkSymbol.bij))
    *)

module TPTP = struct
  let is_bool signature s =
    let rec _is_bool_ty ty = match LogtkType.view ty with
      | LogtkType.Fun (ret, _)  when LogtkType.eq ret LogtkType.TPTP.o -> true
      | _ when LogtkType.eq ty LogtkType.TPTP.o -> true
      | LogtkType.Forall ty' -> _is_bool_ty ty'
      | _ -> false
    in _is_bool_ty (find_exn signature s)

  let is_not_bool signature s =
    not (is_bool signature s)

  (** {2 Pre-defined symbols} *)

  let x = LogtkType.var 0

  let ty_1_to_int = LogtkType.(forall [x] (LogtkType.TPTP.int <=. x))
  let ty2op = LogtkType.(forall [x] (x <== [x;x]))
  let ty2op_to_i = LogtkType.(TPTP.(int <== [int;int]))
  let ty2op_to_o = LogtkType.(forall [x] (TPTP.(o <== [x;x])))

  let arith_table =
    let open LogtkType.TPTP in
    [ LogtkSymbol.TPTP.Arith.less, ty2op_to_o
    ; LogtkSymbol.TPTP.Arith.lesseq, ty2op_to_o
    ; LogtkSymbol.TPTP.Arith.greater, ty2op_to_o
    ; LogtkSymbol.TPTP.Arith.greatereq, ty2op_to_o
    ; LogtkSymbol.TPTP.Arith.uminus, LogtkType.(forall [x] (x <=. x))
    ; LogtkSymbol.TPTP.Arith.sum, ty2op
    ; LogtkSymbol.TPTP.Arith.difference, ty2op
    ; LogtkSymbol.TPTP.Arith.product, ty2op
    ; LogtkSymbol.TPTP.Arith.quotient, ty2op
    ; LogtkSymbol.TPTP.Arith.quotient_e, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.quotient_f, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.quotient_t, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.remainder_e, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.remainder_f, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.remainder_t, ty2op_to_i
    ; LogtkSymbol.TPTP.Arith.floor, ty_1_to_int
    ; LogtkSymbol.TPTP.Arith.ceiling, ty_1_to_int
    ; LogtkSymbol.TPTP.Arith.round, ty_1_to_int
    ; LogtkSymbol.TPTP.Arith.truncate, ty_1_to_int
    ; LogtkSymbol.TPTP.Arith.to_int, LogtkType.(forall [x] (int <=. x))
    ; LogtkSymbol.TPTP.Arith.to_rat, LogtkType.(forall [x] (rat <=. x))
    ; LogtkSymbol.TPTP.Arith.is_int, LogtkType.(forall [x] (o <=. x))
    ; LogtkSymbol.TPTP.Arith.is_rat, LogtkType.(forall [x] (o <=. x))
    ]

  let table =
    let open LogtkType.TPTP in
    [ LogtkSymbol.Base.true_, o
    ; LogtkSymbol.Base.false_, o
    ; LogtkSymbol.Base.eq, LogtkType.(forall [x] (o <== [x;x]))
    ; LogtkSymbol.Base.neq, LogtkType.(forall [x] (o <== [x;x]))
    ; LogtkSymbol.Base.exists, LogtkType.(forall [x] (o <=. (o <=. x)))
    ; LogtkSymbol.Base.forall, LogtkType.(forall [x] (o <=. (o <=. x)))
    ; LogtkSymbol.Base.not_, LogtkType.(o <=. o)
    ; LogtkSymbol.Base.imply, LogtkType.(o <== [o;o])
    ; LogtkSymbol.Base.and_, LogtkType.(o <== [o;o])
    ; LogtkSymbol.Base.or_, LogtkType.(o <== [o;o])
    ; LogtkSymbol.Base.equiv, LogtkType.(o <== [o;o])
    ; LogtkSymbol.Base.xor, LogtkType.(o <== [o;o])
    ; LogtkSymbol.Base.wildcard, LogtkType.(forall [x] x)
    (* special symbols
    ; LogtkSymbol.db_symbol
    ; LogtkSymbol.split_symbol
    ; LogtkSymbol.const_symbol
    ; LogtkSymbol.num_symbol
    ; *)
    ]

  let base = of_list table

  let base_symbols = to_set base

  let is_base_symbol s = LogtkSymbol.Set.mem s base_symbols

  let pp_no_base buf s =
    pp buf (diff s base)

  (** {2 Arith} *)

  module Arith = struct
    let operators =
      Sequence.of_list arith_table
        |> Sequence.map fst
        |> LogtkSymbol.Set.of_seq

    let is_operator s = LogtkSymbol.Set.mem s operators

    let base = of_list arith_table
    let full = of_list (table @ arith_table)
  end
end
