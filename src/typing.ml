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

(** {1 Typing} *)

module T = Term

type ty = Term.t


(** {2 Helpers} *)

(* constant *)
let is_atom ty =
  assert (T.is_type ty);
  T.is_const ty

(* FO type: either a simple function type, or a variable *)
let is_fo ty =
  assert (T.is_type ty);
  match ty.T.term with
  | T.Node (s, [{T.term=T.Node (s', l)}; r])
    when Symbol.eq s Symbol.arrow_symbol
    && Symbol.eq s' Symbol.tuple_symbol ->
    is_atom r && List.for_all (fun t -> is_atom t || T.is_var t) l
  | T.Var _ -> true
  | T.At (_, _) -> false
  | T.Node _ -> false
  | T.Bind _ | T.BoundVar _ -> assert false

(* simple type: function with atom as arguments *)
let is_simple_typed ty =
  assert (T.is_type ty);
  match ty.T.term with
  | T.Node (s, [{T.term=T.Node (s', l)}; r])
    when Symbol.eq s Symbol.arrow_symbol
    && Symbol.eq s' Symbol.tuple_symbol ->
    is_atom r && List.for_all is_atom l
  | T.At (_, _) -> false
  | T.Var _ -> false
  | T.Node _ -> false
  | T.Bind _ | T.BoundVar _ -> assert false

let arity ty =
  assert (T.is_type ty);
  match ty.T.term with
  | T.Node (s, [{T.term=T.Node (s', l)}; _])
    when Symbol.eq s Symbol.arrow_symbol
    && Symbol.eq s' Symbol.tuple_symbol -> List.length l
  | T.Node (s, [_; _]) when Symbol.eq s Symbol.arrow_symbol -> 1
  | _ -> 0

(** {2 Signatures} *)

module Signature = struct
  module SMap = Symbol.SMapSeq

  type t = ty SMap.t
    (** A signature maps symbols to types *)

  let empty = SMap.empty

  let mem signature s = SMap.mem s signature

  let declare signature s ty =
    (if SMap.mem s signature
      then raise (Invalid_argument "symbol already declared"));
    SMap.add s ty signature

  let find signature s =
    SMap.find s signature

  let cardinal signature = SMap.cardinal signature

  let arity signature s =
    let ty = find signature s in
    arity ty

  let to_seq s = SMap.to_seq s
  let of_seq seq = SMap.of_seq seq

  let is_fo signature =
    Sequence.for_all is_fo (Sequence.map snd (to_seq signature))

  let is_simple_typed signature =
    is_fo signature &&
    Sequence.for_all is_simple_typed (Sequence.map snd (to_seq signature))

  let pp buf s =
    let pp_pair buf (s,ty) = Printf.bprintf buf "%a: %a" Symbol.pp s Term.pp ty in
    Printf.bprintf buf "{";
    Util.pp_seq pp_pair buf (to_seq s);
    Printf.bprintf buf "}";
    ()

  let to_string s =
    Util.sprintf "%a" pp s

  let fmt fmt s =
    Format.pp_print_string fmt (to_string s)
end

(** {2 Type inference} *)

type env = {
  mutable signature : Signature.t;
  mutable subst : Substs.t;
  mutable max_var : int;  (* to generate fresh vars *)
}

let create_env () =
  { signature = Signature.empty;
    subst = Substs.empty;
    max_var = 0;
  }

let copy_env env =
  { env with max_var = env.max_var; }

let sig_of_env env = env.signature

(* fresh type variable from the environment *)
let fresh_ty_var env =
  let v = T.ty_var env.max_var in
  env.max_var <- env.max_var + 1;
  v

(* replace bound type variables *)
let normalize_ty env ty =
  Substs.apply_subst ~recursive:true env.subst ty 0

(* rename variables with fresh variables *)
let rename_vars env ty =
  let ty = normalize_ty env ty in
  (* still free variables *)
  let vars = T.vars ty in
  match vars with
  | [] -> ty
  | _ ->
    let subst = List.map (fun v -> v, 0, fresh_ty_var env, 0) vars in
    let subst = Substs.of_list subst in
    Substs.apply_subst ~recursive:false subst ty 0

exception TypeError

let infer signature t =
  failwith "infer: not implemented"

let infer_update signature t =
  failwith "infer_update: not implemented"
