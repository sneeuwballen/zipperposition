
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

(** {1 Terms without type, typically produced from AST} *)

module T = ScopedTerm

(** {2 First Order terms} *)

module FO = struct
  type t = T.t
  type term = t

  let kind = T.Kind.BasicFOTerm

  type view =
    | Var of int * Type.t
    | BVar of int * Type.t
    | Const of Symbol.t
    | App of t * t list

  let is_term t = match T.kind t with
    | T.Kind.BasicFOTerm -> true
    | _ -> false

  let ty t = match T.ty t with
    | T.HasType ty -> Type.of_term_exn ty
    | T.NoType -> assert false

  let of_term t = match T.kind t with
    | T.Kind.BasicFOTerm -> Some t
    | _ -> None

  let of_term_exn t = match T.kind t with
    | T.Kind.BasicFOTerm -> t
    | _ -> raise (Invalid_argument "Basic.FO.of_term_exn")

  (* high-level view of the term *)
  let view t =
    assert (is_term t);
    match T.view t, T.ty t with
    | T.Var i, T.HasType ty -> Var (i, Type.of_term_exn ty)
    | T.BVar i, T.HasType ty -> BVar (i, Type.of_term_exn ty)
    | T.Const s, _ -> Const s
    | T.App (f, l), _ -> App (f, l)
    | _, T.NoType -> assert false
    | T.Record _, _ (* TODO *)
    | T.Bind _, _ -> assert false

  let eq = T.eq
  let cmp = T.cmp
  let hash = T.hash

  let pp = T.pp
  let to_string = T.to_string
  let fmt = T.fmt

  let __no_type = Type.const Symbol.Base.wildcard

  let const ?(ty=__no_type) s = T.const ~kind ~ty:(ty:Type.t:>T.t) s
  let app ?(ty=__no_type) f l = T.app ~kind ~ty:(ty:Type.t:>T.t) f l
  let var ~ty i = T.var ~kind ~ty:(ty:Type.t:>T.t) i
  let bvar ~ty i = T.bvar ~kind ~ty:(ty:Type.t:>T.t) i

  let is_var t = match view t with | Var _ -> true | _ -> false
  let is_bvar t = match view t with | BVar _ -> true | _ -> false
  let is_app t = match view t with | App _ -> true | _ -> false
  let is_const t = match view t with | Const _ -> true | _ -> false

  let size = T.size

  exception ExpectedType of t

  let rec as_ty t = match T.view t with
    | T.Const s -> Type.const s
    | T.App (f, l) ->
        begin match T.view f with
        | T.Const s -> Type.app s (List.map as_ty l)
        | _ -> raise (ExpectedType t)
        end
    | T.Var i ->
        begin match T.ty t with
        | T.HasType ty when T.eq ty T.tType -> Type.var i
        | _ -> raise (ExpectedType t)
        end
    | T.BVar i -> Type.__bvar i
    | T.Bind _
    | T.Record _ -> assert false

  let rec of_ty ty = match Type.view ty with
    | Type.Var i -> var ~ty:Type.tType i
    | Type.BVar i -> bvar ~ty:Type.tType i
    | Type.App (s, l) -> app (const ~ty:Type.tType s) (List.map of_ty l)
    | Type.Forall _ -> raise (Invalid_argument "Basic.FO.of_ty: forall")
    | Type.Fun _ -> raise (Invalid_argument "Basic.FO.of_ty: fun")

  module Seq = struct
    let rec symbols t k = match T.view t with
      | T.Var _ | T.BVar _ -> ()
      | T.Const s -> k s
      | T.App (f, l) -> symbols f k; List.iter (fun t' -> symbols t' k) l
      | T.Bind _
      | T.Record _ -> assert false

    let rec vars t k = match T.view t with
      | T.Var _ -> k t
      | T.BVar _
      | T.Const _ -> ()
      | T.App (f, l) -> vars f k; List.iter (fun t' -> vars t' k) l
      | T.Bind _
      | T.Record _ -> assert false
  end

  let rec pp_depth ?(hooks=[]) depth buf t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec buf t =
      match view t with
      | BVar (i,_) -> Printf.bprintf buf "Y%d" (!depth - i - 1)
      | App (s, args) ->
          Printf.bprintf buf "%a(" pp_rec s;
          Util.pp_list pp_rec buf args;
          Buffer.add_string buf ")";
      | Const s -> Symbol.pp buf s
      | Var (i,ty) ->
        Printf.bprintf buf "X%d:%a" i Type.pp ty
    in
    pp_rec buf t

  type print_hook = int -> (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool

  module TPTP = struct
    let pp_depth ?(hooks=[]) depth buf t =
      let depth = ref depth in
      (* recursive printing *)
      let rec pp_rec buf t = match view t with
      | BVar (i,_) -> Printf.bprintf buf "Y%d" (!depth - i - 1)
      | Const s -> Symbol.TPTP.pp buf s
      | App (s, args) ->
        Printf.bprintf buf "%a(" pp_rec s;
        Util.pp_list pp_rec buf args;
        Buffer.add_string buf ")"
      | Var (i,_) -> Printf.bprintf buf "X%d" i
      in
      pp_rec buf t
  end

  module Set = Sequence.Set.Make(struct
    type t = term
    let compare = cmp
  end)
end

(** {2 First Order formulas} *)

module Form = Formula.Make(FO)
