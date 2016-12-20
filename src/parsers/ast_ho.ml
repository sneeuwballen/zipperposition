
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Very Simple AST}

    AST that holds Horn Clauses and type declarations, nothing more. *)

open Libzipperposition

module PT = STerm

type term = PT.t

type t =
  | Clause of term * term list
  | Type of string * term

type location = ParseLocation.t

let pp out t = match t with
  | Clause (head, []) ->
      Format.fprintf out "@[%a@]." PT.pp head
  | Clause (head, body) ->
      Format.fprintf out "@[<hv2>@[%a@]@ <-@ %a.@]"
        PT.pp head (Util.pp_list ~sep:", " PT.pp) body
  | Type (s, ty) ->
      Format.fprintf out "@[val @[<hv>%s@ : %a@]@]" s PT.pp ty

let to_string = CCFormat.to_string pp

module Seq = struct
  let terms decl k = match decl with
    | Type (_, ty) -> k ty
    | Clause (head, l) ->
        k head; List.iter k l

  let vars decl =
    terms decl
    |> Sequence.flat_map PT.Seq.vars
end

(* XXX: ad-hoc infix symbol constructor *)
let app_infix ?loc o a b =
  let head = PT.const ?loc o in
  match o with
  | "-->" -> PT.app ?loc head [PT.wildcard; a; b]  (* polymorphic! *)
  | _ -> PT.app ?loc head [a; b] (* default *)
