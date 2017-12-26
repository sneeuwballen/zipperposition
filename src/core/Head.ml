
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Head} *)

type var = Type.t HVar.t

module T = Term

type t =
  | I of ID.t
  | B of Builtin.t
  | V of var

let pp out = function
  | I id -> ID.pp out id
  | B b -> Builtin.pp out b
  | V x -> HVar.pp out x

let term_to_head s =
  match T.view s with
    | T.App (f,_) ->
      begin match T.view f with
        | T.Const fid -> Some (I fid)
        | T.Var x ->     Some (V x)
        | _ -> None
      end
    | T.AppBuiltin (fid,_) -> Some (B fid)
    | T.Const fid -> Some (I fid)
    | T.Var x ->     Some (V x)
    | _ -> None

let term_to_args s =
  let ignore_ty_args = List.filter (fun u -> not (Type.is_tType (T.ty u))) in
  match T.view s with
    | T.App (_,ss) -> ignore_ty_args ss
    | T.AppBuiltin (_,ss) -> ignore_ty_args ss
    | _ -> []

let to_string = CCFormat.to_string pp
