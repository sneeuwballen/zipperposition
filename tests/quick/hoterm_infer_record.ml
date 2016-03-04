#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let f_ = ID.make "f"
let ty_f = 
  TT.Ty.(
    [ record_flatten ["x", int; "y", prop]
        ~rest:(Some (meta (Var.of_string ~ty:tType "r", ref None, `Generalize)))
    ; int
    ] ==> term
  )

let add_decls ctx =
  List.iter
    (fun (id, ty) ->
      TypeInference.Ctx.declare ctx id ty)
    [f_, ty_f];;

(* first inference *)

let ctx = TypeInference.Ctx.create ();;
add_decls ctx;;

let t =
  PT.app
    (PT.const "f")
      [ PT.record ["x", PT.of_int 42] ~rest:(Some "R");
        PT.var "X"];;

(* tough type inference *)
let t' =
  TypeInference.infer_exn ctx t;;

TypeInference.unify (TT.ty_exn t') TT.Ty.term;;

(* tougher type inference *)
let ctx = TypeInference.Ctx.create() in
add_decls ctx;;
let t'' = TypeInference.infer_exn ctx t;;
TypeInference.Ctx.exit_scope ctx;;

ok ();;
