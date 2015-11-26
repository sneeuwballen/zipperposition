#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let ty = Type.(forall [var 0] (var 0 <== [var 0; record ["x",var 0] ~rest:None]));;
let ty2 = Type.(const ~< "a" <== [const ~< "a"; record ["x", const ~< "a"] ~rest:None]);;
let s = Unif.Ty.unification Type.(apply ty (var 1)) 0 ty2 1;;
let ty3 = Substs.Ty.apply ~renaming:(Substs.Renaming.create ()) s Type.(apply ty (var 1)) 0;;
Util.debug 1 "types: %a (%d) and %a (%d)\n"
  (fun k->k
  Type.pp ty2 (ST.hash (ty2:Type.t:>ST.t))
  Type.pp ty3 (ST.hash (ty3:Type.t:>ST.t)));;
assert (Type.eq ty2 ty3);;

ok ();;

