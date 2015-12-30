#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let a_ = Type.const (ID.make "a")

let ty = Type.(forall ([bvar 0; record ["x", bvar 0] ~rest:None] ==> bvar 0));;
let ty2 = Type.([a_; record ["x", a_] ~rest:None] ==> a_);;
let s = Unif.Ty.unification (Type.(apply ty [var_of_int 1]),0) (ty2,1);;
let ty3 =
  Substs.Ty.apply ~renaming:(Substs.Renaming.create ()) s (Type.(apply ty [var_of_int 1]),0);;
Util.debugf 1 "types: %a and %a"
  (fun k->k Type.pp ty2 Type.pp ty3);;
assert (Type.equal ty2 ty3);;

ok ();;

