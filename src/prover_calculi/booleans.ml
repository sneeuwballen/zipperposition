
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

let _axioms_enabled = ref false

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let (=~),(/~) = Literal.mk_eq, Literal.mk_neq
  let (@:) = T.app_builtin ~ty:Type.prop
  let no a = a /~ T.true_
  let yes a = a =~ T.true_
  let imply a b = Builtin.Imply @: [a;b]
  let const_true p = T.fun_ (List.hd @@ fst @@ Type.open_fun (T.ty p)) T.true_

  let true_not_false = [no T.false_]
  let true_or_false a = [yes a; a =~ T.false_]
  let imp_true1 a b = [yes a; yes(imply a b)]
  let imp_true2 a b = [no b; yes(imply a b)]
  let imp_false a b = [no(imply a b); no a; yes b]
  let all_true p = [p /~ const_true p; yes(Builtin.ForallConst@:[p])]
  let all_false p = [no(Builtin.ForallConst@:[p]); p =~ const_true p]
  let eq_true x y = [x/~y; yes(Builtin.Eq@:[x;y])]
  let eq_false x y = [no(Builtin.Eq@:[x;y]); x=~y]

  let not = [Builtin.Not @: [] =~ T.fun_ Type.prop (imply (T.bvar ~ty:Type.prop 0) T.false_)]
  let exists t = 
    let t2bool = Type.arrow [t] Type.prop in
    [Builtin.ExistsConst @:[] =~ T.fun_ t2bool
      (Builtin.Not @:[Builtin.ForallConst @:[T.fun_ t (Builtin.Not @:[T.app (T.bvar t2bool 1) [T.bvar t 0]])]])]
  
  let as_clause c = Env.C.create ~penalty:1 ~trail:Trail.empty c Proof.Step.trivial

  let create_clauses () = 
    let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let a = T.var (HVar.make ~ty:Type.prop 0) in
    let b = T.var (HVar.make ~ty:Type.prop 1) in
    let p = T.var (HVar.make ~ty:(Type.arrow [alpha] Type.prop) 1) in
    let x = T.var (HVar.make ~ty:alpha 1) in
    let y = T.var (HVar.make ~ty:alpha 1) in
    let cls = [
      true_not_false; true_or_false a; imp_true1 a b;
      imp_true2 a b; imp_false a b; all_true p; 
      all_false p  ; eq_true x y  ; eq_false x y; 
      not          ; exists alpha 
    ] in
    Iter.of_list (List.map as_clause cls)

  let setup () =
    Env.add_passive (create_clauses () );
    ()
end


let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "bool";
      env_actions=[register];
  }

let () =
  Options.add_opts
    [ "boolean-axioms", Arg.Bool (fun b -> _axioms_enabled := b), 
      " enable/disable boolean axioms"  ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Extensions.register extension