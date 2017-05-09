
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module T = FOTerm

type term = FOTerm.t
let section = Util.Section.make ~parent:Const.section "fool"

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

  (* replace [sub] by [true/false] in [c], obtaining a new clause *)
  let fool_param_sign ~sub sign c =
    let lits =
      C.lits c
      |> Literals.map (T.replace ~old:sub ~by:(if sign then T.true_ else T.false_))
      |> Array.to_list
    in
    let new_lit = Literal.mk_eq sub (if sign then T.false_ else T.true_) in
    let proof =
      Proof.Step.inference [C.proof_parent c]
        ~rule:(Proof.Rule.mk"fool_param")
    in
    let new_c =
      C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
        (new_lit :: lits) proof
    in
    Util.debugf ~section 5 "... deduce `@[%a@]`" (fun k->k C.pp new_c);
    new_c

  let fool_param c : C.t list =
    let sub_terms =
      C.Seq.terms c
      |> Sequence.flat_map
        (fun t ->
           T.Seq.subterms_depth t
           |> Sequence.filter_map (fun (t,d) -> if d>0  then Some t else None))
      |> Sequence.filter
        (fun t ->
           Type.is_prop (T.ty t)
           &&
           begin match T.view t with
             | T.Const _ | T.App _ -> true
             | T.AppBuiltin ((Builtin.True | Builtin.False), _) -> false
             | T.AppBuiltin (_, _) -> true
             | T.Var _ | T.DB _ -> false
           end)
      |> T.Set.of_seq
    in
    if not (T.Set.is_empty sub_terms) then (
      Util.debugf ~section 5
        "@[<2>in clause `@[%a@]`@ possible subterms are [@[<hv>%a@]]@]"
        (fun k->k C.pp c (T.Set.pp ~sep:"," T.pp) sub_terms);
    );
    begin
      T.Set.to_seq sub_terms
      |> Sequence.flat_map_l
        (fun sub -> [fool_param_sign ~sub true c; fool_param_sign ~sub false c])
      |> Sequence.to_rev_list
    end

  let setup () =
    Util.debug ~section 1 "setup fool rules";
    Env.add_unary_inf "fool_param" fool_param;
    ()
end

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with Extensions.
                         name = "fool";
                         env_actions=[register];
  }

let () = Extensions.register extension
