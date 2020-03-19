open Logtk
open Libzipperposition

module L = Literal
module Ls = Literals
module T = Term

let enabled = ref false

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

  let fold_lits c = 
    Ls.fold_eqn 
      ~both:true ~ord:(E.Ctx.ord ()) ~eligible:(C.Eligible.res c) 
      (C.lits c)

  let proof ~name c =
    Proof.Step.inference 
      ~rule:(Proof.Rule.mk name) [C.proof_parent c]

  let mk_and ~rule_name and_args c lit_idx =
    let lits = CCArray.except_idx (C.lits c) lit_idx in
    let proof = proof rule_name c in
    List.map (fun t -> 
      C.create ~penalty:(C.penalty c) ~trail:(C.trail c) 
        (L.mk_true t :: lits) proof) and_args
  
  let mk_or ~rule_name or_args c lit_idx =
    let lits = 
      (List.map L.mk_true or_args) @
      (CCArray.except_idx (C.lits c) lit_idx) in
    let proof = proof rule_name c in
    [C.create ~penalty:(C.penalty c) ~trail:(C.trail c) lits proof]

  let lazy_clausify c =
    fold_lits c
    |> Iter.flat_map_l ( fun (lhs, rhs, sign, pos) -> 
      let i,_ = Ls.Pos.cut pos in
      if T.equal rhs T.true_ then (
        match T.view lhs with 
        | T.AppBuiltin(And, l) when List.length l >= 2 ->
          let rule_name = "lazy_cnf_and" in
          if sign then mk_and l c i ~rule_name
          else mk_or (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Or, l) when List.length l >= 2 ->
          let rule_name = "lazy_cnf_or" in
          if sign then mk_or l c i ~rule_name
          else mk_and (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Imply, [a;b]) ->
          let rule_name = "lazy_cnf_imply" in
          if sign then mk_or [T.Form.not_ a; b] c i ~rule_name
          else mk_and [a; T.Form.not_ b] c i ~rule_name
        | T.AppBuiltin((Equiv|Xor) as hd, [a;b]) ->
          let rule_name = "lazy_cnf_equiv" in
          let hd = if sign then hd else (if hd = Equiv then Xor else Equiv) in
          if hd = Equiv then (
            let not_a_or_b = T.Form.or_ (T.Form.not_ a) b  in
            let a_or_not_b = T.Form.or_ a (T.Form.not_ b)  in
            mk_and [not_a_or_b; a_or_not_b] c i ~rule_name
          ) else (
            let a_or_b = T.Form.or_ a b  in
            let not_a_or_not_b = T.Form.or_ (T.Form.not_ a) (T.Form.not_ b) in
            mk_and [a_or_b; not_a_or_not_b] c i ~rule_name
          )
        | T.AppBuiltin((ForallConst|ExistsConst) as hd, [f]) ->
          let rule_name = 
            CCFormat.sprintf "lazy_cnf_%s" 
              (if hd = ForallConst then "forall" else "exists") in
          let free_vars = C.Seq.vars c in
          let var_id = T.Seq.max_var free_vars + 1 in
          let var_tys, body =  T.open_fun f in
          assert(CCList.length var_tys = 1);
          let var_ty = List.hd var_tys in
          let hd, f =
            if sign then hd,f
            else ((if hd=ForallConst then ExistsConst else ForallConst),
                  T.fun_ var_ty (T.Form.not_ body)) in
          let subst_term =
            if hd = ForallConst then (
              T.var @@ HVar.make ~ty:var_ty var_id
            ) else (
              let free_vars_l = 
                  T.VarSet.to_list (T.VarSet.of_seq free_vars) in
              let (id,ty), t = T.mk_fresh_skolem ~prefix:"sk" free_vars_l var_ty in
              E.Ctx.declare id ty;
              t) in
          let res = Lambda.snf @@ T.app f [subst_term] in
          assert(Type.is_prop (T.ty res));
          mk_or ~rule_name [res] c i
        | T.AppBuiltin(Not, _) -> assert false
        | _ -> []
      ) else []) 
    |> Iter.to_list
    

  let setup () = ()
end