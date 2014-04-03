
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

(** {1 Skolem symbols} *)

module ST = ScopedTerm
module T = FOTerm
module F = Formula.FO
module S = Substs

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

type definition = {
  form : Formula.FO.t;
  proxy : Formula.FO.t;
  polarity : polarity;
}

type ctx = {
  sc_prefix : string;
  sc_ty_prop : Type.t;
  mutable sc_gensym : int;              (* new symbols *)
  mutable sc_var_index : int;           (* fresh universal vars *)
  mutable sc_fcache : (F.t * F.t) list; (* cache for skolemization *)
  mutable sc_defs : definition F.Map.t; (* formula -> definition of formula *)
  mutable sc_new_defs : definition list; (* "new" definitions *)
  mutable sc_signature : Signature.t;
}

(* TODO: use a term index for the cache? *)

let create ?(ty_prop=Type.TPTP.o) ?(prefix="logtk_sk__") signature =
  let ctx = {
    sc_prefix=prefix;
    sc_ty_prop=ty_prop;
    sc_gensym = 0;
    sc_var_index = 0;
    sc_fcache = [];
    sc_defs = F.Map.empty;
    sc_new_defs = [];
    sc_signature = signature;
  } in
  ctx

let to_signature ctx = ctx.sc_signature

let fresh_sym ~ctx ~ty =
  let n = ctx.sc_gensym in
  ctx.sc_gensym <- n+1;
  let s = Symbol.of_string (ctx.sc_prefix ^ string_of_int n) in
  (* declare type of the new symbol *)
  ctx.sc_signature <- Signature.declare ctx.sc_signature s ty;
  Util.debug 5 "new skolem symbol %a with type %a" Symbol.pp s Type.pp ty;
  s

(* update varindex in [ctx] so that it won't get captured in [t] *)
let update_var ~ctx t =
  let m = max ctx.sc_var_index ((T.Seq.vars t |> T.Seq.max_var) + 1) in
  ctx.sc_var_index <- m

let clear_var ~ctx =
  ctx.sc_var_index <- 0

let fresh_var ~ctx =
  let n = ctx.sc_var_index in
  ctx.sc_var_index <- n + 1;
  n

exception FoundFormVariant of F.t * F.t * S.t

let skolem_form ~ctx ~ty f =
  let vars = F.Seq.vars f |> T.Seq.add_set T.Set.empty |> T.Set.elements in
  (* find a variant of [f] *)
  try
    List.iter
      (fun (f', new_f') ->
        Util.debug 5 "check variant %a and %a" F.pp f F.pp f';
        match Unif.Form.variant f' 1 f 0 with
        | KList.Nil -> ()
        | KList.Cons (subst, _) ->
          raise (FoundFormVariant (f', new_f', subst)))
      ctx.sc_fcache;
    (* fresh symbol with the proper type *)
    let ty_of_vars = List.map T.ty vars in
    let ty = Type.(ty <== ty_of_vars) in
    (* close the type w.r.t its type variables *)
    let tyargs = Type.vars ty in
    let ty = Type.forall tyargs ty in
    let const = T.const ~ty (fresh_sym ~ctx ~ty) in
    let skolem_term = T.app_full const tyargs vars in
    (* replace variable by skolem t*)
    let env = DBEnv.singleton (skolem_term : T.t :> ST.t) in
    let new_f =
        ST.DB.eval env (f : F.t :> ST.t)
      |> ST.DB.unshift 1
      |> (fun t -> match F.of_term t with
          | None -> assert (T.is_term t); F.Base.atom (T.of_term_exn t)
          | Some f -> f)
    in
    ctx.sc_fcache <- (f, new_f) :: ctx.sc_fcache;
    Util.debug 5 "skolemize %a using new term %a" F.pp f T.pp skolem_term;
    new_f
  with FoundFormVariant(f',new_f',subst) ->
    Util.debug 5 "form %a is variant of %a under %a" F.pp f' F.pp f S.pp subst;
    let renaming = S.Renaming.create () in
    let new_f = S.Form.apply ~renaming subst new_f' 1 in
    new_f

(** {2 Definitions} *)

let has_definition ~ctx f =
  F.Map.mem f ctx.sc_defs

let get_definition ~ctx ~polarity f =
  let ty_prop = ctx.sc_ty_prop in
  try
    let def = F.Map.find f ctx.sc_defs in
    assert (T.Set.equal (F.free_vars_set f) (F.free_vars_set def.proxy));
    let def' = match polarity, def.polarity with
      | `Pos, `Pos
      | `Neg, `Neg
      | `Both, `Both -> def
      | _ -> { def with polarity=`Both; }
    in
    (* same name, no need to introduce a new def! But we need to remember
     * the polarity *)
    ctx.sc_defs <- F.Map.add f def ctx.sc_defs;
    def.proxy
  with Not_found ->
    (* ok, we have to introduce a new name. It will have all free variables
       (bound in a surrouding scope or not) as arguments. *)
    let vars = F.free_vars f in
    let bvars = F.de_bruijn_set f |> T.Set.elements in
    let all_vars = vars @ bvars in
    let ty_of_vars = List.map T.ty all_vars in
    let ty = Type.(ty_prop <== ty_of_vars) in
    let const = T.const ~ty (fresh_sym ~ctx ~ty) in
    let p = F.Base.atom (T.app const all_vars) in
    (* introduce new name for [f] *)
    Util.debug 5 "define formula %a with %a" F.pp f F.pp p;
    let def = {form=f; proxy=p; polarity; } in
    ctx.sc_defs <- F.Map.add f def ctx.sc_defs;
    ctx.sc_new_defs <- def :: ctx.sc_new_defs;
    p

let all_definitions ~ctx =
  F.Map.to_seq ctx.sc_defs
    |> Sequence.map snd

let pop_new_definitions ~ctx =
  let l = ctx.sc_new_defs in
  ctx.sc_new_defs <- [];
  l

let has_new_definitions ~ctx = match ctx.sc_new_defs with
  | [] -> false
  | _::_ -> true

let skolem_ho ~ctx ~ty f = failwith "Skolem_ho: not implemented"
