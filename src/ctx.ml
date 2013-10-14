
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Basic context for literals, clauses...} *)

open Logtk

module S = Substs.FO

type t = {
  mutable ord : Ordering.t;           (** current ordering on terms *)
  mutable select : Selection.t;       (** selection function for literals *)
  mutable skolem : Skolem.ctx;        (** Context for skolem symbols *)
  mutable signature : Signature.t;    (** Signature *)
  mutable complete : bool;            (** Completeness preserved? *)
  renaming : S.Renaming.t;       (** Renaming *)
  ac : Theories.AC.t;                 (** AC symbols *)
  total_order : Theories.TotalOrder.t;(** Total ordering *)
}

let create ?(ord=Ordering.none) ?(select=Selection.no_select) ~signature () =
  let ctx = {
    ord=ord;
    select=select;
    skolem = Skolem.create ~prefix:"zsk" ();
    signature;
    complete=true;
    renaming = S.Renaming.create 13;
    ac = Theories.AC.create ();
    total_order = Theories.TotalOrder.create ();
  } in
  ctx

let ord ~ctx = ctx.ord

let compare ~ctx t1 t2 = Ordering.compare ctx.ord t1 t2

let select ~ctx lits = ctx.select lits

let skolem_ctx ~ctx = ctx.skolem

let signature ~ctx = ctx.signature

let lost_completeness ~ctx =
  if ctx.complete then Util.debug 1 "completeness is lost";
  ctx.complete <- false

let is_completeness_preserved ~ctx = ctx.complete

let add_signature ~ctx signature =
  ctx.signature <- Signature.merge ctx.signature signature;
  ctx.ord <- Ordering.add_signature ctx.ord ctx.signature;
  ()

let ac ~ctx = ctx.ac

let total_order ~ctx = ctx.total_order

let renaming_clear ~ctx =
  let r = ctx.renaming in
  S.Renaming.clear r;
  r

let add_ac ~ctx ?proof s = Theories.AC.add ~spec:ctx.ac ?proof s

let add_order ~ctx ?proof ~less ~lesseq =
  let spec = ctx.total_order in
  try
    Theories.TotalOrder.find ~spec less
  with Not_found ->
    let instance = Theories.TotalOrder.add ~spec ?proof ~less ~lesseq in
    (* declare missing symbols, if any; also take care that
      less and lesseq have the same type, which is, a binary predicate *)
    let tyctx = TypeInference.Ctx.of_signature ctx.signature in
    let ty_less = TypeInference.Ctx.type_of_fun tyctx less in
    let ty_lesseq = TypeInference.Ctx.type_of_fun tyctx lesseq in
    TypeInference.Ctx.unify tyctx ty_less Type.(mk_fun o [var "A"; var "A"]);
    TypeInference.Ctx.unify tyctx ty_less ty_lesseq;
    ctx.signature <- TypeInference.Ctx.to_signature tyctx;
    instance

(** {2 Type inference} *)

let tyctx ~ctx = TypeInference.Ctx.of_signature ctx.signature

let declare ~ctx symb ty =
  ctx.signature <- Signature.declare ctx.signature symb ty

let constrain_term_type ~ctx t ty =
  let tyctx = TypeInference.Ctx.of_signature ctx.signature in
  TypeInference.FO.constrain_term_type tyctx t ty;
  ctx.signature <- TypeInference.Ctx.to_signature tyctx

let constrain_term_term ~ctx t1 t2 =
  let tyctx = TypeInference.Ctx.of_signature ctx.signature in
  TypeInference.FO.constrain_term_term tyctx t1 t2;
  ctx.signature <- TypeInference.Ctx.to_signature tyctx
  
let infer_type ~ctx t =
  let tyctx = TypeInference.Ctx.of_signature ctx.signature in
  TypeInference.FO.infer tyctx t

let check_term_type ~ctx t ty =
  let tyctx = TypeInference.Ctx.of_signature ctx.signature in
  TypeInference.FO.check_term_type tyctx t ty

let check_term_term ~ctx t1 t2 =
  let tyctx = TypeInference.Ctx.of_signature ctx.signature in
  TypeInference.FO.check_term_term tyctx t1 t2
