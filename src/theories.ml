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

(** {6 Specifications of Built-in Theories} *)

open Logtk

module T = FOTerm
module F = Formula.FO

type scope = Substs.scope
type term = FOTerm.t

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = {
    sym : Symbol.t;
    ty : Type.t;
  }

end

(** {2 Total Ordering} *)

module TotalOrder = struct
  type t = {
    less : Symbol.t;
    lesseq : Symbol.t;
    ty : Type.t;
  } (** A single instance of total ordering *)

  type lit = {
    left : term;
    right : term;
    tyargs : Type.t list;
    strict : bool;
    order : t;
  } (** A literal is an atomic inequality. [strict] is [true] iff the
      literal is a strict inequality, and the ordering itself
      is also provided. *)

  let eq i1 i2 =
    Symbol.eq i1.less i2.less
    && Symbol.eq i1.lesseq i2.lesseq
    && Type.eq i1.ty i2.ty

  let hash i =
    Hash.hash_int3 (Symbol.hash i.less) (Symbol.hash i.lesseq) (Type.hash i.ty)

  let map f lit =
    { lit with left=f lit.left; right=f lit.right; }

  let apply_subst ~renaming subst lit s_lit =
    let tyargs = List.map
      (fun ty -> Substs.Ty.apply ~renaming subst ty s_lit)
      lit.tyargs
    in
    let left = Substs.FO.apply ~renaming subst lit.left s_lit in
    let right = Substs.FO.apply ~renaming subst lit.right s_lit in
    { lit with left; right; tyargs; }

  let less_const instance = T.const ~ty:instance.ty instance.less
  let lesseq_const instance = T.const ~ty:instance.ty instance.lesseq

  let pp buf i =
    Printf.bprintf buf "total_order{%a, %a}" Symbol.pp i.less Symbol.pp i.lesseq

  let to_string i = Util.on_buffer pp i

  let fmt fmt i = Format.pp_print_string fmt (to_string i)
end
