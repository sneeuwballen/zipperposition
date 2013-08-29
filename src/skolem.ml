
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

module T = Term

type ctx = {
  sc_gensym : Symbol.Gensym.t;                  (* new symbols *)
  mutable sc_var_index : int;                   (* fresh universal vars *)
  mutable sc_cache : (Term.t * Term.t) list;  (* term -> skolem symbol cache *)
}

(* TODO: use a term index for the cache? *)

let create ?(prefix="logtk_sk__") () =
  let ctx = {
    sc_gensym = Symbol.Gensym.create ~prefix ();
    sc_var_index = 0;
    sc_cache = [];
  } in
  ctx

let fresh_sym ~ctx =
  Symbol.Gensym.new_ ctx.sc_gensym

(* update varindex in [ctx] so that it won't get captured in [t] *)
let update_var ~ctx t =
  ctx.sc_var_index <- max ctx.sc_var_index (T.max_var (T.vars t) + 1)

let fresh_var ~ctx =
  let n = ctx.sc_var_index in
  ctx.sc_var_index <- n + 1;
  n

exception FoundVariant of Term.t * Term.t * Substs.t

let skolem_term ~ctx t =
  let vars = T.vars_prefix_order t in
  let scope = T.max_var vars + 1 in
  (* find the skolemized normalized term *)
  try
    List.iter
      (fun (t', new_t') ->
        try
          let subst = Unif.variant t' scope t 0  in
          raise (FoundVariant (t', new_t', subst))
        with Unif.Fail ->
          ())
      ctx.sc_cache;
    (* not found, use a fresh symbol *)
    let symb = Symbol.Gensym.new_ ctx.sc_gensym in
    (* replace the existential variable by [skolem_term] in [t] *)
    let skolem_term = T.mk_node symb vars in
    let new_t = T.db_unlift (T.db_replace t skolem_term) in
    (* update cache with new symbol *)
    ctx.sc_cache <- (t, new_t) :: ctx.sc_cache;
    new_t
  with FoundVariant (t',new_t',subst) ->
    let new_t = Substs.apply_subst subst new_t' scope in
    new_t
