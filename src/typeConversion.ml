
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

(** {1 Conversion between type representations} *)

module BTy = Basic.Ty

type ctx = {
  mutable n : int;
  tbl : (string, Type.t) Hashtbl.t;
}

let create_ctx () = {
  tbl = Hashtbl.create 5;
  n = 1;
}

let clear ctx =
  ctx.n <- 1;
  Hashtbl.clear ctx.tbl;
  ()

let of_basic ?(ctx=create_ctx ()) ty =
  let rec convert ty = match ty with
  | BTy.Var s ->
    begin try
      Hashtbl.find ctx.tbl s
    with Not_found ->
      let v = Type.var ctx.n in
      ctx.n <- ctx.n + 1;
      Hashtbl.add ctx.tbl s v;
      v
    end
  | BTy.App (s, l) ->
    let l = List.map convert l in
    Type.app s l
  | BTy.Fun (ret, l) ->
    let ret = convert ret in
    let l = List.map convert l in
    Type.mk_fun ret l
  | BTy.Forall (vars, ty) ->
    let vars' = List.map convert vars in
    let ty' = convert ty in
    Type.forall vars' ty'
  in
  convert ty

let to_basic ty =
  let rec to_basic depth ty = match ty.Type.ty with
  | Type.Var i -> BTy.var (Util.sprintf "T%d" i)
  | Type.BVar i -> BTy.var (Util.sprintf "Tb%d" (depth-i-1))
  | Type.App (s, l) -> BTy.app s (List.map (to_basic depth) l)
  | Type.Fun (ret, l) -> BTy.mk_fun (to_basic depth ret) (List.map (to_basic depth) l)
  | Type.Forall ty' ->
    BTy.forall [BTy.var (Util.sprintf "Tb%d" depth)] (to_basic (depth+1) ty')
  in
  to_basic 0 ty
