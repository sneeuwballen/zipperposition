
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

(** {1 Arbitrary Basic Terms} *)

open Logtk
open QCheck

type 'a arbitrary = 'a QCheck.Arbitrary.t


module ArbitraryBasic = struct
  module UF = Basic.Form
  module AT = ArTerm.ArbitraryBasic

  let atom =
    Arbitrary.(choose
      [ lift UF.atom AT.pred
      ; lift (fun t -> UF.mk_not (UF.atom t)) AT.pred
      ; lift2 UF.mk_eq AT.default AT.default
      ; lift2 UF.mk_neq AT.default AT.default
      ; among [ UF.mk_true; UF.mk_false ]
      ])

  let clause = Arbitrary.(list atom)

  let default =
    Arbitrary.(
      let f = fix ~max:5 ~base:atom
        (fun sub_f -> choose
          [ lift UF.mk_or (list sub_f)
          ; lift UF.mk_and (list sub_f)
          ; lift2 UF.mk_equiv sub_f sub_f
          ; lift2 UF.mk_imply sub_f sub_f
          ; lift UF.mk_not sub_f
          ; lift UF.close_forall sub_f
          ; lift UF.close_exists sub_f
          ])
      in
      f)
end

let atom =
  Arbitrary.(
    ArbitraryBasic.atom >>= fun f ->
    let ctx = TypeInference.Ctx.create () in
    return (TypeInference.FO.convert_form ~ctx f)
  )

let clause =
  Arbitrary.(
    list ArbitraryBasic.atom >>= fun lits ->
    let ctx = TypeInference.Ctx.create () in
    return (TypeInference.FO.convert_clause ~ctx lits)
  )

let default = 
  Arbitrary.(
    ArbitraryBasic.default >>= fun f ->
    let ctx = TypeInference.Ctx.create () in
    return (TypeInference.FO.convert_form ~ctx f)
  )
