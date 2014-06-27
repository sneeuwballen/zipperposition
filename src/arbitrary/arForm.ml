
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
type form = Formula.FO.t

module PT = struct
  module PT = PrologTerm
  module AT = ArTerm.PT

  let atom =
    Arbitrary.(choose
      [ AT.pred
      ; lift PT.TPTP.not_ AT.pred
      ; lift2 PT.TPTP.eq AT.default AT.default
      ; lift2 PT.TPTP.neq AT.default AT.default
      ; among [ PT.TPTP.true_; PT.TPTP.false_ ]
      ])

  let default =
    Arbitrary.(
      let f = fix ~max:3 ~base:atom
        (fun sub_f -> choose
          [ lift PT.TPTP.or_ (list sub_f)
          ; lift PT.TPTP.and_ (list sub_f)
          ; lift2 PT.TPTP.equiv sub_f sub_f
          ; lift2 PT.TPTP.imply sub_f sub_f
          ; lift PT.TPTP.not_ sub_f
          ; lift (PT.close_all Symbol.Base.forall) sub_f
          ; lift (PT.close_all Symbol.Base.exists) sub_f
          ])
      in
      f)
end

let atom =
  Arbitrary.(
    PT.atom >>= fun f ->
    let ctx = TypeInference.Ctx.create Signature.empty in
    return (TypeInference.FO.convert_form_exn ~ctx f)
  )

let clause =
  Arbitrary.(
    int 3 >>= fun len ->
    list_repeat len PT.atom >>= fun lits ->
    let ctx = TypeInference.Ctx.create Signature.empty in
    return (TypeInference.FO.convert_clause_exn ~ctx lits)
  )

let default =
  Arbitrary.(
    PT.default >>= fun f ->
    let ctx = TypeInference.Ctx.create Signature.empty in
    return (TypeInference.FO.convert_form_exn ~ctx f)
  )
