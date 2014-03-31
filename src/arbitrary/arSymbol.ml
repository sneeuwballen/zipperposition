
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

(** {1 Arbitrary generation of symbols} *)

open Logtk
open QCheck

type 'a arbitrary = 'a QCheck.Arbitrary.t

let base = Arbitrary.(among (Symbol.TPTP.connectives |> Symbol.Set.elements))

let int = Arbitrary.(lift Symbol.of_int (~-50 -- 50))

let rat = Arbitrary.(lift2 Symbol.of_rat (~-50 -- 50) (1 -- 80))

let arith = Arbitrary.choose [int; rat]

let text =
  QCheck.Arbitrary.(
    among ["f"; "g"; "h"; "a"; "b"; "c"; "d"]
      |> lift Symbol.of_string
  )

let default =
  Arbitrary.choose [base; text; arith]

let set =
  Arbitrary.(
    list default >>= fun l ->
    return (Symbol.Set.of_seq (Sequence.of_list l))
  )
