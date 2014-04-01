
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

let base =
  Arbitrary.(among Type.TPTP.([i; o; int; rat]))

let _const s = Type.const (Symbol.of_string s)

let ground =
  Arbitrary.(
    let base = among Type.TPTP.([ i; int; _const "a"; _const "b" ]) in
    fix ~max:3 ~base (fun sub -> choose
      [ lift (Type.app (Symbol.of_string "list")) (list_repeat 1 sub)
      ; lift (Type.app (Symbol.of_string "prod")) (list_repeat 2 sub)
      ; lift2 Type.arrow_list (list sub) sub
      ]))

let default =
  Arbitrary.(
    let var = among [Type.var 0; Type.var 1 ] in
    let base =
      choose
      [ among [ Type.TPTP.i; Type.TPTP.int; _const "a"; _const "b"; ]
      ; var ]
    in
    fix ~max:4 ~base (fun sub -> choose
      [ lift (Type.app (Symbol.of_string "list")) (list_repeat 1 sub)
      ; lift (Type.app (Symbol.of_string "prod")) (list_repeat 2 sub)
      ; lift2 Type.arrow_list (list sub) sub
      ]))

