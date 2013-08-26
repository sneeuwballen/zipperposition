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

(** {1 Meta-prover, working on axioms, theories and lemmas} *)

type t = {
  mutable kb : MetaKB.t;
  mutable patterns : MetaPattern.Set.t;
  on_lemma : MetaKB.found_lemma Signal.t;
  on_axiom : MetaKB.found_axiom Signal.t;
  on_theory : MetaKB.found_theory Signal.t;
  reasoner : MetaReasoner.t;
}

let create () =
  let reasoner = MetaReasoner.create () in
  let p = {
    kb = MetaKB.empty;
    patterns = MetaPattern.Set.empty;
    reasoner;
    on_lemma = MetaKB.on_lemma reasoner;
    on_axiom = MetaKB.on_axiom reasoner;
    on_theory = MetaKB.on_theory reasoner;
  } in
  p

let patterns p = p.patterns

let kb p = p.kb

let reasoner p = p.reasoner

let check_formulas p formulas =
  let instances = Sequence.fold
    (fun l formula ->
      let l' = MetaPattern.Set.matching p.patterns formula in
      List.rev_append l' l)
    [] formulas
  in
  List.iter
    (fun (pat, args) ->
      let lit = MetaReasoner.Translate.encode MetaPattern.mapping "pattern" (pat, args) in
      MetaReasoner.add_fact p.reasoner lit)
    instances
  
let add_pattern p pat =
  p.patterns <- MetaPattern.Set.add p.patterns pat

let on_lemma p = p.on_lemma

let on_theory p = p.on_theory

let on_axiom p = p.on_axiom

let parse_theory_file p filename =
  let kb = MetaKB.parse_theory_file filename in
  p.kb <- MetaKB.union p.kb kb

let save_kb p filename =
  MetaKB.save filename p.kb 

let restore_kb p filename =
  match MetaKB.restore filename with
  | None -> ()
  | Some kb ->
    p.kb <- MetaKB.union p.kb kb
