
(*
Copyright (c) 2013-2014, Simon Cruanes
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

(** {1 Simple Resolution Prover} *)

module Err = CCError
module T = Logtk.FOTerm
module F = Logtk.Formula.FO
module Substs = Logtk.Substs
module Unif = Logtk.Unif
module Util = Logtk.Util

let _signature = ref Logtk.Signature.TPTP.base

(** {2 Literals and Clauses} *)

module Lit = struct
  type t = T.t * bool

  let compare = CCOrd.pair T.cmp CCOrd.bool_
  let equal a b = compare a b=0

  let pp buf (t,b) = Printf.bprintf buf "%s%a" (if b then "" else "¬") T.pp t
end

module Clause = struct
  type t = Lit.t list

  let make l = CCList.Set.uniq ~eq:Lit.equal l
  let compare = CCOrd.list_ Lit.compare
  let equal a b = compare a b = 0

  let is_trivial c =
    List.exists
      (fun (t,b) ->
        b &&
        List.exists (fun (t',b') -> not b' && T.eq t t') c
      ) c

  let apply_subst ~renaming subst c s_c =
    make (List.map (fun (t,b) -> Substs.FO.apply ~renaming subst t s_c, b) c)

  let pp buf c = CCList.pp ~sep:" | " Lit.pp buf c

  (* conversion from list of atomic formulas *)
  let _of_forms c =
    let _atom f = match F.view f with
      | F.Not f' ->
          begin match F.view f' with
          | F.Atom t -> t,false
          | _ -> failwith (CCPrint.sprintf "unsupported formula %a" F.pp f)
          end
      | F.Atom t -> t, true
      | _ -> failwith (CCPrint.sprintf "unsupported formula %a" F.pp f)
    in
    make (List.map _atom c)
end

module ClauseWithPos = struct
  type t = Clause.t * int
  let compare = CCOrd.pair Clause.compare CCInt.compare
end

(** {2 Saturation Algorithm} *)

module Index = Logtk.NPDtree.MakeTerm(ClauseWithPos)
module ClauseSet = Set.Make(Clause)

let _idx = ref (Index.empty())
let _active_set = ref ClauseSet.empty
let _passive_set = Queue.create()

exception Unsat

(* add [c] to the passive set, if not already present *)
let _add_passive c =
  if c = [] then raise Unsat
  else if Clause.is_trivial c
  then (
    Util.debug 4 "clause %a is trivial" Clause. pp c;
  )
  else if not (ClauseSet.mem c !_active_set)
  then (
    Util.debug 4 "new passive clause %a" Clause.pp c;
    Queue.push c _passive_set
  )

let _add_active c =
  _active_set := ClauseSet.add c !_active_set;
  List.iteri
    (fun i (t,_) -> _idx := Index.add !_idx t (c,i))
    c

(* factoring:
    A or A' or C
    ---------------
    sigma (A' or C)
    if sigma(A) = sigma(A')
*)
let _factoring c =
  List.iteri
    (fun i (t,b) ->
      if b then List.iteri
        (fun j (t',b') ->
          if i<j && b'
          then try
            let subst = Unif.FO.unification t 0 t' 0 in
            let c' = CCList.Idx.remove c i in
            let renaming = Substs.Renaming.create() in
            let c' = Clause.apply_subst ~renaming subst c' 0 in
            Util.debug 3 "factoring of %a ----> %a" Clause.pp c Clause.pp c';
            _add_passive c'
          with Unif.Fail -> ()
        ) c
    ) c

(* resolution with [c] and active set.
  A or C    ¬A or D
  -----------------
    sigma(C or D)
if sigma(A) = sigma(A')
*)
let _resolve_with c =
  List.iteri
    (fun i (t,b) ->
      Index.retrieve_unifiables !_idx 0 t 1 ()
        (fun () _t' (d,j) subst ->
          let (_,b') = List.nth d j in
          if b<>b'
          then (
            let renaming = Substs.Renaming.create() in
            let concl =
              (let c' = CCList.Idx.remove c i in
               Clause.apply_subst ~renaming subst c' 1)
              @
              (let d' = CCList.Idx.remove d j in
               Clause.apply_subst ~renaming subst d' 0)
            in
            let concl = Clause.make concl in
            Util.debug 3 "resolution of %a and %a ---> %a"
              Clause.pp c Clause.pp d Clause.pp concl;
            _add_passive concl
          )
        )
    ) c

(* main saturation algorithm, a simple "given clause" loop *)
let _saturate formulas =
  List.iter _add_passive formulas;
  try
    while not (Queue.is_empty _passive_set) do
      let c = Queue.pop _passive_set in
      if not (Clause.is_trivial c) && not (ClauseSet.mem c !_active_set)
      then (
        Util.debug 2 "given clause: %a" Clause.pp c;
        _add_active c;
        _resolve_with c;
        _factoring c;
      )
    done;
    `Sat
  with
  | Unsat -> `Unsat

(** {2 Main} *)

let process_file f =
  Util.debug 2 "process file %s..." f;
  let res = Err.(
    (* parse *)
    Logtk_parsers.Util_tptp.parse_file ~recursive:true f 
    (* type inference *)
    >>=
    Logtk_parsers.Util_tptp.infer_types (`sign !_signature)
    (* CNF *)
    >>= fun (signature, statements) ->
    let clauses = Logtk_parsers.Util_tptp.Typed.formulas statements in
    let clauses = Sequence.to_list clauses in
    let ctx = Logtk.Skolem.create ~prefix:"sk" signature in
    let clauses = Logtk.Cnf.cnf_of_list ~ctx clauses in
    let clauses = CCList.map Clause._of_forms clauses in
    _signature := Logtk.Skolem.to_signature ctx; (* recover signature *)
    (* saturation *)
    Err.return (_saturate clauses)
  ) in
  match res with
  | `Error msg ->
      print_endline msg;
      exit 1
  | `Ok `Sat -> print_endline "sat"
  | `Ok `Unsat -> print_endline "unsat"

let _options = ref (
  [ 
  ] @ Logtk.Options.global_opts
  )
let _help = "usage: satbleau file.p"
let _file = ref None

let _set_file f = match !_file with
  | None -> _file := Some f
  | Some _ -> failwith "can only deal with one file"

let main () =
  Arg.parse !_options _set_file _help;
  match !_file with
  | None -> print_endline _help; exit 0
  | Some f -> process_file f

let () = main()
