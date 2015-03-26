
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Bridge to [MSat] prover} *)

module Util = Logtk.Util
module BS = BoolSolver

let section = BS.section

module Make(X : sig end) : BS.SAT = struct

  type result =
    | Sat
    | Unsat

  [@@@warning "-39"]

  type lit = Qbf.Lit.t [@@deriving ord]
  type clause = lit list [@@deriving ord]

  type form =
    | Form of Qbf.Formula.t * int option
    | Clauses of clause list * int option
    [@@deriving ord]

  [@@@warning "+39"]

  module FormSet = Sequence.Set.Make(struct
    type t = form
    let compare = compare_form
  end)

  (* state of the algorithm *)
  type state = FormSet.t

  let stack_ : state CCVector.vector =
    let v = CCVector.create() in
    CCVector.push v FormSet.empty;
    v

  (* obtain current state *)
  let get_ () = CCVector.top_exn stack_
  let update_ f = CCVector.push stack_ (f (CCVector.pop_exn stack_))

  let add_clause ?tag (c:clause) =
    update_ (FormSet.add (Clauses ([c], tag)))

  let add_clauses ?tag l =
    update_ (FormSet.add (Clauses (l, tag)))

  let add_form ?tag f =
    update_ (FormSet.add (Form (f, tag)))

  let add_clause_seq ?tag (seq:clause Sequence.t) =
    add_clauses ?tag (Sequence.to_rev_list seq)

  let eval_fail_ _ = invalid_arg "eval"
  let unsat_core_fail_ _ = invalid_arg "unsat core"

  let result_ = ref Sat
  let eval_ = ref eval_fail_
  let unsat_core_ : int Sequence.t ref = ref unsat_core_fail_
  let pp_ = ref Qbf.Lit.print

  let pp_clause fmt c =
    Format.fprintf fmt "@[<hv2>%a@]"
      (CCList.print ~sep:" ⊔ " !pp_) c

  let tag_ = function
    | Clauses (_, t)
    | Form (_, t) -> t

  let pp_form_simpl fmt = function
    | Clauses (c,_) ->
        CCList.print ~start:"" ~stop:"" ~sep:"" pp_clause fmt c
    | Form (f,_) ->
        Qbf.Formula.print_with ~pp_lit:!pp_ fmt (Qbf.Formula.simplify f)

  let pp_form fmt f =
    match tag_ f with
    | None -> pp_form_simpl fmt f
    | Some tag -> Format.fprintf fmt "%a/%d" pp_form_simpl f tag

  let check () =
    unsat_core_ := unsat_core_fail_;
    eval_ := eval_fail_;
    Util.debugf ~section 4 "@[<hv2>formula before CNF:@ %a@]@."
      (Sequence.pp_seq pp_form) (FormSet.to_seq (get_()));
    (* Instantiate solver *)
    let module S = Msat.Sat.Make(struct
      let debug lvl fmt = Util.debug ~section lvl fmt
    end) in
    let assume_ ?tag (l:lit list list) =
      let l = (l :> int list list)
        |> List.map (List.map S.make)
      in
      S.assume ?tag l
    in
    (* add problem *)
    FormSet.iter
      (function
        | Clauses (c,tag) -> assume_ ?tag c
        | Form (f,tag) ->
            let clauses, _ = Qbf.Formula.cnf ~gensym:Qbf.Lit.fresh f in
            assume_ ?tag clauses
      ) (get_ ());
    (* solve *)
    let res = match S.solve () with
      | S.Sat ->
          eval_ := (fun (l:lit) -> S.eval (S.make (l:>int)));
          Sat
      | S.Unsat ->
          unsat_core_ := (fun k ->
            S.get_proof ()
            |> S.unsat_core
            |> CCList.to_seq
            |> CCFun.tap
              (fun seq ->
                Util.debugf ~section 4 "@[unsat_core:@ @[<hv>%a@]@]"
                  (Sequence.pp_seq S.print_clause) seq
              )
            |> Sequence.filter_map S.tag_clause
            |> Sequence.sort_uniq ~cmp:CCInt.compare
            |> Sequence.iter k
          );
          Unsat
    in
    result_ := res;
    res

  let valuation l = !eval_ l
  let unsat_core = Some (fun k -> !unsat_core_ k)

  let set_printer pp = pp_ := pp

  let name = "msat"


  type save_level = int

  let root_save_level = 0

  let save () =
    let i = CCVector.length stack_ in
    assert (i>0);
    let top = CCVector.top_exn stack_ in
    CCVector.push stack_ top;
    i

  let restore i =
    if i>= CCVector.length stack_ then invalid_arg "restore";
    CCVector.shrink stack_ i

end

let sat_solver =
  BS.{
    name="msat";
    strength=40;
    create=(fun () ->
      let module S = Make(struct end) in
      (module S : BS.SAT)
    );
  }

(* register as an extension *)
let () =
  let open Extensions in
  let e = {
    default with
    name="msat";
    init_actions=[Init_do(fun () -> BS.register_sat sat_solver)]
  } in
  register e
