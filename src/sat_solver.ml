
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Logtk

let section = Util.Section.make ~parent:Const.section "msat"

module Lit = Bool_lit

type clause = Lit.t list

type form = clause list * int option

type result =
  | Sat
  | Unsat

let compare_form (c1,o1) (c2,o2) =
  let open CCOrd in
  list_ (list_ (Lit.compare)) c1 c2
  <?> (option CCInt.compare, o1, o2)

module FormSet = CCSet.Make(struct
  type t = form
  let compare = compare_form
end)

module Make(X : sig end) = struct
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
    update_ (FormSet.add ([c], tag))

  let add_clauses ?tag l =
    update_ (FormSet.add (l, tag))

  let add_clause_seq ?tag (seq:clause Sequence.t) =
    add_clauses ?tag (Sequence.to_rev_list seq)

  let eval_fail_ _ = invalid_arg "eval"
  let unsat_core_fail_ _ = invalid_arg "unsat core"

  let result_ = ref Sat
  let eval_ = ref eval_fail_
  let unsat_core_ : int Sequence.t ref = ref unsat_core_fail_
  let pp_ = ref Lit.pp

  let pp_clause out c =
    Format.fprintf out "@[<hv2>%a@]"
      (CCList.print ~sep:" ⊔ " !pp_) c

  let tag_ = snd

  let pp_form_simpl out (c,_) =
    CCFormat.list ~start:"" ~stop:"" ~sep:"" pp_clause out c

  let pp_form fmt f =
    match tag_ f with
    | None -> pp_form_simpl fmt f
    | Some tag -> Format.fprintf fmt "%a/%d" pp_form_simpl f tag

  let valuation l = !eval_ l
  let unsat_core k = !unsat_core_ k

  (* TODO incrementality *)

  let check () =
    unsat_core_ := unsat_core_fail_;
    eval_ := eval_fail_;
    Util.debugf ~section 4 "@[<hv2>formula before CNF:@ %a@]@."
      (fun k->k (CCFormat.seq pp_form) (FormSet.to_seq (get_())));
    (* Instantiate solver *)
    let module S = Msat.Sat.Make(struct
      let buf_ = Buffer.create 16
      let debug _ msg = Printf.ifprintf buf_ msg (* ignore *)
    end) in
    let assume_ ?tag (l:Lit.t list list) =
      let l = (l :> int list list)
        |> List.map (List.map S.make)
      in
      S.assume ?tag l
    in
    (* add problem *)
    FormSet.iter
      (fun (c,tag) -> assume_ ?tag c)
      (get_ ());
    (* solve *)
    let res = match S.solve () with
      | S.Sat ->
          eval_ := (fun (l:Lit.t) -> S.eval (S.make (l:>int)));
          Sat
      | S.Unsat ->
          unsat_core_ := (fun k ->
            S.get_proof ()
            |> S.unsat_core
            |> CCList.to_seq
            |> CCFun.tap
              (fun seq ->
                Util.debugf ~section 4 "@[unsat_core:@ @[<hv>%a@]@]"
                  (fun k->k (CCFormat.seq S.print_clause) seq)
              )
            |> Sequence.filter_map S.tag_clause
            |> Sequence.sort_uniq ~cmp:CCInt.compare
            |> Sequence.iter k
          );
          Unsat
    in
    result_ := res;
    res

  let set_printer pp = pp_ := pp

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
