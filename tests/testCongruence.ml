
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Logtk_arbitrary

module T = Term
module CC = Congruence.FO

(* build a congruence closure from a list of list of terms.
  each list of terms represents a congruence. *)
let _cc_of_classes classes : CC.t =
  let cc = CC.create () in
  List.fold_left
    (fun cc cls -> match cls with
       | [] -> cc
       | t::cls' ->
         let cc = CC.add cc t in
         List.fold_left (fun cc t' -> CC.mk_eq cc t t') cc cls')
    cc classes

let _size_class l =
  List.fold_left (fun acc t -> T.size t + acc) 0 l

let _size_classes l =
  List.fold_left (fun acc l -> (_size_class l) + acc) 0 l

let check_term_eq_itself =
  let gen = ArTerm.default in
  let prop t =
    let cc = CC.create () in
    CC.is_eq cc t t
  in
  let name = "congruence_term_eq_to_itself" in
  QCheck.Test.make ~long_factor:20 ~name gen prop

(* if we build a congruence closure with classes, in each class,
    all elements are equal *)
let check_classes_are_eq =
  let gen = QCheck.(list_of_size Gen.(1--10) (list_of_size Gen.(1--5) ArTerm.default)) in
  let prop classes =
    let cc = _cc_of_classes classes in
    List.for_all
      (fun cls -> match cls with
      | [] -> true
      | t::cls' -> List.for_all (fun t' -> CC.is_eq cc t t') cls')
      classes
  in
  let name = "congruence_class_members_are_eq" in
  QCheck.Test.make ~long_factor:20 ~name gen prop

(* reference congruence closure *)
module CC_ref : sig 
  type t
  val of_classes : T.t list list -> t
  val is_eq : t -> T.t -> T.t -> bool
  val pp : t CCFormat.printer
end = struct
  module TSet_set = CCSet.Make(T.Set)

  (* a set of congruence classes *)
  type t = TSet_set.t

  let is_eq_ (cc:t) t u : bool =
    T.equal t u ||
    TSet_set.exists
      (fun set -> T.Set.mem t set && T.Set.mem u set)
      cc

  (* are these two classes congruent? *)
  let are_congruent_ (cc:t) (s1:T.Set.t) (s2:T.Set.t) : bool =
    T.Set.exists
      (fun t ->
         T.Set.exists
           (fun u ->
              T.equal t u ||
              begin match T.view t, T.view u with
                | T.App (f1,l1), T.App (f2,l2) ->
                  is_eq_ cc f1 f2 &&
                  CCList.equal (is_eq_ cc) l1 l2
                | _ -> false
              end)
           s2)
      s1

  (* fixpoint: merge classes that are congruent *)
  let rec update (cc:t) : t =
    let find_merge_ cc =
      TSet_set.to_seq cc
      |> Sequence.flat_map
        (fun s1 ->
           assert (not (T.Set.is_empty s1));
           TSet_set.to_seq cc
           |> Sequence.filter (fun s2 -> s1 != s2)
           |> Sequence.map (fun s2 -> s1, s2))
      |> Sequence.find_map
        (fun (s1,s2) -> if are_congruent_ cc s1 s2 then Some (s1,s2) else None)
    in
    begin match find_merge_ cc with
      | None -> cc (* fixpoint *)
      | Some (s1,s2) ->
        cc
        |> TSet_set.remove s1
        |> TSet_set.remove s2
        |> TSet_set.add (T.Set.union s1 s2)
        |> update
    end

  let of_classes (l:T.t list list) : t =
    let s = List.map T.Set.of_list l |> TSet_set.of_list in
    update s

  let is_eq (cc:t) (t:T.t) (u:T.t) : bool =
    (* add t and u *)
    let cc =
      TSet_set.add_list cc [T.Set.singleton t; T.Set.singleton u]
      |> update
    in
    is_eq_ cc t u

  let pp out (cc:t): unit =
    let pp_c out s =
      Format.fprintf out "(@[<hv>%a@])" (Util.pp_seq T.pp) (T.Set.to_seq s)
    in
    Format.fprintf out "(@[<v>%a@])" (Util.pp_seq pp_c) (TSet_set.to_seq cc)
end

let check_ref =
  let gen = QCheck.(list_of_size Gen.(3--8) (list_of_size Gen.(1--6) ArTerm.default)) in
  let prop classes =
    let cc1 = _cc_of_classes classes in
    let cc2 = CC_ref.of_classes classes in
    let all_terms =
      Sequence.of_list classes
      |> Sequence.flat_map_l CCFun.id
      |> Sequence.to_rev_list
    in
    (* check every pair of terms *)
    Sequence.diagonal_l all_terms
    |> Sequence.for_all
      (fun (t,u) ->
         let eq1 = CC.is_eq cc1 t u in
         let eq2 = CC_ref.is_eq cc2 t u in
         if eq1<>eq2 then (
           QCheck.Test.fail_reportf
             "@[<2>cc.check_ref:@ inconsistency for terms `%a`@ and `%a`:@ \
              eq_cc %B, eq_cc_ref %B@ \
              :cc_ref %a@]"
             T.pp t T.pp u eq1 eq2 CC_ref.pp cc2
         ) else true)
  in
  let name = "congruence_ref" in
  QCheck.Test.make ~long_factor:20 ~name gen prop

let props =
  [ check_term_eq_itself;
    check_classes_are_eq;
    check_ref;
  ]
