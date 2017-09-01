
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
open Logtk_arbitrary

module T = Term
module CC = Congruence.FO

(* build a congruence closure from a list of list of terms.
  each list of terms represents a congruence. *)
let _cc_of_classes classes =
  let cc = CC.create () in
  List.iter (fun cls -> match cls with
    | [] -> ()
    | t::cls' -> List.iter (fun t' -> CC.mk_eq cc t t') cls')
    classes;
  cc

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

let props =
  [ check_term_eq_itself
  ; check_classes_are_eq
  ]
