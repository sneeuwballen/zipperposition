(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** E-graph imperative data structure *)

open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * types
 * ---------------------------------------------------------------------- *)

(** Label of a node: symbol or variable *)
type label = NodeVar of int | NodeSymbol of string

let is_var_label = function
  | NodeVar _ -> true
  | NodeSymbol _ -> false

let is_symb_label = function
  | NodeVar _ -> false
  | NodeSymbol _ -> true

(** A node that represents a term in the E-graph *)
type egraph_node = {
  node_label: label;                        (** label of the node *)
  node_sort: sort;                          (** sort of the node *)
  node_term: foterm;                        (** term this node represents *)
  node_children: egraph_node list;          (** children of the node *)
  mutable node_representative: egraph_node; (** representative of the term *)
  mutable node_parents: egraph_node list;   (** parents terms of the node *)
  mutable node_class: egraph_node list;     (** equivalence class, only meaningful when the
                                                node is the representative of its class *)
}

(** hashtable with terms as keys *)
module THashtbl = Hashtbl.Make(
  struct
    type t = foterm
    let equal = T.eq_foterm
    let hash x = x.hkey
  end)

(** Actions to perform to backtrack *)
type action =
  | StopBacktracking  (** we reached the last backtracking point *)
  | Delete of egraph_node
  | SetParents of egraph_node * egraph_node list
  | SetClass of egraph_node * egraph_node list
  | SetRepresentative of egraph_node * egraph_node
  | SetSymbol of string * egraph_node list
  | SetMaxvar of int

(** The E-graph structure *)
type egraph = {
  graph_nodes: egraph_node THashtbl.t;                  (** term -> node *)
  graph_symbol: (string, egraph_node list) Hashtbl.t;   (** f -> nodes f(t1...tn) *)
  graph_stack: action Stack.t;                          (** stack used for backtracking *)
  mutable graph_maxvar: int;                            (** max variable in E-graph *)
}

(* ----------------------------------------------------------------------
 * basic operations and congruence closure
 * ---------------------------------------------------------------------- *)

(** label of a term, i.e. the root symbol/var *)
let rec compute_label t =
  match t.term with
  | Var i -> NodeVar i
  | Leaf s -> NodeSymbol s
  | Node (hd::_) -> compute_label hd
  | Node [] -> assert false

(** create an empty E-graph *)
let empty () =
  let nodes = THashtbl.create 1621
  and stack = Stack.create () in
  Stack.push StopBacktracking stack;
  { graph_nodes = nodes;
    graph_symbol = Hashtbl.create 41;
    graph_stack = stack;
    graph_maxvar = 0; }

(** push a backtracking point on the stack *)
let push egraph =
  Stack.push StopBacktracking egraph.graph_stack

let add_to_symbols egraph symbol node =
  let l =
    try Hashtbl.find egraph.graph_symbol symbol
    with Not_found -> []
  in
  Stack.push (SetSymbol (symbol, l)) egraph.graph_stack;
  Hashtbl.replace egraph.graph_symbol symbol (node::l)

let remove_from_symbols egraph symbol node =
  try
    let l = Hashtbl.find egraph.graph_symbol symbol in
    let l = List.filter (fun n -> n != node) l in
    if l = []
      then Hashtbl.remove egraph.graph_symbol symbol
      else Hashtbl.replace egraph.graph_symbol symbol l
  with Not_found -> failwith ("tried to remove unknown symbol " ^ symbol)

let remove_node egraph node =
  THashtbl.remove egraph.graph_nodes node.node_term;
  List.iter
    (fun child ->  (* remove node from the parents of all its children *)
      child.node_parents <- List.filter (fun parent -> parent != node) child.node_parents)
    node.node_children;
  (* now if node = f(t1...tn) remove node from terms_f *)
  match node.node_label with
  | NodeVar _ -> ()
  | NodeSymbol f -> remove_from_symbols egraph f node

let from_symbol egraph symbol =
  try Hashtbl.find egraph.graph_symbol symbol
  with Not_found -> []

(** pop to the last backtracking point, cancelling all actions performed since *)
let pop egraph =
  let do_action = function
  | StopBacktracking -> assert false
  | Delete node -> remove_node egraph node
  | SetParents (node, parents) -> node.node_parents <- parents
  | SetClass (node, class_) -> node.node_class <- class_
  | SetRepresentative (node, representative) -> node.node_representative <- representative
  | SetSymbol (symbol, nodes) ->
    if nodes = []
      then Hashtbl.remove egraph.graph_symbol symbol
      else Hashtbl.replace egraph.graph_symbol symbol nodes
  | SetMaxvar v -> egraph.graph_maxvar <- v
  in
  (* unwind the stack down to the last backtracking point *)
  let rec unwind () =
    let action =
      try Stack.pop egraph.graph_stack
      with Stack.Empty -> raise (Invalid_argument "no backtracking point left")
    in
    match action with
    | StopBacktracking -> ()  (* reached the last backtracking point *)
    | _ -> do_action action; unwind ()
  in
  unwind ()

(** find the representative of a node *)
let rec find node =
  let repr = node.node_representative in
  if node == repr
    then node  (* its own representative *)
    else begin
      (if T.eq_foterm node.node_term repr.node_term
        then Format.printf "two nodes have same term %a" !T.pp_term#pp node.node_term);
      assert (not (T.eq_foterm node.node_term repr.node_term));
      (* recurse to find the actual representative *)
      let root = find repr in
      (* path compression, disabled because it may not be backtrackable *)
      (* node.node_representative <- root; *)
      root
    end

(** union of two nodes *)
let union egraph n1 n2 =
  (* merge the first into the second. The first is no more a representative. *)
  let merge_into from into =
    assert (from.node_representative == from);
    assert (into.node_representative == into);
    Stack.push (SetRepresentative (from, from)) egraph.graph_stack;
    Stack.push (SetClass (into, into.node_class)) egraph.graph_stack;
    from.node_representative <- into;
    into.node_class <- List.rev_append from.node_class into.node_class
  in
  (* merge one parent into the other *)
  let n1 = find n1
  and n2 = find n2 in
  match n1, n2 with
  | _ when n1 == n2 -> () (* already merged *)
  | _ when is_var_label n1.node_label -> merge_into n1 n2 (* var -> term *)
  | _ when is_var_label n2.node_label -> merge_into n2 n1 (* var -> term *)
  | _ when List.length n1.node_class > List.length n2.node_class ->
    merge_into n2 n1  (* smaller class merged into bigger class *)
  | _ -> merge_into n1 n2

(** get term from node *)
let term_of_node node = node.node_term

let are_equal n1 n2 = n1 == n2 || find n1 == find n2

(** Is the term present in the E-graph? *)
let term_in_graph egraph t =
  THashtbl.mem egraph.graph_nodes t

let equiv_class node = (find node).node_class

let representative node = find node

(** check whether two nodes are congruent. They are if they
    are currently equal (same representative), or if they have the
    same label and all their children are congruent. *)
let congruent n1 n2 =
  are_equal n1 n2 ||
    (n1.node_label = n2.node_label &&
     (try List.for_all2 are_equal n1.node_children n2.node_children
      with Invalid_argument _ -> false))

(** merge two terms (assert a = b). This follows the Nelson-Oppen
    algorithm for computing congruence closure, 1980. *)
let rec merge egraph n1 n2 =
  let n1 = find n1
  and n2 = find n2 in
  if n1 == n2
    then ()  (* trivial *)
    else begin
      let c1 = n1.node_class
      and c2 = n2.node_class in
      (* union of classes *)
      union egraph n1 n2;
      (* check parents of equivalence classes for new congruences *)
      List.iter
        (fun t1 ->
          List.iter
            (fun t2 ->
              List.iter
                (fun parent1 ->
                  List.iter
                    (fun parent2 ->
                        if find parent1 != find parent2 &&
                            congruent parent1 parent2 then merge egraph parent1 parent2)
                    t2.node_parents)
                t1.node_parents)
            c2)
        c1
    end

(** creation of a node *)
let rec node_of_term egraph t =
  try THashtbl.find egraph.graph_nodes t
  with Not_found ->
    (* find subnodes *)
    let subterms = match t.term with
    | Var _ | Leaf _ -> []
    | Node (hd::tl) -> List.map (node_of_term egraph) tl
    | Node [] -> assert false
    in
    (* create a node *)
    let rec node = {
      node_term = t;
      node_sort = t.sort;
      node_label = compute_label t;
      node_children = subterms;
      node_representative = node;
      node_class = [node];
      node_parents = [];
    }
    in
    (* put the node in the hashtable *)
    THashtbl.add egraph.graph_nodes t node;
    (* add node as a parent of all its children *)
    List.iter (fun child -> child.node_parents <- node :: child.node_parents) subterms;
    (* if t = f(t1...tn), put t in the use list of f, and
       check for congruences *)
    begin match node.node_label with
    | NodeVar i ->
      if i > egraph.graph_maxvar then
        (Stack.push (SetMaxvar egraph.graph_maxvar) egraph.graph_stack;
        egraph.graph_maxvar <- i)
    | NodeSymbol f ->
      add_to_symbols egraph f node;
      List.iter
        (fun node' -> if node != node' && congruent node node' then merge egraph node node')
        (from_symbol egraph f)
    end;
    (* delete node when backtracking *)
    Stack.push (Delete node) egraph.graph_stack;
    (* return the node *)
    node

(** max var index in E-graph *)
let maxvar egraph = egraph.graph_maxvar

(* ----------------------------------------------------------------------
 * unification/matching functions
 * ---------------------------------------------------------------------- *)

(** A substitution maps (var) nodes to nodes. *)
type subst = (egraph_node * egraph_node) list

(** check whether var is bound by subst *)
let in_subst subst var =
  assert (is_var_label var.node_label);
  List.exists (fun (v, _) -> v == var) subst

(** get the term var is bound to in subst, or raises Not_found *)
let get_subst subst var =
  assert (is_var_label var.node_label);
  let rec recurse subst = match subst with
  | [] -> raise Not_found
  | (v,n)::subst' -> if v == var then n else recurse subst'
  in
  recurse subst

module PairSet = Set.Make(
  struct
    type t = egraph_node * egraph_node
    let compare (n1, n2) (n1', n2') =
      if T.eq_foterm n1.node_term n1'.node_term
        then T.compare_foterm n2.node_term n2'.node_term
        else T.compare_foterm n1.node_term n1'.node_term
  end)

(** Check whether the node (which must be a variable) is
    bound in the E-graph. It amounts to check whether
    the representative is a variable. *)
let is_bound node =
  assert (is_var_label node.node_label);
  not (is_var_label (find node).node_label)

(** all possible linear unifications between the two terms, modulo congruence.
    if a variable is to be bound several times, it will be bound several times
    in the substitution *)
let linear_soft_unify egraph n1 n2 subst k =
  (* try to unify those equivalence classes (explored: set of pairs already tried) *)
  let rec unify n1 n2 explored subst k =
    if are_equal n1 n2
      then k subst  (* trivial success *)
      else if n1.node_sort <> n2.node_sort || PairSet.mem (find n1, find n2) explored
        then () (* cannot unify, distinct sorts or already pending pair *)
        else    (* try all combinations *)
          let explored = PairSet.add (find n1, find n2) explored in
          unify_all (equiv_class n1) (equiv_class n2) explored subst k
  (* try to unify every term of l1 with every term of l2 *)
  and unify_all l1 l2 explored subst k =
    match l1 with
    | [] -> ()
    | n1::l1' ->
      (* unify n1 with terms in l2 *)
      List.iter (fun n2 -> root_unify n1 n2 explored subst k) l2;
      unify_all l1' l2 explored subst k
  (* try to unify those two terms *)
  and root_unify n1 n2 explored subst k =
    assert (not (are_equal n1 n2));
    match n1.node_label, n2.node_label with
    | NodeVar _, _ when not (is_bound n1) && not (T.member_term n1.node_term n2.node_term) ->
      k ((n1, n2) :: subst)  (* bind n1 to n2, if n1 not bound *)
    | _, NodeVar _ when not (is_bound n2) && not (T.member_term n2.node_term n1.node_term) ->
      k ((n2, n1) :: subst)  (* bind n2 to n1, if n2 not bound *)
    | NodeSymbol g, NodeSymbol h when g = h &&
      List.length n1.node_children = List.length n2.node_children ->
      assert (n1.node_children <> []);  (* otherwise they would be equal *)
      unify_subterms n1.node_children n2.node_children explored subst k
    | _ -> ()  (* failure *)
  (* unify children pairwise *)
  and unify_subterms l1 l2 explored subst k =
    match l1, l2 with
    | [], [] -> k subst (* success *)
    | n1::l1', n2::l2' ->
      let new_f subst' = unify_subterms l1' l2' explored subst' k in
      unify n1 n2 explored subst new_f
    | _ -> assert false (* not same length *)
  in
  unify n1 n2 PairSet.empty subst k

(** Linear unification of the term t against the E-graph. Any substitution
    sigma returned is such that sigma(t) and sigma(t'), where t' is
    a term in the E-graph, top-unify. *)
let linear_hard_unify egraph t subst k =
  (* match term against E-graph *)
  let rec unify_term t subst k =
    match t.term with
    | Var _ ->
      (* try against all nodes that have the same sort... *)
      THashtbl.iter
        (fun _ node ->
          if node.node_sort = t.sort && not (T.member_term t node.node_term)
            then k ((t, node.node_term) :: subst))
        egraph.graph_nodes
    | Leaf g ->
      if term_in_graph egraph t
        then k subst  (* ok, it matches some term in the graph *)
        else ()  (* fails, no such term *)
    | Node ({term=Leaf g}::tl) ->
      (* unify against children of all terms that start with g *)
      List.iter
        (fun node ->
           if List.length tl = List.length node.node_children
             then unify_list tl node.node_children subst k)
        (from_symbol egraph g)
    | Node _ -> assert false
  (* unify list of terms against list of nodes *)
  and unify_list terms nodes subst k =
    match terms, nodes with
    | [], [] -> k subst  (* success *)
    | t::terms', node::nodes' ->
      let new_f subst = unify_list terms' nodes' subst k in
      unify_node t node subst new_f
    | _ -> assert false
  (* unify term and node *)
  and unify_node t node subst k =
    (if term_in_graph egraph t && are_equal (node_of_term egraph t) node
      then k subst (* t congruent to node, success *) else ());
    match t.term, node.node_label with
    | Var _, _ -> k (S.build_subst t node.node_term subst)
    | _, NodeVar _ -> k (S.build_subst node.node_term t subst)
    | Leaf g, _ ->
      List.iter  (* try to unify t with a variable *)
        (fun node' ->
          match node'.node_label with
          | NodeVar _ ->
            k (S.build_subst node'.node_term t subst)  (* variable congruent to node *)
          | _ -> ())
        (equiv_class node)
    | Node ({term=Leaf g}::tl), _ ->
      let len = List.length tl in
      (* try to match tl with children of all nodes congruent to node, that are
         labelled with g *)
      List.iter
        (fun node' -> 
          match node'.node_label with
          | NodeVar _ ->
            k (S.build_subst node'.node_term t subst)  (* variable congruent to node *)
          | NodeSymbol h when g = h &&  List.length node'.node_children = len ->
            unify_list tl node'.node_children subst k (* unify subterms *)
          | _ -> ())
        (equiv_class node)
    | Node _, _ -> assert false
  in
  unify_term t subst k

(** Proper matching of the terms against the E-graph. Proper means
    that if a variable x occurs several times in the list of terms,
    all its occurrences will match nodes in the same equivalence class.

    It returns a list of results, where each result is a list of nodes
    that match the input terms, and a substitution to bind variables.

    For instance, when matching [f(x,x), x] against an E-graph
    where a = b, and f(a,b) and b occur, then [f(a,b),b] and sigma={x->a} will be
    a proper matcher since f(x,x) matches f(a,b) modulo the congruence. *)
let proper_match egraph patterns subst k =
  (* only those variables can be bound during matching *)
  let vars = List.fold_left
    (fun vars t -> T.merge_varlist (T.vars_of_term t) vars)
    [] patterns in
  let can_bind v = List.exists (fun t -> T.eq_foterm v t) vars in
  (* match terms against E-graph (inspired from Simplify's E-matching iterators) *)
  let rec match_terms terms acc subst k =
    match terms with
    | [] -> k acc subst
    | t::terms' ->
      let new_f acc subst = match_terms terms' acc subst k in
      match_term t acc subst new_f
  (* match term against E-graph *)
  and match_term t acc subst k =
    let t = S.apply_subst subst t in
    Utils.debug 4 (lazy (Utils.sprintf "match term %a with E-graph" !T.pp_term#pp t));
    match t.term with
    | Var _ when can_bind t ->
      (* try against all nodes that have the same sort... *)
      THashtbl.iter
        (fun _ node ->
          if node.node_sort = t.sort && not (T.member_term t node.node_term)
            then k (node :: acc) ((t, node.node_term) :: subst))
        egraph.graph_nodes
    | Var _ | Leaf _ ->
      if term_in_graph egraph t
        then k ((node_of_term egraph t) :: acc) subst  (* matches itself *)
        else ()  (* fails, no such term *)
    | Node ({term=Leaf g}::tl) ->
      (* match against children of all terms that start with g *)
      List.iter
        (fun node ->
          let new_f subst = k (node :: acc) subst in
           if List.length tl = List.length node.node_children
             then match_list tl node.node_children subst new_f)
        (from_symbol egraph g)
    | Node _ -> assert false
  (* match list of terms against list of nodes *)
  and match_list terms nodes subst k =
    match terms, nodes with
    | [], [] -> k subst  (* success *)
    | t::terms', node::nodes' ->
      let new_f subst = match_list terms' nodes' subst k in
      match_node t node subst new_f
    | _ -> assert false
  (* match term and node *)
  and match_node t node subst k =
    let t = S.apply_subst subst t in
    Utils.debug 4 (lazy (Utils.sprintf "match term %a with node %a"
                  !T.pp_term#pp t !T.pp_term#pp node.node_term));
    match t.term with
    | Var _ when can_bind t ->
      let node = find node in (* bind with representative, to avoid var-> bound var *)
      k (S.build_subst t node.node_term subst)
    | Var _ | Leaf _ ->
      if term_in_graph egraph t && are_equal (node_of_term egraph t) node
        then k subst  (* t is congruent to node *)
        else ()
    | Node ({term=Leaf g}::tl) ->
      let len = List.length tl in
      (* try to match tl with children of all nodes congruent to node, that are
         labelled with g *)
      let g_label = NodeSymbol g in
      List.iter
        (fun node' -> 
          if node'.node_label = g_label && List.length node'.node_children = len
            then match_list tl node'.node_children subst k)
        (equiv_class node)
    | Node _ -> assert false
  in
  match_terms patterns [] subst k

(* ----------------------------------------------------------------------
 * High level interface for theories
 * ---------------------------------------------------------------------- *)

(** rename the equation so that none of its variables
    occur in the current E-graph *)
let relocate_equation egraph (e1, e2) =
  if T.is_ground_term e1 && T.is_ground_term e2
    then (e1, e2)
    else
      let vars = T.merge_varlist (T.vars_of_term e1) (T.vars_of_term e2) in
      let _, _, renaming = S.relocate (maxvar egraph) vars S.id_subst in
      S.apply_subst ~recursive:false renaming e1, S.apply_subst ~recursive:false renaming e2

type theory = (foterm * foterm) list

(** Close the E-graph w.r.t some set of equations *)
let theory_close egraph equations =
  (* apply equations while they give new equalities *)
  let rec loop current_equations =
    match current_equations with
    | [] -> ()  (* exit *)
    | (e1,e2)::next_equations ->
      let some_change = ref false in
      (* avoid variable collision by renaming terms *)
      let e1, e2 = relocate_equation egraph (e1, e2) in
      (* equate the list of nodes *)
      let f nodes subst =
        match nodes with
        | [a;b] when are_equal a b -> ()  (* already equal *)
        | [a;b] -> (* merge a and b! *)
          Utils.debug 3 (lazy (Utils.sprintf
            "  @[<h>theory equation %a=%a -> merge %a and %a with %a@]"
            !T.pp_term#pp e1 !T.pp_term#pp e2 !T.pp_term#pp a.node_term
            !T.pp_term#pp b.node_term S.pp_substitution subst));
          some_change := true;
          merge egraph a b
        | _ -> (Format.printf "got nodes [%a]@." (Utils.pp_list
              (fun f node -> !T.pp_term#pp f node.node_term)) nodes; assert false)
      in
      (* E-matching to propagate the equation e1=e2 *)
      proper_match egraph [e1; e2] S.id_subst f;
      (* if it did something, re-scan through all equations *)
      if !some_change
        then loop equations
        else loop next_equations
  in
  Utils.debug 3 (lazy "close egraph w.r.t theory");
  loop equations

(** Set of possible paramodulation inferences. Each inference is a 
    (possibly speculative) top-unification of the side of an equation,
    and of some node in the E-graph. *)
let find_paramodulations egraph equations =
  let answers = ref [] in
  (* check whether the substitution is already true in the E-graph *)
  let subst_true subst =
    List.for_all
      (fun (v, t) ->
        term_in_graph egraph v && term_in_graph egraph t &&
        are_equal (node_of_term egraph v) (node_of_term egraph t))
      subst
  in
  (* try to paramodulate with every equation of the theory *)
  List.iter
    (fun (e1, e2) ->
      (* avoid variable collision by renaming terms *)
      let e1, e2 = relocate_equation egraph (e1, e2) in
      (* if terms are already in the E-graph, and equal, it's no use
         paramodulating with them *)
      (if term_in_graph egraph e1
        then assert (T.vars_of_term e1 = []));
      (if term_in_graph egraph e2
        then assert (T.vars_of_term e2 = []));
      (* function that will update answers *)
      let f subst =
        if subst_true subst
          (* means that the term is already in the E-graph *)
          then assert (T.vars_of_term e1 = [] || T.vars_of_term e2 = [])
          else answers := (e1, e2, subst) :: !answers in
      linear_hard_unify egraph e1 S.id_subst f;
      linear_hard_unify egraph e2 S.id_subst f)
    equations;
  !answers

(** Apply the term substitution to the E-graph, after adding both
    terms to the E-graph *)
let rec apply_substitution egraph subst =
  match subst with
  | [] -> ()
  | (v,t) :: subst' ->
    let v = node_of_term egraph v
    and t = node_of_term egraph t in
    merge egraph v t;
    apply_substitution  egraph subst'

(** Apply the paramodulation to the E-graph *)
let apply_paramodulation egraph (t1, t2, subst) =
  let t1' = S.apply_subst subst t1
  and t2' = S.apply_subst subst t2 in
  Utils.debug 3 (lazy (Utils.sprintf "apply paramodulation %a=%a with %a"
                !T.pp_term#pp t1 !T.pp_term#pp t2 S.pp_substitution subst));
  ignore (node_of_term egraph t1');
  ignore (node_of_term egraph t2');
  (* remove from subst the bindings of terms, keep the bindings of E-graph nodes *)
  let subst = S.restrict_exclude subst t1 in
  let subst = S.restrict_exclude subst t2 in
  (* TODO: if the effect of the paramodulation is null (already done, for instance), backtrack *)
  apply_substitution egraph subst

(** Apply the substitution to the E-graph *)
let rec apply_subst egraph subst =
  match subst with
  | [] -> ()
  | (n1, n2) :: subst' ->
    merge egraph n1 n2;
    apply_subst egraph subst'

(** Convert a substitution on nodes to a substitution on terms
    TODO what happens if all terms of some equivalence class do occur check? *)
let substitution_of_subst subst =
  List.map
    (fun (n1, n2) ->
      assert (is_var_label n1.node_label);
      let v = n1.node_term in
      if T.member_term v (find n2).node_term
        then  (* find a term that does not occur check with v *)
          let n2 =
            List.find
              (fun node -> not (T.member_term v node.node_term))
              (equiv_class n2) in
          v, n2.node_term
        else v, (find n2).node_term (* normalize if occur check allows it *))
    subst

(** Try to close the E-graph by unifying the two given nodes. It
    returns a list of E-substitutions on success,
    or an empty list on failure. *)
let try_unify egraph theory n1 n2 =
  (* remove multiple bindings of a variable *)
  let rec uniquify subst =
    match subst with 
    | [] -> []
    | (n1, n2) as pair :: subst' ->
      assert (is_var_label n1.node_label);
      if List.exists (fun (n1',_) -> n1 == n1') subst'
        then uniquify subst'
        else pair :: (uniquify subst')
  in
  (* collect answers by unification within the E-graph *)
  let answers = ref [] in
  let k subst = answers := (uniquify subst) :: !answers in
  linear_soft_unify egraph n1 n2 [] k;
  let answers = !answers in
  List.filter
    (fun subst ->
      (* test whether the substitution actually works, by applying them *)
      push egraph;
      try
        apply_subst egraph subst;     (* apply substitution to E-graph *)
        theory_close egraph theory;   (* apply theories *)
        let did_unify = are_equal n1 n2 in
        Utils.debug 3 (lazy (Utils.sprintf
          "unification of %a and %a with %a %s" !T.pp_term#pp n1.node_term
          !T.pp_term#pp n2.node_term S.pp_substitution (substitution_of_subst subst)
          (if did_unify then "succeeds" else "fails")));
        pop egraph;
        did_unify
      with e ->
        pop egraph; raise e)
    answers

module SubstSet = Set.Make(
  struct
    type t = substitution
    let compare = S.compare_substs
  end)

(** Complete the substitution by finding bindings, if any, for
    the given list of variables.*)
let complete_subst egraph subst vars =
  let subst = substitution_of_subst subst in
  List.fold_left
    (fun subst v ->
      if not (S.is_in_subst v subst) && term_in_graph egraph v
        then
          let node = node_of_term egraph v in
          S.build_subst v (find node).node_term subst
        else subst)
    subst vars

(** Search the tree of possible paramodulations, down to the given
    depth, and returns all substitutions that close some branch *)
let e_unify egraph theory t1 t2 depth k =
  let vars = T.merge_varlist (T.vars_of_term t1) (T.vars_of_term t2) in
  assert (depth >= 0);
  let answers = ref SubstSet.empty in
  Utils.debug 2 (lazy (Utils.sprintf "*** E-unify @[<h>%a@] and %a in theory @[<hv>%a@]"
                 !T.pp_term#pp t1 !T.pp_term#pp t2
                 (Utils.pp_list (fun f (e1,e2) -> Format.fprintf f "@[<h>%a=%a@]"
                    !T.pp_term#pp e1 !T.pp_term#pp e2)) theory));
  push egraph;
  let n1 = node_of_term egraph t1
  and n2 = node_of_term egraph t2 in
  (* depth-first search *)
  let rec explore cur_depth =
    Utils.debug 3 (lazy (Utils.sprintf "==== enter depth %d ====" cur_depth));
    (* close w.r.t. the theory *)
    theory_close egraph theory;
    (* is the current state suitable for syntactic unification? *)
    let current_answers = try_unify egraph theory n1 n2 in
    List.iter
      (fun subst ->
        let subst = complete_subst egraph subst vars in
        if SubstSet.mem subst !answers
          then ()  (* already yielded *)
          else begin
            answers := SubstSet.add subst !answers;
            k subst (* yield this substitution *)
          end)
      current_answers;
    (* try paramodulations, if not too deep *)
    if cur_depth < depth
      then begin
        let params = find_paramodulations egraph theory in
        List.iter
          (fun param ->
            (* try this paramodulation in a new stack frame *)
            push egraph;
            try
              apply_paramodulation egraph param;
              explore (cur_depth+1);
              pop egraph
            with e -> (pop egraph; raise e))
          params
      end else ();
    Utils.debug 3 (lazy (Utils.sprintf "==== exit depth %d ====" cur_depth));
  in
  (* do the exploration down to the given depth *)
  (try
    explore 0;
    pop egraph
  with e -> (pop egraph; raise e))

(* TODO: investigate stack overflow
   TODO: caching of already yielded answers at the e_unify function level
   TODO: detect commutating paramodulations, to avoid repeating work
   TODO: detect non-restraining paramodulations and do them inconditionally when expanding
*)

(* ----------------------------------------------------------------------
 * DOT printing
 * ---------------------------------------------------------------------- *)

module Graph =
  struct
    type vertex = egraph_node
    type edge = EdgeSubterm | EdgeCongruent

    let equal n1 n2 = n1 == n2
    let hash n = n.node_term.hkey
    
    let print_vertex node =
      match node.node_label with
      | NodeVar i -> Utils.sprintf "X%d" i
      | NodeSymbol s -> s

    let print_edge _ = ""
  end

(** module to print E-graphs to DOT *)
module D = Dot.Make(Graph)

(** Print the E-graph in DOT format *)
let to_dot ~name egraph =
  let graph = D.mk_graph ~name in
  (* map the node to the DOT graph *)
  let on_node node = 
    let n = D.get_node graph node in
    D.add_node_attribute n (D.Shape "box");
    (* add links to children *)
    List.iter
      (fun child ->
        let c = D.get_node graph child in
        let e = D.add_edge graph n c Graph.EdgeSubterm in
        D.add_edge_attribute e (D.Weight 10);
        D.add_edge_attribute e (D.Style "filled");
        D.add_edge_attribute e (D.Style "bold"))
      node.node_children;
    (* add links to congruent terms (to representative) *)
    if node.node_representative != node
      then 
        let c = D.get_node graph node.node_representative in
        let e = D.add_edge graph n c Graph.EdgeCongruent in
        D.add_edge_attribute e (D.Weight 1);
        D.add_edge_attribute e (D.Style "dotted");
        D.add_edge_attribute e (D.Color "red");
        D.add_edge_attribute e (D.Other ("arrowhead", "none"))
  in
  THashtbl.iter (fun _ node -> on_node node) egraph.graph_nodes;
  (* print the graph into a string *)
  D.print_graph graph

(** print to a file *)
let to_dot_file ~name egraph filename =
  let f = open_out filename in
  let content = to_dot ~name egraph in
  output_string f content;
  flush f;
  close_out f

