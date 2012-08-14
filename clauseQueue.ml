(* heuristic selection of clauses *)

open Types
open Hashcons

module O = Orderings

(* a queue of clauses *)
class type queue =
  object
    method add : hclause -> queue
    method is_empty: bool
    method take_first : (queue * hclause)
    method remove : hclause list -> queue  (* slow *)
    method name : string
  end

module LH = Leftistheap

type clause_ord = hclause LH.ordered

(* generic clause queue based on some ordering on clauses *)
class hq (ord : clause_ord) (name : string) : queue =
  object
    val heap = new LH.leftistheap ord
    
    method is_empty = heap#is_empty

    method add hc =
      let new_heap = heap#insert hc in
      ({< heap = new_heap >} :> queue)

    method take_first =
      assert (not (heap#is_empty));
      let c,new_h = heap#extract_min in
      (({< heap = new_h >} :> queue), c)

    method remove hclauses =
      match hclauses with
      | [] -> ({< >} :> queue)
      | _ ->  ({< heap = heap#remove hclauses >} :> queue)

    method name = name
  end

(* select by increasing age (for fairness) *)
let fifo ~ord =
  let clause_ord =
    object
      method le hc1 hc2 =  hc1.tag <= hc2.tag
    end
  and name = "fifo_queue" in
  new hq clause_ord name

(* select by increasing weight of clause *)
let clause_weight ~ord =
  let clause_ord =
    object
      method le hc1 hc2 =
        let w1 = ord#compute_clause_weight hc1.node
        and w2 = ord#compute_clause_weight hc2.node in
        w1 <= w2
    end
  and name = "clause_weight" in
  new hq clause_ord name
  

let default_queues ~ord =
  [ (clause_weight ~ord, 5);
    (fifo ~ord, 2);
  ]
