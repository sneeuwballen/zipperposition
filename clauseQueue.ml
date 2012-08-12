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
  end

module type HeapQueueOrd = Leftistheap.Ordered with type t = hclause

(* generic clause queue based on some ordering on clauses *)
module HeapQueue(Ord : HeapQueueOrd) =
  struct
    module H = Leftistheap.Make(Ord)

    (* the functor contains a class implementation *)
    class q =
      object
        val heap = H.empty
        
        method is_empty = H.is_empty heap

        method add clause =
          let new_heap = H.insert clause heap in
          ({< heap = new_heap >} :> queue)

        method take_first =
          assert (not (H.is_empty heap));
          let c,new_h = H.extract_min heap in
          (({< heap = new_h >} :> queue), c)
      end
  end

(* select by increasing age (for fairness) *)
module FifoQueue = HeapQueue(
  struct
    type t = hclause
    let le hc1 hc2 = hc1.hkey <= hc2.hkey
  end)

class fifo = FifoQueue.q

(* select by increasing weight of clause *)
module ClauseWeight = HeapQueue(
  struct
    type t = hclause
    let le hc1 hc2 =
      let w1 = O.compute_clause_weight hc1.node
      and w2 = O.compute_clause_weight hc2.node in
      w1 <= w2
  end)

class clause_weight = ClauseWeight.q

let default_queues =
  [ (new clause_weight, 5);
    (new fifo, 2);
  ]
