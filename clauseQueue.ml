(* heuristic selection of clauses *)

module T = Terms

(* a queue of clauses *)
class type queue =
  object
    method add : Terms.clause -> queue
    method is_empty: bool
    method take_first : (queue * Terms.clause)
  end

module type HeapQueueOrd = Leftistheap.Ordered with type t = T.clause
(* generic clause queue based on some ordering on clauses *)
module HeapQueue(Ord : HeapQueueOrd) =
  struct
    module H = Leftistheap.Make(Ord)

    (* the functor contains a class implementation *)
    class q =
      object (self)
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
    type t = T.clause
    let le c1 c2 = c1.T.cid <= c2.T.cid
  end)

class fifo = FifoQueue.q

(* select by increasing weight of clause *)
module ClauseWeight = HeapQueue(
  struct
    type t = T.clause
    let le c1 c2 =
      let w1 = Orderings.compute_clause_weight c1
      and w2 = Orderings.compute_clause_weight c2 in
      w1 <= w2
  end)

class clause_weight = ClauseWeight.q

let default_queues =
  [ (new clause_weight, 5);
    (new fifo, 2);
  ]
