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

open Types
open Symbols

module C = Clauses
module T = Terms
module S = FoSubst
module PS = ProofState
module Sat = Saturate
module Utils = FoUtils

(** Module for distributed (pipelined) given clause.

    Components are processes, that communicate through Unix sockets;
    every component has a unique name. Its output is "name_output".
    *)

(* ----------------------------------------------------------------------
 * basics
 * ---------------------------------------------------------------------- *)

type net_symbol = string
type net_sort = string

type net_term =
  | NVar of int * net_sort
  | NNode of net_symbol * net_sort * net_term list

(** A clause that can be sent through the network *)
type net_clause = {
  nc_maxlits : int;                 (** bitvector of maximal literals *)
  nc_selected : int;                (** bitvector of selected literals *)
  nc_lits : net_literal array;      (** array of literals *)
}
(** A serializable literal *)
and net_literal =
  | NEquation of net_term * net_term * bool * comparison

(** A proof that can be sent through the network (some loss of information)*)
type net_proof =
  | NpAxiom of string * string
  | NpProof of string * (net_clause * position) list

(** A state, threaded through the network *)
type net_state = {
  ns_num : int;                     (** unique timestamp of the state *)
  ns_ord : net_symbol list option;  (** update to new precedence, if needed *)
  ns_given : net_clause option;     (** the current given clause, if still useful *)
  ns_simplified : net_clause list;  (** clauses simplified, to remove from active set *)
  ns_new : net_clause list;         (** new clauses produced with given clause *)
}

(** Hash a net_clause *)
let hash_net_clause nc =
  let rec hash_nc_term acc t = match t with
   | NVar (i, s) -> Hashcons.combine2 acc (Utils.murmur_hash i) (Hashtbl.hash s)
   | NNode (f, s, l) ->
     Hashcons.combine3 331 (Hashtbl.hash f) (Hashtbl.hash s) (List.fold_left hash_nc_term acc l)
  and hash_nc_literal acc = function
  | NEquation (l,r,sign,_) ->
    Hashcons.combine3 acc (hash_nc_term 17 l) (hash_nc_term 401 r) (if sign then 14 else 15)
  in
  Array.fold_left hash_nc_literal 853 nc.nc_lits

(** Equality for net_clauses *)
let eq_net_clause nc1 nc2 =
  let rec check_lits l1 l2 i =
    if i = Array.length l1 then true else eq_lits l1.(i) l2.(i) && check_lits l1 l2 (i+1)
  and eq_lits lit1 lit2 = match lit1, lit2 with
  | NEquation (l1, r1, sign1, ord1), NEquation (l2, r2, sign2, ord2) ->
    sign1 = sign2 && ord1 = ord2 && eq_term l1 r1 && eq_term l2 r2
  and eq_term l r = match l, r with
  | NVar (i, si), NVar (j, sj) -> i = j && si = sj
  | NNode (f1, s1, l1), NNode (f2,s2,l2) -> f1 = f2 && s1 = s2 && List.for_all2 eq_term l1 l2
  | _ -> false
  in
  Array.length nc1.nc_lits = Array.length nc2.nc_lits && check_lits nc1.nc_lits nc2.nc_lits 0

(** Hashtable of net_clauses *)
module NHashtbl = Hashtbl.Make(struct type t = net_clause let hash = hash_net_clause let equal = eq_net_clause end)

(** Merge two states that have the same timestamp. *)
let merge_states s1 s2 =
  assert (s1.ns_num = s2.ns_num);
  assert (s1.ns_ord = s2.ns_ord);
  { s1 with
    ns_simplified = List.rev_append s1.ns_simplified s2.ns_simplified;
    ns_new = List.rev_append s1.ns_new s2.ns_new;
  }

(* ----------------------------------------------------------------------
 * conversion functions
 * ---------------------------------------------------------------------- *)

let rec term_of_net_term nt = match nt with
  | NVar (i,s) -> T.mk_var i (mk_symbol s)
  | NNode (f, s, l) ->
    let f = mk_symbol f in
    let sort = mk_symbol s in
    T.mk_node f sort (List.map term_of_net_term l)
and literal_of_net_literal nlit = match nlit with
  | NEquation (l,r,sign,cmp) ->
    let l = term_of_net_term l
    and r = term_of_net_term r in
    assert (l.sort == r.sort);
    Equation (l, r, sign, cmp)
and proof_of_net_proof ~ord np = match np with
  | NpAxiom (s1, s2) -> Axiom (s1, s2)
  | NpProof (rule, clauses) ->
    let clauses = List.map (fun (nc, pos) ->
                            C.base_clause (hclause_of_net_clause ~ord nc), pos, S.id_subst) clauses in
    Proof (rule, clauses)
and hclause_of_net_clause ~ord nc =
  let lits = Array.map literal_of_net_literal nc.nc_lits in
  let proof = Axiom ("network", "network") in
  C.mk_hclause_raw ~maxlits:nc.nc_maxlits ~selected:nc.nc_selected lits proof []
  
let rec net_term_of_term t = match t.term with
  | Var i -> NVar (i, name_symbol t.sort)
  | Node (f, l) ->
    NNode (name_symbol f, name_symbol t.sort, List.map net_term_of_term l)
and net_literal_of_literal lit = match lit with
  | Equation (l,r,sign,cmp) -> NEquation (net_term_of_term l, net_term_of_term r, sign, cmp)
and net_proof_of_proof proof = match proof with
  | Axiom (s1,s2) -> NpAxiom (s1, s2)
  | Proof (rule, clauses) ->
    let clauses = List.map (fun (c,pos,_) -> net_clause_of_hclause c.cref, pos) clauses in
    NpProof (rule, clauses)
and net_clause_of_hclause hc =
  assert hc.hcselected_done; (* must select before serialize *)
  let lits = Array.map net_literal_of_literal hc.hclits in
  { nc_maxlits = hc.hcmaxlits;
    nc_selected = hc.hcselected;
    nc_lits = lits; }

(* ----------------------------------------------------------------------
 * access to global variables
 * ---------------------------------------------------------------------- *)

(** name for the communication socket *)
let socketname =
  let name = Format.sprintf "/tmp/zipperposition_%d" (Unix.getpid ()) in
  Unix.ADDR_UNIX name

(** Process-localized debug *)
let ddebug level msg =
  if level <= Utils.debug_level ()
    then Format.printf "%% [%d at %.2f] %s@." (Unix.getpid ())
      (Sat.get_total_time ()) (Lazy.force msg)
    else ()

let get_add_parents () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "add_parents" : (net_clause * net_clause list) Join.chan)

let get_add_proof () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "add_proof" : (net_clause * net_proof) Join.chan)

let get_get_descendants () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "get_descendants" : net_clause -> net_clause list)

let get_get_proof () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "get_proof" : net_clause -> net_proof option)

let get_publish_redundant () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "publish_redundant" : net_clause list Join.chan)

let get_subscribe_redundant () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "subscribe_redundant" : net_clause list Join.chan -> unit)
  
let get_subscribe_exit () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "subscribe_exit" : unit Join.chan -> unit)

let get_send_result () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "send_result" : net_clause Sat.szs_status * int -> unit)

(** Create a queue. It returns a channel to send input objects (type 'a) in,
    and a channel to send a synchronous chan in to register to the queue.
    
    The semantics is that subscribers will receive messages in the correct
    order, and the queue will wait for all recipients to receive a message
    before it processes the next one. *)
let mk_queue () : ('a -> unit) * (('a -> unit) -> unit) =
  (* put message in queue *)
  def send(x) & waiting(l) =
    waiting(l @ [x]) & reply to send
  (* forward first message to all subscribers *)
  or waiting(x::l) & subscribers(subs) & ready() =
    waiting(l) & subscribers(subs) & begin
      List.iter (fun sub -> sub x) subs;  (* wait for all subscribers to receive x *)
      ready()
    end
  (* subscribe *)
  or  subscribe(s) & subscribers(subs) =
    subscribers(s :: subs) & reply to subscribe
  in
  spawn subscribers([]) & waiting([]) & ready();
  (* return components of the queue *)
  send, subscribe

(** Same as mk_queue, but registers (send, subscribe) with
    the given global name. *)
let mk_global_queue name = 
  let send, subscribe = mk_queue () in
  (* register global name *)
  let ns = Join.Ns.there socketname in
  Join.Ns.register ns name (send, subscribe);
  send, subscribe

(** Get a handle on the remote queue by name *)
let get_queue name =
  let ns = Join.Ns.there socketname in
  Join.Ns.lookup ns name

(** Barrier for n incoming messages. It waits for n incoming messages, merge
    them using [merge], then sends them to all registered outputs *)
let mk_barrier ~merge n : ('a -> unit) * (('a -> unit) -> unit) =
  assert (n > 0); failwith "not implemented"  (* TODO *)

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

(** Publish proof and parents for this clause. and convert it
    to a net_clause *)
let convert_clause add_parents add_proof ~novel hc =
  (* convert into serializable form *)
  let nc = net_clause_of_hclause hc in
  (if novel then
    let proof = net_proof_of_proof hc.hcproof in
    let parents = List.map net_clause_of_hclause hc.hcparents in
    (* send messages *)
    spawn add_parents(nc, parents) & add_proof(nc, proof));
  nc

(** Update the ordering, using the net_state *)
let update_ord ~ord net_state =
  match net_state.ns_ord with
  | None -> ()  (* up to date *)
  | Some symbols -> begin
    let symbols = List.map mk_symbol symbols in
    let constr = Precedence.list_constraint symbols in
    let precedence = Precedence.mk_precedence [constr] symbols in
    ddebug 1 (lazy (Utils.sprintf "%% new precedence @[<h>%a@]"
              T.pp_precedence precedence#snapshot));
    ignore (ord#set_precedence precedence)
  end

(** Update the ordering of the net_state. The initial length of the ordering
    is [old_version], and if the new ordering contains more symbols,
    the net_state.ns_ord is updated. *)
let update_precedence ~ord old_version =
  if ord#precedence#version = old_version
    then None
    else Some (List.map name_symbol ord#precedence#snapshot)

(** Update the passive set using the net_state *)
let update_passive ~select ~calculus ~ord passive_set net_state =
  let new_clauses = net_state.ns_new in
  let new_clauses = List.map (hclause_of_net_clause ~ord) new_clauses in
  let new_clauses = Utils.list_flatmap (calculus#list_simplify ~ord ~select) new_clauses in
  passive_set#add new_clauses

(** Access to global utils *)
type globals =
  < subscribe_redundant: (net_clause list -> unit) -> unit;
    publish_redundant: net_clause list -> unit;
    subscribe_exit: (unit -> unit) -> unit;
    publish_exit: unit -> unit;
    convert: novel:bool -> hclause -> net_clause;
    get_descendants: net_clause -> net_clause list;
    send_result: net_clause Saturate.szs_status * int -> unit;
  >

(** Structure used to keep track of parent/descendants and clause/proof
    relationships *)
type genealogy = {
  mutable gen_descendants: net_clause list;
  mutable gen_proof: net_proof option;
}

let empty_genealogy = { gen_descendants = []; gen_proof = None; }

(** The process responsible for keeping track of
    parents and proofs of clauses *)
let proof_parents_process () =
  let genealogies = NHashtbl.create 109 in
  (* get genealogy for this clause *)
  let rec get_genealogy c =
    try NHashtbl.find genealogies c
    with Not_found ->
      let it = empty_genealogy in
      NHashtbl.add genealogies c it;
      it
  (* add a descendant to the clause *)
  and add_descendant parent child =
    let genealogy = get_genealogy parent in
    genealogy.gen_descendants <- child :: genealogy.gen_descendants
  (* add a proof to the clause *)
  and maybe_add_proof clause proof =
    let genealogy = get_genealogy clause in
    match genealogy.gen_proof with
    | None -> genealogy.gen_proof <- Some proof  (* first proof *)
    | Some _ -> ()  (* already a proof, do nothing *)
  in 
  def ready() & add_parents(clause, parents) =
    List.iter (fun p -> add_descendant p clause) parents; ready()
  or  ready() & add_proof(clause, proof) =
    maybe_add_proof clause proof; ready()
  or  ready() & get_descendants(clause) =
    let genealogy = get_genealogy clause in
    reply genealogy.gen_descendants to get_descendants & ready ()
  or  ready() & get_proof(clause) =
    let genealogy = get_genealogy clause in
    reply genealogy.gen_proof to get_proof & ready ()
  in
  spawn ready();
  add_parents, add_proof, get_descendants, get_proof

(** connects the given input to queues whose names are in the list. *)
let connect input queues =
  (* lookup a queue by name *)
  let ns = Join.Ns.there socketname in
  List.iter
    (fun queue ->
      let _, subscribe =
        (Join.Ns.lookup ns queue : net_state Join.chan *
                                  ((net_state -> unit) -> unit))
      in
      subscribe input)
    queues

(** Setup global components in this process. Also
    setup the network server. *)
let setup_globals send_result =
  ddebug 1 (lazy "setup globals");
  (* listen on the address *)
  Join.Site.listen socketname;
  (* create global queues *)
  let exit, sub_exit = mk_global_queue "exit" in
  let redundant, sub_redundant = mk_global_queue "redundant" in
  let add_parents, add_proof, get_descendants, get_proof =
    proof_parents_process () in
  (* register global channels *)
  let ns = Join.Ns.there socketname in
  Join.Ns.register ns "add_parents" add_parents;
  Join.Ns.register ns "add_proof" add_proof;
  Join.Ns.register ns "get_descendants" get_descendants;
  Join.Ns.register ns "get_proof" get_proof;
  Join.Ns.register ns "send_result" send_result;
  (object
    method subscribe_redundant = sub_redundant
    method publish_redundant = redundant
    method subscribe_exit = sub_exit
    method publish_exit = exit
    method convert ~novel hc = convert_clause add_parents add_proof ~novel hc 
    method get_descendants = get_descendants
    method get_proof = get_proof
    method send_result = send_result
  end :> globals)

(** Create a globals object, using (possibly remote) components *)
let get_globals () =
  (* register global channels *)
  let add_parents = get_add_parents () in
  let add_proof = get_add_proof () in
  let get_descendants = get_get_descendants () in
  let get_proof = get_get_proof () in
  let send_result = get_send_result () in
  let redundant, sub_redundant = get_queue "redundant" in
  let exit, sub_exit = get_queue "exit" in
  (object
    method subscribe_redundant = sub_redundant
    method publish_redundant = redundant
    method subscribe_exit = sub_exit
    method publish_exit = exit
    method convert ~novel hc = convert_clause add_parents add_proof ~novel hc 
    method get_descendants = get_descendants
    method get_proof = get_proof
    method send_result = send_result
  end :> globals)

(* ----------------------------------------------------------------------
 * components
 * ---------------------------------------------------------------------- *)

(** Create a component dedicated to simplification by rewriting/simplify-reflect.
    It takes as input:
    - get_ord:int -> ordering
    - output: net_state Join.chan
    and returns a synchronous input chan (net_state -> unit) *)
let fwd_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx =
  let last_state = ref (-1) in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "rw_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    simpl_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      let simplified = List.map (hclause_of_net_clause ~ord) net_state.ns_simplified in
      simpl_set#remove simplified;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        output net_state; ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        ddebug 1 (lazy (Utils.sprintf "rw_process processing given clause @[<h>%a@]"
                 !C.pp_clause#pp_h given));
        (* simplify given clause *)
        let new_given = calculus#rw_simplify ~select simpl_set given in
        let new_given = calculus#basic_simplify ~ord new_given in
        let novel = not (C.eq_hclause given new_given) in
        (* old given clause is redundant *)
        (if novel then spawn (globals#publish_redundant [ns_given]; 0));
        (* forward the given clause and the state *)
        let net_state =
          if calculus#is_trivial new_given
          then { net_state with ns_given = None } (* stop processing this clause *)
          else begin
            simpl_set#add [new_given];  (* add the clause to active set *)
            let ns_new = globals#convert ~novel new_given in
            { net_state with ns_given = Some ns_new }
          end
        in
        (* forward the state *)
        output(net_state);
        ready()
      end
    end
  in
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component dedicated to simplification by the active set
    and by means of subsumption. It returns a synchronous input chan (net_state -> unit) *)
let fwd_active_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "active_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    active_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        output net_state; ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        ddebug 1 (lazy (Utils.sprintf "active_process processing given clause @[<h>%a@]"
                 !C.pp_clause#pp_h given));
        (* simplify given clause *)
        let simplified = calculus#active_simplify ~select active_set given in
        let simplified = calculus#basic_simplify ~ord simplified in
        let novel = not (C.eq_hclause given simplified) in
        (* old given clause is redundant *)
        (if novel then spawn (globals#publish_redundant [ns_given]; 0));
        let net_state =
          (* check whether the clause is trivial or redundant *)
          if calculus#is_trivial simplified || calculus#redundant active_set simplified
          then { net_state with ns_given = None } (* stop processing this clause *)
          else begin
            active_set#add [simplified]; (* add clause to active_set *)
            let ns_new = globals#convert ~novel simplified in
            { net_state with ns_given = Some ns_new }
          end
        in
        (* forward the state *)
        output net_state;
        ready()
      end
    end
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component dedicated to removal of active clauses that
    are subsumed by the given clause. *)
let bwd_subsume_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "bwd_subsume_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    active_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      let simplified = List.map (hclause_of_net_clause ~ord) net_state.ns_simplified in
      active_set#remove simplified;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        output net_state; ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        if not (calculus#is_trivial given) then begin
          ddebug 1 (lazy (Utils.sprintf "bwd_subsume processing given clause @[<h>%a@]"
                   !C.pp_clause#pp_h given));
          (* find subsumed clauses *)
          let subsumed = calculus#backward_redundant active_set given in
          (* add clause to active set *)
          active_set#add [given];
          (* global broadcast that those close are redundant, and forward state *)
          let subsumed = List.map net_clause_of_hclause subsumed in
          let net_state = {net_state with ns_simplified = subsumed @ net_state.ns_simplified } in
          output net_state;
          ready() & (globals#publish_redundant subsumed; 0)
        end else begin
          output net_state;
          ready()
        end
      end
    end
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component dedicated to simplification of the active set
    by rewriting using the given clause *)
let bwd_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "bwd_rewrite_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    simpl_set#remove clauses;
    active_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      let simplified = List.map (hclause_of_net_clause ~ord) net_state.ns_simplified in
      active_set#remove simplified;
      simpl_set#remove simplified;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        output net_state; ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        if not (calculus#is_trivial given) then begin
          ddebug 1 (lazy (Utils.sprintf "bwd_rewrite processing given clause @[<h>%a@]"
                   !C.pp_clause#pp_h given));
          (* rewrite some clauses *)
          simpl_set#add [given];
          let simplified, newly_simplified = Sat.backward_simplify ~select
            ~calculus active_set simpl_set given in
          (* add given to active set *)
          active_set#add [given];
          (* forward information *)
          let simplified = C.CSet.to_list simplified in
          let simplified = List.map net_clause_of_hclause simplified in
          let newly_simplified = List.map (globals#convert ~novel:true) newly_simplified in
          (* put simplified clause in ns_simplified, and their simplification
             in ns_new *)
          let net_state = {net_state with
            ns_simplified = simplified @ net_state.ns_simplified;
            ns_new = newly_simplified @ net_state.ns_new } in
          output net_state;
          ready()
        end else begin
          output net_state;
          ready()
        end
      end
    end
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component that performs generating inferences *)
let generate_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "generate_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    reply to redundant & begin
      let clauses = List.map (hclause_of_net_clause ~ord) clauses in
      active_set#remove clauses;
      ready()
    end
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      let simplified = List.map (hclause_of_net_clause ~ord) net_state.ns_simplified in
      active_set#remove simplified;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        output net_state; ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        if not (calculus#is_trivial given) then begin
          ddebug 1 (lazy (Utils.sprintf "generate processing given clause @[<h>%a@]"
                   !C.pp_clause#pp_h given));
          (* add given to active set *)
          active_set#add [given];
          (* perform inferences *)
          let new_clauses = Sat.generate ~calculus active_set given in
          let new_clauses = List.map (globals#convert ~novel:true) new_clauses in
          let net_state = {net_state with ns_new = new_clauses @ net_state.ns_new } in
          output net_state;
          ready()
        end else begin
          output net_state;
          ready()
        end
      end
    end
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component dedicated to RW simplification of newly generated clauses by
    the active set. *)
let post_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx =
  let last_state = ref (-1) in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "new_rewrite_process exiting..."); reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    simpl_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      update_ord ~ord net_state;
      let simplified = List.map (hclause_of_net_clause ~ord) net_state.ns_simplified in
      simpl_set#remove simplified;
      (* add given clause to active set *)
      (match net_state.ns_given with
      | Some ns_given ->
        let given = hclause_of_net_clause ~ord ns_given in
        simpl_set#add [given]
      | None -> ());
      (* current version of ordering *)
      let old_ord_version = ord#precedence#version in
      ddebug 1 (lazy (Utils.sprintf "new_rewrite processing %d clauses"
               (List.length net_state.ns_new)));
      let new_clauses = List.map (hclause_of_net_clause ~ord) net_state.ns_new in
      let new_clauses = List.fold_left
        (fun acc hc ->
          let cs = calculus#list_simplify ~ord ~select hc in
          let cs = List.map (calculus#rw_simplify ~select simpl_set) cs in
          List.rev_append cs acc)
        [] new_clauses
      in 
      (* put simplified clauses back in the state *)
      let new_clauses = List.map net_clause_of_hclause new_clauses in
      (* maybe we just created new symbols, forward them *)
      let ns_ord = update_precedence ~ord old_ord_version in
      let net_state = {net_state with ns_ord; ns_new = new_clauses } in
      output(net_state);
      ready()
    end
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Number of clauses being processed at the same time *)
let pipeline_capacity = ref 10

(** Create a component dedicated to maintaining the passive set. It also
    performs list_simplify on clauses, and is therefore the only process
    to modify the ordering.
    The [result] argument is a szs_status Join.chan on which the answer is
    sent. *)
let passive_process ~calculus ~select ~ord ~output ~globals ?steps ?timeout queues clauses =
  (* initialize passive set with the given clauses *)
  let passive_set = PS.mk_passive_set ~ord queues in
  passive_set#add (List.map (hclause_of_net_clause ~ord) clauses);
  (* timestamp of last started loop, last finished loop, last transmitted ord *)
  let last_start= ref 0 in
  let last_done = ref (-1) in
  let last_ord_version = ref 0 in
  (* how many remaining steps? *)
  let steps_to_go = ref (match steps with None -> max_int | Some s -> s) in
  (* how to process new clauses. Returns Some hc if some hc is empty *)
  let process_new net_state =
    (* look for the empty clause *)
    let empty =
      try Some (List.find (fun nc -> nc.nc_lits = [||]) net_state.ns_new)
      with Not_found -> None
    in
    (* add new clauses to passive set *)
    let new_clauses = List.map (hclause_of_net_clause ~ord) net_state.ns_new in
    let new_clauses = List.filter (fun hc -> not (calculus#is_trivial hc)) new_clauses in
    passive_set#add new_clauses;
    empty
  (* how to process simplified clauses: remove their orphans (needs to get the
     descendants and add them to the hclause explicitely before) *)
  and process_simplified net_state =
    let simplified_descendants = List.map
      (fun nc -> nc, globals#get_descendants nc) net_state.ns_simplified in
    let simplified = List.map
      (fun (nc, descendants) ->
        let hc = hclause_of_net_clause ~ord nc in
        List.iter (fun nc' -> let hc' = hclause_of_net_clause ~ord nc in
          hc.hcdescendants <- Ptset.add hc'.hctag hc.hcdescendants)
        descendants;
        hc)
      simplified_descendants
    in
    Sat.remove_orphans passive_set simplified
  in
  (* trigger to start looping *)
  def start () = notfull() & reply to start
  (* cases in which we exit (TODO react with full(), notfull() or wait()? *)
  or  exit() =
    ddebug 1 (lazy "process exiting..."); reply to exit
  (* process a new clause, the network is not full *)
  or  notfull() =
    assert (!steps_to_go >= 0 && !last_done <= !last_start);
    if !steps_to_go = 0
    then (ddebug 1 (lazy "reached max number of steps");
          globals#send_result (Sat.Unknown,!last_done); 0)
    else if not (Sat.check_timeout timeout)
    then (ddebug 1 (lazy "reached timeout");
           globals#send_result (Sat.Unknown, !last_done); 0)
    else match passive_set#next () with
    | None when !last_done = !last_start ->
      (* all clauses have been processed, none remains in passive *)
      globals#send_result (Sat.Sat,!last_done); 0
    | None ->
      (* wait for an incoming result *)
      assert (!last_start > !last_done);
      wait ()
    | Some given ->
      (* preprocess: list_simplify *)
      (match calculus#list_simplify ~ord ~select given with
      | [] -> notfull()
      | given::others -> begin
        let nc_given = globals#convert ~novel:true given in
        assert (ord#precedence#version >= !last_ord_version);
        let net_state = {
          ns_num = !last_start;
          (* update the precedence of the pipeline if needed *)
          ns_ord = update_precedence ~ord !last_ord_version;
          ns_given = Some nc_given;
          ns_simplified = [];
          ns_new = List.map (globals#convert ~novel:true) others;
        } in
        (* another clause is sent to the pipeline *)
        incr last_start;
        last_ord_version := ord#precedence#version;
        output net_state;
        (* shall we wait for results to come before we continue? *)
        if !last_start - !last_done > !pipeline_capacity
          then full()
          else notfull()
      end)
  (* get result back while full *)
  or  input(net_state) & full() =
    reply to input & process_input(net_state)
  (* get result back while not full yet (when the passive_set is empty) *)
  or  input(net_state) & wait() =
    reply to input & process_input(net_state)
  or  input(net_state) & notfull() =
    reply to input & process_input(net_state)
  (* process an input clause *)
  or  process_input(net_state) =
    update_ord ~ord net_state;
    process_simplified net_state;
    (match process_new net_state with
    | None -> ()
    | Some nc -> globals#send_result (Sat.Unsat nc, !last_done));
    assert (net_state.ns_num = !last_done + 1);
    last_done := net_state.ns_num;
    decr steps_to_go;
    if !last_start - !last_done > !pipeline_capacity
      then (ddebug 1 (lazy "enter state full"); full())
      else (ddebug 1 (lazy "enter state full"); notfull())
  in
  globals#subscribe_exit exit;
  input, start

(** Create a pipeline within the same process, without forking *)
let layout_one_process ~calculus ~select ~ord ~globals ?steps ?timeout clauses queues signature =
  ddebug 0 (lazy "use single-process pipeline layout");
  let unit_idx = Dtree.unit_index in
  let index = PS.choose_index "fp" in
  (* passive set *)
  let q1_in, q1_subscribe = mk_queue () in
  let passive_in, start = passive_process ~calculus ~select ~ord ~output:q1_in
    ~globals ?steps ?timeout queues clauses in
  (* forward rewriting *)
  let q2_in, q2_subscribe = mk_queue () in
  let fwd_rw_in = fwd_rewrite_process ~calculus ~select ~ord ~output:q2_in
    ~globals unit_idx in
  q1_subscribe fwd_rw_in;
  (* forward subsumption *)
  let q3_in, q3_subscribe = mk_queue () in
  let fwd_subsume_in = fwd_active_process ~calculus ~select ~ord ~output:q3_in
    ~globals index signature in
  q2_subscribe fwd_subsume_in;
  (* backward subsumption *)
  let q4_in, q4_subscribe = mk_queue () in
  let bwd_subsume_in = bwd_subsume_process ~calculus ~select ~ord ~output:q4_in
    ~globals index signature in
  q3_subscribe bwd_subsume_in;
  (* backward rewriting *)
  let q5_in, q5_subscribe = mk_queue () in
  let bwd_rewrite_in = bwd_rewrite_process ~calculus ~select ~ord ~output:q5_in
    ~globals unit_idx index signature in
  q4_subscribe bwd_rewrite_in;
  (* generate *)
  let q6_in, q6_subscribe = mk_queue () in
  let generate_in = generate_process ~calculus ~select ~ord ~output:q6_in
    ~globals index signature in
  q5_subscribe generate_in;
  (* post-simplify *)
  let q7_in, q7_subscribe = mk_queue () in
  let post_rw_in = post_rewrite_process ~calculus ~select ~ord ~output:q7_in
    ~globals unit_idx in
  q6_subscribe post_rw_in;
  (* close the loop *)
  q7_subscribe passive_in;
  (* start *)
  ddebug 0 (lazy "%% start the single-process pipeline");
  start ()

(** Create a pipeline with several forked processes *)
let layout_standard ~calculus ~select ~ord ~globals ?steps ?timeout clauses queues signature =
  ()

(** Rebuild a hclause with all its proof *)
let rebuild_proof ~ord ~globals nc =
  hclause_of_net_clause ~ord nc  (* TODO *)

(** run the given clause until a timeout occurs or a result
    is found. It returns the result. *)
let given_clause ?(parallel=true) ?steps ?timeout ?(progress=false) ~calculus state =
  let ord = state#ord
  and select = state#select
  and clauses = C.CSet.to_list state#passive_set#clauses in
  def send_result(result,num) & wait() =
    ddebug 1 (lazy "got result from pipeline");
    reply (result,num) to wait & reply to send_result
  in
  ddebug 0 (lazy "start pipelined given clause");
  (* TODO give queues as a parameter? *)
  let queues = ClauseQueue.default_queues in
  let signature = C.signature clauses in
  (* setup globals *)
  let globals = setup_globals send_result in
  (* convert clauses to net_clause *)
  ddebug 1 (lazy "convert clauses");
  let clauses = List.map (globals#convert ~novel:true) clauses in
  (* create the pipeline and start processing *)
  let layout = if parallel then layout_standard else layout_one_process in
  layout ~calculus ~select ~ord ~globals ?steps ?timeout clauses queues signature;
  (* wait for a result *)
  match wait () with
  | Sat.Unsat nc, num ->
    (* reconstruct a hclause with its proof *)
    let hc = rebuild_proof ~ord ~globals nc in
    Sat.Unsat hc, num
  | Sat.Sat, num -> Sat.Sat, num
  | Sat.Unknown, num -> Sat.Unknown, num
  | Sat.Error e, num -> Sat.Error e, num
  | Sat.Timeout, num -> Sat.Timeout, num

