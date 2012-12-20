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
  let name = Format.sprintf "zipperposition_%d" (Unix.getpid ()) in
  Unix.ADDR_UNIX name

(** Process-localized debug *)
let ddebug level msg =
  if level <= Utils.debug_level ()
    then Format.printf "%% [%d] %s@." (Unix.getpid ()) (Lazy.force msg)
    else ()

let get_add_parents () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "add_parents" : (net_clause * net_clause list) Join.chan)

let get_add_proof () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "add_proof" : (net_clause * net_proof) Join.chan)

let get_get_parents () =
  let ns = Join.Ns.there socketname in
  (Join.Ns.lookup ns "get_parents" : net_clause -> net_clause list)

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

(** Create a queue. It returns a channel to send input objects (type 'a) in,
    and a channel to send a synchronous chan in to register to the queue.
    
    The semantics is that subscribers will receive messages in the correct
    order, and the queue will wait for all recipients to receive a message
    before it processes the next one. *)
let mk_queue () : 'a Join.chan * ('a -> unit) Join.chan =
  (* send message to all subscribers *)
  def send(x) & ready() & subscribers(l) =
    List.iter (fun s -> s(x)) l;
    ready() & subscribers(l)
  (* subscribe *)
  or  subscribe(s) & subscribers(l) =
    subscribers(s :: l)
  in
  spawn subscribers( [] ) & ready();
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

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

(** Publish proof and parents for this clause. and convert it
    to a net_clause *)
let get_convert_clause () =
  let add_parents = get_add_parents ()
  and add_proof = get_add_proof () in
  fun ~novel hc ->
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
    ddebug 1 (lazy (Utils.sprintf "%% new precedence @[<h>%a@]" T.pp_precedence precedence#snapshot));
    ignore (ord#set_precedence precedence)
  end

(** Update the passive set using the net_state *)
let update_passive ~select ~calculus ~ord passive_set net_state =
  let new_clauses = net_state.ns_new in
  let new_clauses = List.map (hclause_of_net_clause ~ord) new_clauses in
  let new_clauses = Utils.list_flatmap (calculus#list_simplify ~ord ~select) new_clauses in
  passive_set#add new_clauses

(** Access to global utils *)
type globals =
  < subscribe_redundant: net_clause list Join.chan -> unit;
    subscribe_exit: unit Join.chan -> unit;
    convert: novel:bool -> hclause -> net_clause;
    get_descendants: net_clause -> net_clause list;
    send_result: net_clause Sat.szs_status -> unit;
  >

(* ----------------------------------------------------------------------
 * components
 * ---------------------------------------------------------------------- *)

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
    | None -> genealogy.gen_proof <- proof  (* first proof *)
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

(** Create a component dedicated to simplification by rewriting/simplify-reflect.
    It takes as input:
    - get_ord:int -> ordering
    - output: net_state Join.chan
    and returns a synchronous input chan (net_state -> unit) *)
let rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx =
  let last_state = ref (-1) in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "process exiting..."); 0
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    simpl_set#remove clauses;
    ready()
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num > !last_state);
      last_state := net_state.ns_num;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        update_ord ~ord net_state;
        output(net_state) & ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        update_ord ~ord net_state;
        ddebug 1 (lazy (Utils.sprintf "rw_process processing given clause @[<h>%a@]"
                 !C.pp_clause#pp_h given));
        (* simplify given clause *)
        let simplified = calculus#rw_simplify ~select simpl_set given in
        let simplified = calculus#basic_simplify ~ord simplified in
        let novel = not (C.eq_hclause given simplified) in
        let net_state =
          if calculus#is_trivial simplified
          then { net_state with ns_given = None } (* stop processing this clause *)
          else begin
            simpl_set#add [simplified];  (* add the clause to active set *)
            let ns_new = globals#convert_clause ~novel simplified in
            { net_state with ns_given = Some ns_new }
          end
        in
        (* forward the state *)
        output(net_state) & ready()
      end
    end
  in
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  spawn ready();
  input

(** Create a component dedicated to simplification by the active set
    and by means of subsumption. It returns a synchronous input chan (net_state -> unit) *)
let active_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set *)
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() =
    ddebug 1 (lazy "process exiting..."); 0
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    active_set#remove clauses;
    ready()
  (* case in which we process the next clause *)
  or  ready() & input(net_state) =
    reply to input & begin
      assert (net_state.ns_num = !last_state + 1);
      last_state := net_state.ns_num;
      (* obtain given clause, and ordering *)
      match net_state.ns_given with
      | None ->
        update_ord ~ord net_state;
        output(net_state) & ready()
      | Some ns_given -> begin
        let given = hclause_of_net_clause ~ord ns_given in
        update_ord ~ord net_state;
        ddebug 1 (lazy (Utils.sprintf "active_process processing given clause @[<h>%a@]"
                 !C.pp_clause#pp_h given));
        (* simplify given clause *)
        let simplified = calculus#active_simplify ~select active_set given in
        let simplified = calculus#basic_simplify ~ord simplified in
        let novel = not (C.eq_hclause given simplified) in
        let net_state =
          (* check whether the clause is trivial or redundant *)
          if calculus#is_trivial simplified || calculus#redundant active_set simplified
          then { net_state with ns_given = None } (* stop processing this clause *)
          else begin
            active_set#add [simplified]; (* add clause to active_set *)
            let ns_new = globals#convert_clause ~novel simplified in
            { net_state with ns_given = Some ns_new }
          end
        in
        (* forward the state *)
        output(net_state) & ready()
      end
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
let passive_process ~calculus ~select ~ord ~output ~globals ?steps queues =
  let passive_set = PS.mk_passive_set ~ord queues in
  (* timestamp of last started loop, last finished loop *)
  let last_start, last_done = ref 0, ref (-1) in
  (* how many remaining steps? *)
  let steps_to_go = ref (match steps with None -> max_int | Some s -> s) in
  (* how to process new clauses *)
  let process_new net_state =
    let new_clauses = List.map (hclause_of_net_clause ~ord) net_state.ns_new in
    let new_clauses = List.filter (fun hc -> not (calculus#is_trivial hc)) new_clauses in
    passive_set#add new_clauses
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
  (* cases in which we exit *)
  def exit() =
    ddebug 1 (lazy "process exiting..."); 0
  (* process a new clause, the network is not full *)
  or  notfull() =
    assert (!steps_to_go >= 0 && !last_done <= !last_start);
    if !steps_to_go = 0
    then (ddebug 1 (lazy "reached max number of steps"); globals#send_result(Sat.Unknown))
    else match passive_set#next () with
    | None when !last_done = !last_start ->
      (* all clauses have been processed, none remains in passive *)
      globals#send_result(Sat.Sat)
    | None ->
      (* wait for an incoming result *)
      assert (!last_start > !last_done);
      wait ()
    | Some given ->
      decr steps_to_go;
      (* preprocess: list_simplify *)
      let old_ord_version = ord#precedence#version in
      (match calculus#list_simplify ~ord ~select given with
      | [] -> notfull()
      | given::others -> begin
        (* put others back in passive set, and send given in the pipeline! *)
        passive_set#add others;
        let nc_given = globals#convert_clause ~novel:true given in
        assert (ord#precedence#version >= old_ord_version);
        (* should we update the ordering? *)
        let ns_ord = if ord#precedence#version = old_ord_version
          then None
          else Some (List.map name_symbol ord#precedence#snapshot) in
        let net_state = {
          ns_num = !last_start;
          ns_ord;
          ns_given = Some nc_given;
          ns_simplified = [];
          ns_new = [];
        } in
        (* another clause is sent to the pipeline *)
        incr last_start;
        output(net_state);
        (* shall we wait for results to come before we continue? *)
        if !last_start - !last_done > !pipeline_capacity
          then full()
          else notfull()
      end)
  (* get result back while full *)
  or  input(net_state) & full() =
    reply to input & begin
      process_simplified net_state;
      process_new net_state;
      assert (net_state.ns_num = !last_done + 1);
      last_done := max !last_done net_state.ns_num;
      if !last_start - !last_done > !pipeline_capacity
        then (ddebug 1 (lazy "enter state full"); full())
        else (ddebug 1 (lazy "enter state full"); notfull())
    end
  (* get result back while not full yet (when the passive_set is empty) *)
  or  input(net_state) & wait() =
    reply to input & begin
      process_simplified net_state;
      process_new net_state;
      assert (net_state.ns_num = !last_done + 1);
      last_done := max !last_done net_state.ns_num;
      if !last_start - !last_done > !pipeline_capacity
        then (ddebug 1 (lazy "enter state full"); full())
        else (ddebug 1 (lazy "enter state full"); notfull())
    end
  in
  globals#subscribe_exit exit;
  spawn notfull();
  input

