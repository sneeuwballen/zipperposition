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
open Params

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

let prof_hc_of_nc = Utils.mk_profiler "hc_of_nc"
let prof_nc_of_hc = Utils.mk_profiler "nc_of_hc"

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
  nc_selected_done : bool;          (** are literals already selected? *)
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
  Utils.enter_prof prof_hc_of_nc;
  let lits = Array.map literal_of_net_literal nc.nc_lits in
  let proof = Axiom ("network", "network") in
  let c = C.mk_hclause_raw ~maxlits:nc.nc_maxlits ~selected:nc.nc_selected
    ~selected_done:nc.nc_selected_done lits proof [] in
  Utils.exit_prof prof_hc_of_nc;
  c
  
let rec net_term_of_term t = match t.term with
  | Var i -> NVar (i, name_symbol t.sort)
  | Node (f, l) ->
    NNode (name_symbol f, name_symbol t.sort, List.map net_term_of_term l)
and net_literal_of_literal lit = match lit with
  | Equation (l,r,sign,cmp) -> NEquation (net_term_of_term l, net_term_of_term r, sign, cmp)
and net_proof_of_proof proof = match proof with
  | Axiom (s1,s2) -> NpAxiom (s1, s2)
  | Proof (rule, clauses) ->
    let clauses = List.map (fun (c,pos,_) ->
      net_clause_of_hclause ~selected:false c.cref, pos) clauses in
    NpProof (rule, clauses)
and net_clause_of_hclause ?(selected=true) hc =
  Utils.enter_prof prof_nc_of_hc;
  (if selected then Selection.check_selected hc);
  let lits = Array.map net_literal_of_literal hc.hclits in
  let nc =
  { nc_maxlits = hc.hcmaxlits;
    nc_selected = hc.hcselected;
    nc_selected_done = selected;
    nc_lits = lits; } in
  Utils.exit_prof prof_nc_of_hc;
  nc

(* ----------------------------------------------------------------------
 * access to global variables
 * ---------------------------------------------------------------------- *)

let socketname_file = 
  Format.sprintf "/tmp/zipperposition_%d" (Unix.getpid ())

(** name for the communication socket *)
let socketname = Unix.ADDR_UNIX socketname_file

(** Process-localized debug *)
let ddebug level who msg =
  if level <= Utils.debug_level ()
    then Format.printf "%% [%.3f %s]: %s@." (Sat.get_total_time ()) who (Lazy.force msg)
    else ()

let get_add_parents ns =
  (Join.Ns.lookup ns "add_parents" : (net_clause * net_clause list) Join.chan)

let get_add_proof ns =
  (Join.Ns.lookup ns "add_proof" : (net_clause * net_proof) Join.chan)

let get_get_descendants ns =
  (Join.Ns.lookup ns "get_descendants" : net_clause -> net_clause list)

let get_get_proof ns =
  (Join.Ns.lookup ns "get_proof" : net_clause -> net_proof option)

let get_publish_redundant ns =
  (Join.Ns.lookup ns "publish_redundant" : net_clause list Join.chan)

let get_subscribe_redundant ns =
  (Join.Ns.lookup ns "subscribe_redundant" : net_clause list Join.chan -> unit)
  
let get_subscribe_exit ns =
  (Join.Ns.lookup ns "subscribe_exit" : unit Join.chan -> unit)

let get_send_result ns =
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
let mk_global_queue ~ns name = 
  let send, subscribe = mk_queue () in
  (* register global name *)
  Join.Ns.register ns name (send, subscribe);
  send, subscribe

(** Get a handle on the remote queue by name *)
let get_queue ~ns name =
  Join.Ns.lookup ns name

(** Join for n incoming messages. It waits for n incoming messages, merge
    them using [merge], then sends them to all registered outputs *)
let mk_join ~merge n : ('a -> unit) * (('a -> unit) -> unit) =
  assert (n > 0); failwith "not implemented"  (* TODO *)

(** Make a global barrier to synchronize [n] processes. The arguments
    are the (global) name, and the number of processes involved. The
    semantics is: [mk_barrier name n] registers a barrier on given name;
    It returns a function sync which is blocking until [n] calls to [sync]
    have been made. Then [sync] is ready for [n] other calls, and so on. *)
let mk_barrier ~ns name n =
  (* count [n] syncs *)
  def sync(process_name) & count(n') =
    (if n' > 1 then begin
      (* wait for other processes *)
      ddebug 1 process_name (lazy (Utils.sprintf "wait for %d other processes" (n'-1)));
      spawn count(n'-1);
      wait ();
    end else begin
      (* unlock the n-1 other processes that are waiting *)
      for i = 1 to n-1 do
        cross ();
      done;
      (* start next barrier *)
      spawn count(n);
    end);
    reply to sync
  or wait() & cross() = reply to wait & reply to cross
  in
  spawn count(n);
  (* register *)
  Join.Ns.register ns name sync;
  sync

(** Get the barrier registered under this name *)
let get_barrier ~ns name =
  (Join.Ns.lookup ns name : string -> unit)

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
    let parents = List.map (net_clause_of_hclause ~selected:false) hc.hcparents in
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
    sync_barrier: string -> unit;
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

(** Setup global components in this process. Also setup the network server.
    [num] is the number of components. *)
let setup_globals send_result num =
  ddebug 1 "globals" (lazy "setup globals");
  (* listen on the address; remove the socket upon exit *)
  (try Unix.unlink socketname_file with Unix.Unix_error _ -> ());
  (try Join.Site.listen socketname;
       Format.printf "%% listen on socket %s@." socketname_file
  with Failure f -> begin
    Format.printf "%% error trying to listen on %s: %s@." socketname_file f;
    (exit 1 : unit);
  end);
  at_exit (fun () -> 
      ddebug 0 "globals" (lazy (Utils.sprintf "%% remove socket file %s" socketname_file));
      Unix.unlink socketname_file);
  (* local nameservice *)
  let ns = Join.Ns.here in
  (* create global queues *)
  let exit, sub_exit = mk_global_queue ~ns "exit" in
  let redundant, sub_redundant = mk_global_queue ~ns "redundant" in
  let add_parents, add_proof, get_descendants, get_proof =
    proof_parents_process () in
  (* register global channels *)
  Join.Ns.register ns "add_parents" add_parents;
  Join.Ns.register ns "add_proof" add_proof;
  Join.Ns.register ns "get_descendants" get_descendants;
  Join.Ns.register ns "get_proof" get_proof;
  Join.Ns.register ns "send_result" send_result;
  (* global barrier *)
  let sync = mk_barrier ~ns "barrier" num in
  (object
    method subscribe_redundant = sub_redundant
    method publish_redundant = redundant
    method subscribe_exit = sub_exit
    method publish_exit = exit
    method convert ~novel hc = convert_clause add_parents add_proof ~novel hc 
    method get_descendants = get_descendants
    method get_proof = get_proof
    method send_result = send_result
    method sync_barrier = sync
  end :> globals)

(** Create a globals object, using (possibly remote) components *)
let get_globals ~ns process_name =
  (* get global channels *)
  let add_parents = Join.Ns.lookup ns "add_parents" in
  let add_proof = get_add_proof ns in
  let get_descendants = get_get_descendants ns in
  let get_proof = get_get_proof ns in
  let send_result = get_send_result ns in
  let redundant, sub_redundant = get_queue ~ns "redundant" in
  let exit, sub_exit = get_queue ~ns "exit" in
  (* get barrier *)
  let sync = get_barrier ~ns "barrier" in
  (object
    method subscribe_redundant = sub_redundant
    method publish_redundant = redundant
    method subscribe_exit = sub_exit
    method publish_exit = exit
    method convert ~novel hc = convert_clause add_parents add_proof ~novel hc 
    method get_descendants = get_descendants
    method get_proof = get_proof
    method send_result = send_result
    method sync_barrier = sync
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
  def ready() & exit() & wait_done() =
    ddebug 1 "fwd_rw" (lazy "exiting..."); reply to exit & reply to wait_done
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
        ddebug 1 "fwd_rw" (lazy (Utils.sprintf "processing given clause @[<h>%a@]"
                           !C.pp_clause#pp_h given));
        (* simplify given clause *)
        let new_given = calculus#rw_simplify ~select simpl_set given in
        let new_given = calculus#basic_simplify ~ord new_given in
        let new_given = C.select_clause ~select new_given in
        let novel = not (C.eq_hclause given new_given) in
        (* old given clause is redundant *)
        (if novel then spawn (globals#publish_redundant [ns_given]; 0));
        (* forward the given clause and the state *)
        let net_state =
          if calculus#is_trivial new_given
          then { net_state with ns_given = None } (* stop processing this clause *)
          else begin
            (if novel then ddebug 1 "fwd_rw"
              (lazy (Utils.sprintf "simplified to @[<h>%a@]"
              !C.pp_clause#pp_h new_given)));
            simpl_set#add [new_given];  (* add the clause to active set *)
            let ns_new = globals#convert ~novel new_given in
            { net_state with ns_given = Some ns_new }
          end
        in
        (* forward the state *)
        output net_state;
        ready()
      end
    end
  or start() = spawn ready(); wait_done (); reply to start
  in
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

(** Create a component dedicated to simplification by the active set
    and by means of subsumption. It returns a synchronous input chan (net_state -> unit) *)
let fwd_active_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() & wait_done() =
    ddebug 1 "fwd_active" (lazy "exiting..."); reply to exit & reply to wait_done
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
        ddebug 1 "fwd_active" (lazy (Utils.sprintf "processing given clause @[<h>%a@]"
                               !C.pp_clause#pp_h given));
        Selection.check_selected given;
        if calculus#redundant active_set given
        then begin
          ddebug 1 "fwd_active" (lazy "given clause is redundant");
          let net_state = { net_state with ns_given = None } in
          spawn (globals#publish_redundant [ns_given]; 0);
          output net_state;
          ready()
        end else begin
          (* simplify given clause *)
          let new_given = calculus#active_simplify ~select active_set given in
          let new_given = calculus#basic_simplify ~ord new_given in
          let new_given = C.select_clause ~select new_given in
          let novel = not (C.eq_hclause given new_given) in
          (* old given clause is redundant *)
          (if novel then spawn (globals#publish_redundant [ns_given]; 0));
          (* forward the given clause and the state *)
          let net_state =
            if calculus#is_trivial new_given
            then { net_state with ns_given = None } (* stop processing this clause *)
            else begin
              (if novel then ddebug 1 "fwd_active"
                (lazy (Utils.sprintf "simplified to @[<h>%a@]"
                !C.pp_clause#pp_h new_given)));
              Selection.check_selected new_given;
              active_set#add [new_given];  (* add the clause to active set *)
              let ns_new = globals#convert ~novel new_given in
              { net_state with ns_given = Some ns_new }
            end
          in
          (* forward the state *)
          output net_state;
          ready()
        end
      end
    end
  or start() = spawn ready(); wait_done (); reply to start
  in
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

(** Create a component dedicated to removal of active clauses that
    are subsumed by the given clause. *)
let bwd_subsume_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() & wait_done() =
    ddebug 1 "bwd_subsume" (lazy "exiting..."); reply to exit & reply to wait_done
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
          ddebug 1 "bwd_subsume" (lazy (Utils.sprintf "processing given clause @[<h>%a@]"
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
  or start() = spawn ready(); wait_done (); reply to start
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

(** Create a component dedicated to simplification of the active set
    by rewriting using the given clause *)
let bwd_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx index signature =
  let last_state = ref (-1) in
  (* XXX possible optim: only retain the fv_index of active_set? *)
  let active_set = PS.mk_active_set ~ord index signature in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() & wait_done() =
    ddebug 1 "bwd_rw" (lazy "exiting..."); reply to exit & reply to wait_done
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
          ddebug 1 "bwd_rw" (lazy (Utils.sprintf "processing given clause @[<h>%a@]"
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
          let newly_simplified = List.map (C.select_clause ~select) newly_simplified in
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
  or start() = spawn ready(); wait_done (); reply to start
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

(** Create a component that performs generating inferences *)
let generate_process ~calculus ~select ~ord ~output ~globals index signature =
  let last_state = ref (-1) in
  let active_set = PS.mk_active_set ~ord index signature in
  (* case in which we exit *)
  def ready() & exit() & wait_done() =
    ddebug 1 "generate" (lazy "exiting..."); reply to exit & reply to wait_done
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
          ddebug 1 "generate" (lazy (Utils.sprintf "processing given clause @[<h>%a@]"
                              !C.pp_clause#pp_h given));
          (* add given to active set *)
          active_set#add [given];
          (* perform inferences *)
          let new_clauses = Sat.generate ~calculus active_set given in
          let new_clauses = List.map (C.select_clause ~select) new_clauses in
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
  or start() = spawn ready(); wait_done (); reply to start
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

(** Create a component dedicated to RW simplification of newly generated clauses by
    the active set. *)
let post_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx =
  let last_state = ref (-1) in
  let simpl_set = PS.mk_simpl_set ~ord unit_idx in
  (* case in which we exit *)
  def ready() & exit() & wait_done() =
    ddebug 1 "post_rw" (lazy "exiting..."); reply to wait_done & reply to exit
  (* redundant clauses *)
  or  ready() & redundant(clauses) =
    let clauses = List.map (hclause_of_net_clause ~ord) clauses in
    simpl_set#remove clauses;
    ready() & reply to redundant
  (* case in which we process the next state *)
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
      ddebug 1 "post_rw" (lazy (Utils.sprintf "processing %d clauses"
                         (List.length net_state.ns_new)));
      let new_clauses = List.map (hclause_of_net_clause ~ord) net_state.ns_new in
      (* simplify the new clauses *)
      let new_clauses = List.fold_left
        (fun acc hc ->
          let cs = calculus#list_simplify ~ord ~select hc in
          let cs = List.map (calculus#rw_simplify ~select simpl_set) cs in
          let cs = List.map (C.select_clause ~select) cs in
          List.rev_append cs acc)
        [] new_clauses
      in 
      (* put simplified clauses back in the state *)
      let new_clauses = List.map (globals#convert ~novel:true) new_clauses in
      (* maybe we just created new symbols, forward them *)
      let ns_ord = update_precedence ~ord old_ord_version in
      let net_state = {net_state with ns_ord; ns_new = new_clauses } in
      output(net_state);
      ready()
    end
  or start() = spawn ready(); wait_done (); reply to start
  in
  (* subscribe to some events *)
  globals#subscribe_exit exit;
  globals#subscribe_redundant redundant;
  input, start

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
  let last_start= ref (-1) in
  let last_done = ref (-1) in
  let last_ord_version = ref 0 in
  (* how many remaining steps? *)
  let steps_to_go = ref (match steps with None -> max_int | Some s -> s) in
  (* how to process new clauses. Returns Some hc if some hc is empty *)
  let process_new net_state =
    (* look for the empty clause *)
    let empty =
      match net_state.ns_given with
      | Some nc when nc.nc_lits = [||] -> Some nc
      | _ ->
      (try Some (List.find (fun nc -> nc.nc_lits = [||]) net_state.ns_new)
       with Not_found -> None)
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
  (* cases in which we exit *)
  def  exit() & wait_done() =
    ddebug 1 "passive" (lazy "exiting..."); reply to wait_done & reply to exit
  (* decide the next state *)
  or  next_state() =
    assert (!steps_to_go >= 0 && !last_done <= !last_start);
    if !steps_to_go = 0
    then (ddebug 1 "passive" (lazy "reached max number of steps");
          globals#send_result (Sat.Unknown,!last_done); 0)
    else if Sat.check_timeout timeout
    then (ddebug 1 "passive" (lazy "reached timeout");
           globals#send_result (Sat.Unknown, !last_done); 0)
    else if !last_start - !last_done > !pipeline_capacity
    then (ddebug 1 "passive" (lazy "enter state full"); full())
    else (ddebug 1 "passive" (lazy "enter state notfull"); notfull())
  (* process a new clause, the network is not full *)
  or  notfull() = begin
    ddebug 1 "passive" (lazy (Utils.sprintf "pipeline has %d/%d clauses"
                        (!last_start - !last_done) !pipeline_capacity));
    match passive_set#next () with
    | None ->
      if !last_done = !last_start
        then (* all clauses have been processed, none remains in passive *)
          (globals#send_result (Sat.Sat, !last_done); 0)
        else begin (* wait for an incoming result *)
          assert (!last_start > !last_done);
          (ddebug 1 "passive" (lazy "enter state wait"); wait ())
        end
    | Some given ->
      (* preprocess: list_simplify *)
      match calculus#list_simplify ~ord ~select given with
      | [] -> notfull()
      | given::others -> begin
        let given = C.select_clause ~select given in
        let nc_given = globals#convert ~novel:true given in
        assert (ord#precedence#version >= !last_ord_version);
        incr last_start;
        let net_state = {
          ns_num = !last_start;
          (* update the precedence of the pipeline if needed *)
          ns_ord = update_precedence ~ord !last_ord_version;
          ns_given = Some nc_given;
          ns_simplified = [];
          ns_new = List.map (globals#convert ~novel:true) others;
        } in
        (* another clause is sent to the pipeline *)
        ddebug 1 "passive" (lazy (Utils.sprintf "*** enter iteration %d" !last_start));
        last_ord_version := ord#precedence#version;
        output net_state;
        (* shall we wait for results to come before we continue? *)
        next_state()
      end
    end
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
    ddebug 1 "passive" (lazy (Utils.sprintf "*** exit iteration %d" !last_done));
    decr steps_to_go;
    next_state()
  or start() = begin spawn notfull(); wait_done (); reply to start end
  in
  globals#subscribe_exit exit;
  input, start

(* ----------------------------------------------------------------------
 * pipeline setup and wiring
 * ---------------------------------------------------------------------- *)

(*
TODO fix problems with queues when forking
TODO proof reconstruction
*)

(** Get or create the output queue of given name *)
let get_output ~ns ~globals ~process_name ~create_out ~output_name =
  ddebug 1 process_name (lazy (Utils.sprintf "get_out %s (create=%B)" output_name create_out));
  if create_out
  then begin (* create the queue and register it *)
    let q_out, _ = mk_global_queue ~ns output_name in
    ddebug 1 process_name (lazy ("created output queue " ^ output_name));
    globals#sync_barrier process_name;
    q_out
  end else begin (* get the already existing queue *)
    globals#sync_barrier process_name;
    let q_out, _ = get_queue ~ns output_name in
    ddebug 1 process_name (lazy ("got output queue " ^ output_name));
    q_out
  end

(** Run the given role *)
let run_role ~calculus ~select ~ord ~ns ~globals ~create_out ~output_name
~process_name ~role ~input_name ~signature clauses =
  let unit_idx = Dtree.unit_index in
  let index = PS.choose_index "fp" in
  let queues = ClauseQueue.default_queues in
  (* get output queue *)
  let output = get_output ~ns ~globals ~process_name ~create_out ~output_name in
  (* create the process *)
  let process_input, process_start = match role with
  | "passive" -> passive_process ~calculus ~select ~ord ~output ~globals queues clauses
  | "fwd_rewrite" -> fwd_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx
  | "fwd_subsume" -> fwd_active_process ~calculus ~select ~ord ~output ~globals index signature
  | "bwd_subsume" -> bwd_subsume_process ~calculus ~select ~ord ~output ~globals index signature
  | "bwd_rewrite" -> bwd_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx index signature
  | "generate" -> generate_process ~calculus ~select ~ord ~output ~globals index signature
  | "post_rewrite" -> post_rewrite_process ~calculus ~select ~ord ~output ~globals unit_idx
  | _ -> failwith ("bad role " ^ role)
  in
  (* now get the input queue *)
  globals#sync_barrier process_name;
  ddebug 1 process_name (lazy ("get input queue " ^ input_name));
  let _, q_in_subscribe = (get_queue ~ns input_name : (net_state -> unit) *
                           ((net_state -> unit) -> unit)) in
  ddebug 1 process_name (lazy ("got input queue " ^ input_name));
  (* wire process' input to the input queue *)
  q_in_subscribe process_input;
  (* synchronize before starting the process *)
  globals#sync_barrier process_name;
  ddebug 1 process_name (lazy "start");
  process_start ()

(** Assume the given role.
    The role is a string "role,socketname,input_name,output_name,name,bool,signature"
    where the bool is used to choose whether to create the output or not,
    the signature is a blob without ',' *)
let assume_role ~calculus ~select ~ord role_str =
  match Str.split (Str.regexp ",") role_str with
  | [role;socketname;input_name;output_name;process_name;create_out;signature] ->
    (* connect to the master process *)
    let socketname = Unix.ADDR_UNIX socketname in
    let create_out = bool_of_string create_out in
    let ns = Join.Ns.there socketname in
    let globals = get_globals ~ns process_name in
    (* signature *)
    let signature = load_signature signature in
    (* run the process described by role *)
    run_role ~calculus ~select ~ord ~ns ~globals ~create_out ~process_name
      ~role ~input_name ~output_name ~signature []
  | _ -> failwith "wrong role argument"

(** Wrap the given process by creating a global output queue,
    signaling it's ready(), waiting for everyone to be ready,
    and then wiring the process to queues.
    [role] is the name of the action to perform.
    if [create_out] is true, the output queue is created, otherwise it's just
    looked for.
    if [fork] is true, then the process and the queue are created within a
    subprocess.
    
    The global barrier is used twice: once for waiting for queues to be created,
    and once to wait for processes to be linked together before starting them *)
let wrap_process ~fork ?(create_out=true) ~calculus ~ord ~select ~params
input_name output_name process_name role signature clauses =
  if fork
    then if Unix.fork () = 0 then begin
      Format.printf "%% child process %d is %s@." (Unix.getpid ()) process_name;
      let socketname = match socketname with
      | Unix.ADDR_UNIX s -> s
      | Unix.ADDR_INET _ -> assert false
      in
      let role = Utils.sprintf "%s,%s,%s,%s,%s,%B,%s"
        role socketname input_name output_name process_name create_out
        (dump_signature signature) in
      let args = [|"-ord"; ord#name;
                  "-calculus"; params.param_calculus;
                  "-select"; params.param_select;
                  "-debug"; string_of_int (Utils.debug_level ());
                  "-role"; role
                  |] in
      Unix.execv Sys.argv.(0) args
      end else ()
    else spawn begin
      Format.printf "%% master process runs %s@." process_name;
      let ns = Join.Ns.here in
      let globals = get_globals ~ns process_name in
      run_role ~calculus ~select ~ord ~ns ~globals ~create_out ~process_name
        ~output_name ~input_name ~role ~signature clauses;
      0
    end

(** Create the pipeline. [parallel] determines whether to use several
    processes or not *)
let mk_pipeline ~calculus ~select ~ord ~parallel ~send_result ~params
?steps ?timeout clauses signature =
  ddebug 0 "main" (lazy (Utils.sprintf
                  "use %s-process pipeline layout (%d clauses)"
                  (if parallel then "multi" else "single") (List.length clauses)));
  let fork = parallel in
  (* setup globals *)
  let num_processes = 7 in
  let globals = setup_globals send_result num_processes in
  (* convert clauses to net_clause *)
  ddebug 1 "main" (lazy "convert clauses");
  let clauses = List.map (globals#convert ~novel:true) clauses in
  (* passive set *)
  wrap_process ~fork:false ~calculus ~ord ~select ~params
    "q_end" "q_passive" "passive1" "passive" signature clauses;
  (* forward rewriting *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_passive" "q_fwd_rewrite" "fwd_rewrite1" "fwd_rewrite" signature [];
  (* forward subsumption *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_fwd_rewrite" "q_fwd_subsume" "fwd_subsume1" "fwd_subsume" signature [];
  (* backward subsumption *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_fwd_subsume" "q_bwd_subsume" "bwd_subsume1" "bwd_subsume" signature [];
  (* backward rewriting *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_bwd_subsume" "q_bwd_rewrite" "bwd_rewrite1" "bwd_rewrite" signature [];
  (* generate *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_bwd_rewrite" "q_generate" "generate1" "generate" signature [];
  (* post-simplify *)
  wrap_process ~fork ~calculus ~ord ~select ~params
    "q_generate" "q_end" "post_rw1" "post_rewrite" signature [];
  ()

(** Rebuild a hclause with all its proof *)
let rebuild_proof ~ord ~globals nc =
  hclause_of_net_clause ~ord nc  (* TODO *)

(** run the given clause until a timeout occurs or a result
    is found. It returns the result. *)
let given_clause ?(parallel=true) ?steps ?timeout ?(progress=false) ~calculus ~params state =
  let ord = state#ord
  and select = state#select
  and clauses = C.CSet.to_list state#passive_set#clauses in
  def send_result(result,num) & wait() =
    ddebug 1 "main" (lazy "got result from pipeline");
    reply (result,num) to wait & reply to send_result
  in
  ddebug 0 "main" (lazy "start pipelined given clause");
  let signature = C.signature clauses in
  (* create the pipeline of processes *)
  mk_pipeline ~calculus ~select ~ord ~parallel ~send_result ~params ?steps ?timeout
    clauses signature;
  let globals = get_globals ~ns:Join.Ns.here "given_clause" in
  (* wait for a result *)
  let res = match wait () with
  | Sat.Unsat nc, num ->
    (* reconstruct a hclause with its proof *)
    let hc = rebuild_proof ~ord ~globals nc in
    Sat.Unsat hc, num
  | Sat.Sat, num -> Sat.Sat, num
  | Sat.Unknown, num -> Sat.Unknown, num
  | Sat.Error e, num -> Sat.Error e, num
  | Sat.Timeout, num -> Sat.Timeout, num
  in
  globals#publish_exit ();
  res
