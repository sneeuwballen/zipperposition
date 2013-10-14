(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Forward and backward Logic Reasoner} *)

open Logtk

module T = HOTerm

type datalog_symbol =
  | DSName of string
  | DSTerm of HOTerm.t
  | DSType of Type.t
  | DSSTartList of int

let eq_datalog_symbol s1 s2 = match s1, s2 with
  | DSName s1, DSName s2 -> s1 = s2
  | DSTerm t1, DSTerm t2 -> T.eq t1 t2
  | DSType ty1, DSType ty2 -> Type.eq ty1 ty2
  | DSSTartList i1, DSSTartList i2 -> i1 = i2
  | _ -> false

let hash_datalog_symbol s = match s with
  | DSName s -> Hash.hash_string s
  | DSTerm t -> T.hash t
  | DSType ty -> Type.hash ty
  | DSSTartList i -> i + 13

(** Datalog instance *)
module Logic = Datalog.Make(struct
  type t = datalog_symbol
  let equal = eq_datalog_symbol
  let hash t = hash_datalog_symbol t
  let to_string = function
    | DSName s -> s
    | DSTerm t -> T.to_string t
    | DSType ty -> Type.to_string ty
    | DSSTartList i -> Util.sprintf "L%d" i
end)

(** {2 Encoding into datalog literals/clauses} *)

module Translate = struct
  type _ mapping =
    | None : unit mapping  (* no argument *)
    | String : string mapping
    | Term : T.t mapping
    | Type : Type.t mapping
    | Parametrize : 'a mapping -> ('a * T.t list) mapping
    | Map : ('a -> 'b) * ('b -> 'a) * 'b mapping -> 'a mapping
    | List : 'a mapping -> 'a list mapping
    | Pair : 'a mapping * 'b mapping -> ('a * 'b) mapping
    | Triple : 'a mapping * 'b mapping * 'c mapping -> ('a * 'b * 'c) mapping 
    | Quad : 'a mapping * 'b mapping * 'c mapping * 'd mapping -> ('a * 'b * 'c * 'd) mapping 

  (* TODO: disjunction Either (term, 'a) to allow to quantify on
     other things than terms? *)

  let none = None
  let str = String
  let term = Term
  let type_ = Type
  let parametrize p = Parametrize p
  let map ~inject ~extract m = Map (inject, extract, m)
  let list_ m = List m
  let pair a b = Pair (a, b)
  let triple a b c = Triple (a, b, c)
  let quad a b c d = Quad (a, b, c, d)
  let (|||) a b = Pair (a, b)

  exception CannotDecode

  let encode mapping head x =
    (* recursive encoding into a list *)
    let rec build_args : type a. a mapping -> a -> Logic.term list -> Logic.term list =
    fun mapping x l -> match mapping, x with
    | None, () -> l
    | Term, {T.term=node} ->
      begin match node with
      | T.Var i -> Logic.mk_var i :: l
      | _ -> Logic.mk_const (DSTerm x) :: l
      end
    | Type, ty -> (Logic.mk_const (DSType ty)) :: l
    | Parametrize p, (y, args) ->
      let l' = build_args (list_ term) args l in
      build_args p y l'
    | Map (inject, _, m'), x -> build_args m' (inject x) l
    | List m, xs ->
      let l' = List.fold_right (fun x l -> build_args m x l) xs l in
      Logic.mk_const (DSSTartList (List.length xs)) :: l'
    | String, s -> Logic.mk_const (DSName s) :: l
    | Pair (a, b), (xa, xb) ->
      let l' = build_args b xb l in
      build_args a xa l'
    | Triple (a, b, c), (xa, xb, xc) ->
      let l' = build_args c xc l in
      let l' = build_args b xb l' in
      build_args a xa l'
    | Quad (a, b, c, d), (xa, xb, xc, xd) ->
      let l' = build_args d xd l in
      let l' = build_args c xc l' in
      let l' = build_args b xb l' in
      build_args a xa l'
    in
    let args = build_args mapping x [] in
    Logic.mk_literal (DSName head) args

  let decode mapping lit =
    (* recursively decode the list of terms using the mapping *)
    let rec decode : type a. a mapping -> Logic.term list -> a * Logic.term list
    = fun mapping l ->
      match mapping, l with
      | None, _ -> (), l
      | String, (Logic.Const (DSName s))::l' -> s, l'
      | Term, (Logic.Var i) :: l' -> T.mk_var i, l'
      | Term, (Logic.Const (DSTerm t))::l' -> t, l'
      | Type, (Logic.Const (DSType ty))::l' -> ty, l'
      | Map (_, extract, m'), l ->
        let y, l' = decode m' l in
        extract y, l'
      | List m, (Logic.Const (DSSTartList i)) :: l' -> decode_list m i l'
      | Parametrize mapping', _ ->
        let x, l' = decode mapping' l in
        let args, l'' = decode (list_ term) l' in
        (x, args), l''
      | Pair (ma, mb), _ ->
        let a, l' = decode ma l in
        let b, l'' = decode mb l' in
        (a, b), l''
      | Triple (ma, mb, mc), _ ->
        let a, l' = decode ma l in
        let b, l'' = decode mb l' in
        let c, l''' = decode mc l'' in
        (a, b, c), l'''
      | Quad (ma, mb, mc, md), _ ->
        let a, l' = decode ma l in
        let b, l'' = decode mb l' in
        let c, l''' = decode mc l'' in
        let d, l'''' = decode md l''' in
        (a, b, c, d), l''''
      | _, _ ->
        raise CannotDecode
    (* decode a list of [n] elements using [mapping] *)
    and decode_list
    : type a. a mapping -> int -> Logic.term list -> a list * Logic.term list =
    fun mapping n l ->
      if n = 0
        then [], l
        else
          let x, l' = decode mapping l in  (* head *)
          let xs, l'' = decode_list mapping (n-1) l' in  (* tail *)
          (x::xs), l''
    in
    match Logic.open_literal lit with
    | DSName s, l ->
      let x, remain = decode mapping l in
      begin match remain with
      | [] -> s, x
      | _ -> failwith "Reasoner.Translate.decode: wrong arity, terms remain"
      end
    | _, _ -> failwith "bad Datalog literal (does not begin with a string)"

  let decode_head mapping head lit =
    let head', x = decode mapping lit in
    if head = head'
      then x
      else raise CannotDecode
end

let pp_lit buf lit =
  let b = Buffer.create 5 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "@[<h>%a@]@?" Logic.pp_literal lit;
  Buffer.add_buffer buf b

let pp_clause buf c =
  let b = Buffer.create 5 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "@[<h>%a@]@?" Logic.pp_clause c;
  Buffer.add_buffer buf b

(** {2 Reasoner} *)

type t = {
  db : Logic.db;
  on_new_fact : Logic.literal Signal.t;
  on_new_fact_by : (string, Logic.literal Signal.t) Hashtbl.t;
  on_new_goal : Logic.literal Signal.t;
}

let create () =
  let on_new_fact = Signal.create () in
  let on_new_goal = Signal.create () in
  let db = Logic.db_create () in
  let reasoner = {
    db;
    on_new_fact;
    on_new_fact_by = Hashtbl.create 7;
    on_new_goal;
  } in
  Logic.db_subscribe_goal reasoner.db (Signal.send on_new_goal);
  Logic.db_subscribe_all_facts reasoner.db (Signal.send on_new_fact);
  Logic.db_subscribe_all_facts db
    (fun lit -> Util.debug 4 "reasoner: new fact %a" pp_lit lit);
  Logic.db_subscribe_goal db
    (fun lit -> Util.debug 4 "reasoner: new goal %a" pp_lit lit);
  reasoner
    
let is_empty reasoner =
  Logic.db_size reasoner.db = 0

let size reasoner =
  Logic.db_size reasoner.db

let all_clauses reasoner =
  Sequence.from_iter
    (fun k -> Logic.db_fold
      (fun () c -> k c)
      () reasoner.db)

let all_facts reasoner =
  Sequence.from_iter
    (fun k -> Logic.db_fold
      (fun () c ->  match Logic.open_clause c with
        | lit, [] -> k (Logic.of_soft_lit lit)
        | _ -> ())
      () reasoner.db)

let all_facts_by reasoner str =
  let symbol = DSName str in
  Sequence.from_iter
    (fun k -> Logic.db_fold
      (fun () c ->  match Logic.open_clause c with
        | (s,arg) as lit, [] when eq_datalog_symbol s symbol ->
          k (Logic.of_soft_lit lit)
        | _ -> ())
      () reasoner.db)

let all_facts_matching reasoner lit =
  Sequence.from_iter
    (fun k -> Logic.db_match reasoner.db lit k)

let is_fact reasoner lit =
  Logic.db_mem reasoner.db (Logic.mk_clause lit [])

(** {2 Events} *)

let on_new_fact reasoner = reasoner.on_new_fact

(** Obtain an event handler for facts that have the given head symbol *)
let on_new_fact_by reasoner s =
  try
    Hashtbl.find reasoner.on_new_fact_by s
  with Not_found ->
    let signal = Signal.create () in
    Hashtbl.add reasoner.on_new_fact_by s signal;
    (* subscribe to the DB, with a handler that will signal [e] with every
      fact is receives. *)
    let t = DSName s in
    Logic.db_subscribe_fact reasoner.db t (Signal.send signal);
    signal

let on_new_goal reasoner =
  reasoner.on_new_goal

let on_new_goal_by reasoner s =
  Signal.filter reasoner.on_new_goal
    (fun lit -> match Logic.open_literal lit with
      | (DSName s', _) when s = s' -> true
      | _ -> false)

let explanations reasoner c =
  Logic.db_explanations reasoner.db c

let explanations_lit reasoner lit =
  Logic.db_explanations reasoner.db (Logic.mk_clause lit [])

(** {2 Forward chaining} *)

(** Add a clause to the engine *)
let add ?expl reasoner clause =
  Util.debug 4 "reasoner: add clause %a" pp_clause clause;
  Logic.db_add ?expl reasoner.db clause

let add_seq reasoner seq =
  Sequence.iter
    (fun c -> add reasoner c)
    seq

(** Add a fact to the engine *)
let add_fact ?expl reasoner t =
  let clause = Logic.mk_clause t [] in
  add ?expl reasoner clause

(** {2 Backward chaining (goals)} *)

let add_goal reasoner goal =
  Util.debug 4 "reasoner: add goal %a" pp_lit goal;
  Logic.db_goal reasoner.db goal

(** {2 Explanations} *)

(** List of facts that explain the given fact (or Not_found) *)
let explain reasoner fact =
  Logic.db_explain reasoner.db fact

(** Hyperresolution atom+electrons that explain the given fact, ie,
    which directly imply it (or Not_found). *)
let premises reasoner clause =
  Logic.db_premises reasoner.db clause
