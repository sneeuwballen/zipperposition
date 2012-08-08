(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic        
    ||A||  Library of Mathematics, developed at the Computer Science     
    ||T||  Department, University of Bologna, Italy.                     
    ||I||                                                                
    ||T||  HELM is free software; you can redistribute it and/or         
    ||A||  modify it under the terms of the GNU General Public License   
    \   /  version 2 or (at your option) any later version.      
     \ /   This software is distributed as is, NO WARRANTY.     
      V_______________________________________________________________ *)

(* $Id: terms.ml 10720 2010-02-08 07:24:34Z asperti $ *)

module type Blob =
  sig
    (* Blob is the type for opaque leaves: 
     * - checking equality should be efficient
     * - atoms have to be equipped with a total order relation
     *)
    type t

    val eq : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit  (* pretty print object *)

    (* special leaves *)
    val eqP : t  (* symbol for equality predicate *)
    val true_symbol : t (* symbol for 'true' *)
    val bool_symbol : t (* symbol for boolean *)
    val univ_symbol : t (* symbol for universal (terms) sort *)
 
    (* why is this in the /leaves/ signature?!
    type input
    val embed : input -> t foterm
    (* saturate [proof] [type] -> [proof] * [type] *)
    val saturate : input -> input -> t foterm * t foterm
    *)
  end

(* Signature for terms parametrized by leaves *)
module type TermSig =
  sig
    (* type of constant terms and sorts *)
    type leaf

    val pp_leaf : Format.formatter -> leaf -> unit

    (* a sort for terms *)
    type sort = leaf

    (* some special sorts *)
    val bool_sort : sort
    val univ_sort : sort

    (* exception raised when sorts are mismatched *)
    exception SortError of string

    (* a type term *)
    type foterm = typed_term Hashcons.hash_consed
    and typed_term = private {
      term : foterm_cell;   (* the term itself *)
      sort : sort;          (* the sort of the term *)
    }
    and foterm_cell = private
      | Leaf of leaf  (* constant *)
      | Var of int  (* variable *)
      | Node of foterm list  (* term application *)

    (* smart constructors, with type-checking *)
    val mk_var : int -> sort -> foterm
    val mk_leaf : leaf -> sort -> foterm
    val mk_node : foterm list -> foterm

    (* special terms *)
    val eq_term : foterm  (* equality of terms *)
    val true_term : foterm  (* tautology symbol *)

    (* cast (change sort) *)
    val cast : foterm -> sort -> foterm

    (* free variables in the term *)
    val vars_of_term : foterm -> foterm list

    (* substitution, a list of variables -> term *)
    type substitution = (foterm * foterm) list

    (* partial order comparison *)
    type comparison = Lt | Eq | Gt | Incomparable | Invertible

    (* direction of an equation (for rewriting) *)
    type direction = Left2Right | Right2Left | Nodir
    (* side of an equation *)
    type side = LeftSide | RightSide
    (* position in a term *)
    type position = int list

    (* a literal, that is, a signed equation *)
    type literal = 
     | Equation of    foterm  (* lhs *)
                    * foterm  (* rhs *)
                    * bool    (* sign *)
                    (* * comparison (* orientation *) *)

    (* build literals *)
    val mk_eq : foterm -> foterm -> literal
    val mk_neq : foterm -> foterm -> literal
    (* negate literal *)
    val negate_lit : literal -> literal

    (* a proof step for a clause *)
    type proof =
      | Axiom of string  (* axiom of input *)
      | SuperpositionLeft of sup_position
      | SuperpositionRight of sup_position
      | EqualityFactoring of eq_factoring_position  
      | EqualityResolution of eq_resolution_position
    and sup_position = {
      (* describes a superposition inference *)
      sup_active : clause;  (* rewriting clause *)
      sup_passive : clause; (* rewritten clause *)
      sup_active_pos : (int * side * position);
      sup_passive_pos : (int * side * position);
      sup_subst : substitution;
    }
    and eq_factoring_position = {
      (* describes an equality factoring inference *)
      eqf_clause : clause;
      eqf_bigger : (int * side);  (* bigger equation s=t, s > t *)
      eqf_smaller : (int * side); (* smaller equation u=v *)
      eqf_subst : substitution; (* subst(s) = subst(u) *)
    }
    and eq_resolution_position = {
      (* describes an equality resolution inference *)
      eqr_clause : clause;
      eqr_position : int;
      eqr_subst : substitution;
    }
    and clause =
        (* a first order clause *)
        int (* ID *)
      * literal list  (* the equations *)
      * foterm list  (* the free variables *)
      * proof (* the proof for this clause *)

    (* build a clause with a new ID *)
    val mk_clause : literal list -> proof -> clause

    module M : Map.S with type key = int 

    (* multiset of clauses TODO use a map? *)
    type bag = int (* max ID  *)
                  * ((clause * bool * int) M.t)

    (* also gives a fresh ID to the clause *)
    val add_to_bag : clause -> bag -> bag * clause

    val replace_in_bag : clause * bool * int -> bag -> bag

    val get_from_bag : int -> bag -> clause * bool * int

    val empty_bag : bag

  end

open Hashcons

module Make(B : Blob) =
  struct

    type leaf = B.t

    let pp_leaf = B.pp

    (* a sort for terms *)
    type sort = leaf

    (* some special sorts *)
    let bool_sort = B.bool_symbol
    let univ_sort = B.univ_symbol

    (* exception raised when sorts are mismatched *)
    exception SortError of string

    (* simple term. TODO use hashconsing and smart constructors *)
    type foterm = typed_term Hashcons.hash_consed
    and typed_term = {
      term : foterm_cell;   (* the term itself *)
      sort : sort;          (* the sort of the term *)
    }
    and foterm_cell = 
      | Leaf of leaf  (* constant *)
      | Var of int  (* variable *)
      | Node of foterm list  (* term application *)

    (* hashconsing *)
    module H = Hashcons.Make(struct
      type t = typed_term
      let equal = fun x y -> x = y
      let hash = Hashtbl.hash
    end)

    (* the terms table *)
    let terms = H.create 251

    (* smart constructors, with type-checking *)
    let mk_var idx sort = H.hashcons terms {term = Var idx; sort=sort}
    let mk_leaf leaf sort = H.hashcons terms {term = Leaf leaf; sort=sort}
    let rec mk_node = function
      | [] -> failwith "cannot build empty node term"
      | (head::_) as subterms -> 
            H.hashcons terms {term = (Node subterms); sort=head.node.sort}

    (* special terms *)
    let eq_term = mk_leaf B.eqP bool_sort (* equality, returns bool *)
    let true_term = mk_leaf B.true_symbol bool_sort (* tautology symbol *)

    (* cast (change sort) *)
    let cast t sort = H.hashcons terms { t.node with sort=sort; }

    (* free variables in the term *)
    let vars_of_term t =
      let rec aux acc t = match t.node.term with
        | Leaf _ -> acc
        | Var _ -> if (List.mem t acc) then acc else t::acc
        | Node l -> List.fold_left aux acc l
      in aux [] t

    (* substitution, a list of variables -> term *)
    type substitution = (foterm * foterm) list

    (* partial order comparison *)
    type comparison = Lt | Eq | Gt | Incomparable | Invertible

    (* direction of an equation (for rewriting) *)
    type direction = Left2Right | Right2Left | Nodir
    (* side of an equation *)
    type side = LeftSide | RightSide
    (* position in a term *)
    type position = int list

    (* a literal, that is, a signed equation *)
    type literal = 
     | Equation of    foterm  (* lhs *)
                    * foterm  (* rhs *)
                    * bool    (* sign *)
                    (* * comparison (* orientation *) *)

    (* build literals *)
    let check_type a b = if a.node.sort <> b.node.sort
      then raise (SortError "sides of equations of different sorts") else ()
    let mk_eq a b = (check_type a b; Equation (a, b, true))
    let mk_neq a b = (check_type a b; Equation (a, b, false))
    (* negate literal *)
    let negate_lit (Equation (l,r,sign)) = Equation (l,r, not sign)


    (* a proof step for a clause *)
    type proof =
      | Axiom of string (* axiom of input *)
      | SuperpositionLeft of sup_position
      | SuperpositionRight of sup_position
      | EqualityFactoring of eq_factoring_position  
      | EqualityResolution of eq_resolution_position
    and sup_position = {
      (* describes a superposition inference *)
      sup_active : clause;  (* rewriting clause *)
      sup_passive : clause; (* rewritten clause *)
      sup_active_pos : (int * side * position);
      sup_passive_pos : (int * side * position);
      sup_subst : substitution;
    }
    and eq_factoring_position = {
      (* describes an equality factoring inference *)
      eqf_clause : clause;
      eqf_bigger : (int * side);  (* bigger equation s=t, s > t *)
      eqf_smaller : (int * side); (* smaller equation u=v *)
      eqf_subst : substitution; (* subst(s) = subst(u) *)
    }
    and eq_resolution_position = {
      (* describes an equality resolution inference *)
      eqr_clause : clause;
      eqr_position : int;
      eqr_subst : substitution;
    }
    and clause =
      (* a first order clause *)
        int (* ID *)
      * literal list  (* the equations *)
      * foterm list  (* the free variables *)
      * proof (* the proof for this clause *)

    let clause_id = ref 0

    (* build a clause with a new ID *)
    let mk_clause lits proof =
      let rec merge_vars vars = function
        | [] -> vars
        | (x::xs) -> if List.mem x vars
          then merge_vars vars xs else merge_vars (x::vars) xs
      and vars_of_lits (Equation (l, r, _)) =
        merge_vars (vars_of_term l) (vars_of_term r) in
      let all_vars =
        List.fold_left merge_vars [] (List.map vars_of_lits lits)
      and id = (let i = !clause_id in clause_id := i+1; i) in
      (id, lits, all_vars, proof)

    module OT =
     struct
       type t = int 
       let compare = Pervasives.compare
     end

    module M : Map.S with type key = int 
      = Map.Make(OT) 

    (* multiset of clauses *)
    type bag = int (* max ID  *)
                  * ((clause * bool * int) M.t)

    let add_to_bag (_,lit,vl,proof) (id,bag) =
      let id = id+1 in
      let clause = (id, lit, vl, proof) in
      let bag = M.add id (clause,false,0) bag in
      (id,bag), clause 

    let replace_in_bag ((id,_,_,_),_,_ as cl) (max_id,bag) =
      let bag = M.add id cl bag in
        (max_id,bag)

    let get_from_bag id (_,bag) =
      M.find id bag
      
    let empty_bag = (0,M.empty)

end

(* default implementation with strings *)
module Default = Make(struct
  type t = string
  let eq a b = a = b
  let compare = Pervasives.compare
  let pp f x = Format.fprintf f "%s" x
  let eqP = "="
  let equivP = "<=>"
  let true_symbol = "true"
  let bool_symbol = "Bool"
  let univ_symbol = "U"
end)

