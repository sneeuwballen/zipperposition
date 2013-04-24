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

(** {1 Persistent Knowledge Base} *)

open Basic
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils
module Pattern = Meta_Pattern

(** {2 Knowledge Item} *)

(** Definitions at the meta-level, that respectively state that
    a lemma is true, that define a theory, or that bind a ground
    convergent system to a theory *)
type definition =
| Named of string * Pattern.t
| Theory of string parametrized * premise list
| Lemma of Pattern.t parametrized * premise list
| GC of gnd_convergent_spec * premise list
(** Condition for some meta-level assertion *)
and premise =
| IfNamed of string parametrized
| IfTheory of string parametrized
| IfPattern of Pattern.t parametrized
(** Assertions about the problem *)
and fact =
| ThenPattern of Pattern.t parametrized
| ThenTheory of string parametrized
| ThenNamed of string parametrized
| ThenGC of gnd_convergent_spec parametrized
and gnd_convergent_spec = {
  gc_vars : varlist;
  gc_ord : string;
  gc_theory : string;
  gc_prec : varlist;
  gc_eqns : Pattern.t parametrized list;
} (** Abstract equations that form a ground convergent rewriting system
      when instantiated. It is parametrized by the theory it decides.
      gc_ord and gc_prec (once instantiated), give a constraint on the ordering
      that must be satisfied for the system to be a decision procedure. *)

(** {2 Utils} *)

let rec compare_definitions d1 d2 =
  match d1, d2 with
  | Named (n1,p1), Named (n2,p2) ->
    Utils.lexicograph_combine
      [String.compare n1 n2;
       Pattern.compare_pattern p1 p2]
  | Theory ((th1,args1),premises1), Theory ((th2,args2), premises2) ->
    Utils.lexicograph_combine
      [String.compare th1 th2;
       Utils.lexicograph T.compare_term args1 args2;
       Utils.lexicograph compare_premise premises1 premises2]
  | Lemma ((p1,args1),premises1), Lemma ((p2,args2),premises2) ->
    Utils.lexicograph_combine
      [Pattern.compare_pattern p1 p2;
       Utils.lexicograph T.compare_term args1 args2;
       Utils.lexicograph compare_premise premises1 premises2]
  | GC (gc1,premises1), GC (gc2, premises2) ->
    Utils.lexicograph_combine
      [Utils.lexicograph
        (fun (p1,a1) (p2,a2) ->
          Utils.lexicograph_combine
            [Pattern.compare_pattern p1 p2;
             Utils.lexicograph T.compare_term a1 a2])
        gc1.gc_eqns gc2.gc_eqns;
       String.compare gc1.gc_ord gc2.gc_ord;
       Utils.lexicograph T.compare_term gc1.gc_prec gc2.gc_prec;
       Utils.lexicograph compare_premise premises1 premises2]
  | Named _, _ -> 1
  | _, Named _ -> -1
  | Theory _, _ -> 1
  | _, Theory _ -> -1
  | Lemma _, _ -> 1
  | _, Lemma _ -> -1
and compare_premise p1 p2 =
  match p1, p2 with
  | IfNamed (n1,args1), IfNamed (n2,args2) ->
    Utils.lexicograph_combine
      [String.compare n1 n2;
       Utils.lexicograph T.compare_term args1 args2]
  | IfTheory (n1,args1), IfTheory (n2,args2) ->
    Utils.lexicograph_combine
      [String.compare n1 n2;
       Utils.lexicograph T.compare_term args1 args2]
  | IfPattern (p1, args1), IfPattern (p2, args2) ->
    Utils.lexicograph_combine
      [Pattern.compare_pattern p1 p2;
       Utils.lexicograph T.compare_term args1 args2]
  | IfNamed _, _ -> 1
  | _, IfNamed _ -> -1
  | IfTheory _, _ -> 1
  | _, IfTheory _ -> -1

module DefSet = Set.Make(struct
  type t = definition
  let compare = compare_definitions
end)

(** Convert a ground-convergent abstract specification to a concrete
    system, if possible (ie, if fully instantiated). It also takes a list
    of clauses that "justify" the truth of the GC. *)
let gc_spec_to_gc ~ctx ((gc,args) : gnd_convergent_spec parametrized) parents =
  assert (List.length args == List.length gc.gc_vars);
  assert (List.for_all2 (fun t1 t2 -> t1.sort == t2.sort) gc.gc_vars args);
  (* create the substitution that will allow to ground equations *)
  let subst = List.fold_left2
    (fun subst var t -> S.bind subst (var,1) (t,0))
    S.id_subst gc.gc_vars args in
  try
    (* instantiate equations *)
    let eqns = List.map
      (fun p -> Pattern.apply_subst (p,1) subst)
      gc.gc_eqns in
    let eqns = List.map
      (fun t ->
        (* make a clause from the term, and simplify it (back to CNF) *)
        let lits = [Literals.mk_eq ~ord:ctx.ctx_ord t T.true_term] in
        let premises = List.map (fun hc -> hc.hcproof) parents in
        let proof c = Proof.mk_proof c ("gc_" ^ gc.gc_theory) premises in
        let hc = C.mk_hclause ~ctx lits proof in
        Cnf.simplify hc)
      eqns in
    (* recover precedence *)
    let prec = List.map (fun v -> S.apply_subst subst (v,1)) gc.gc_prec in
    let prec = List.map
      (fun t -> match t.term with
        | Node (s, []) -> s
        | _ -> failwith "invalid precedence")
      prec in
    let gc = Experts.mk_gc ~theory:gc.gc_theory ~ord:gc.gc_ord ~prec eqns in
    (* success *)
    Some gc
  with Failure s ->
    Utils.debug 1 "%% failed to instantiate GndConvergent system: %s" s;
    None

(** {2 Printing/parsing} *)

let rec pp_definition formatter definition =
  match definition with
  | Named (name, p) ->
    Format.fprintf formatter "@[<h>%s is %a@]" name Pattern.pp_pattern p
  | Lemma ((concl, vars), premises) ->
    Format.fprintf formatter "@[<hov2>lemma @[<h>%a@] if@ %a@]"
      Pattern.pp_pattern_p (concl, vars)
      (Utils.pp_list pp_premise) premises
  | Theory ((name,args), premises) ->
    Format.fprintf formatter "@[<hov2>@[<h>theory %s(%a)@] if@ %a@]" name
      (Utils.pp_list !T.pp_term#pp) args
      (Utils.pp_list pp_premise) premises
  | GC (gc, premises) ->
    Format.fprintf formatter
      "@[<hov2>gc for %s @[<hov2>%a@]@ @[<h>with %s(%a) if@ %a@]@]"
      gc.gc_theory (Utils.pp_list ~sep:" and " Pattern.pp_pattern_p) gc.gc_eqns
      gc.gc_ord (Utils.pp_list !T.pp_term#pp) gc.gc_prec
      (Utils.pp_list pp_premise) premises
and pp_premise formatter premise =
  match premise with
  | IfNamed (name, args) | IfTheory(name, args) ->
    if args = []
      then Format.fprintf formatter "%s" name
      else Format.fprintf formatter "@[<h>%s(%a)@]" name (Utils.pp_list !T.pp_term#pp) args
  | IfPattern (p, args) ->
    Pattern.pp_pattern_p formatter (p, args)
and pp_fact formatter fact =
  match fact with
  | ThenPattern (p, args) ->
    Pattern.pp_pattern_p formatter (p, args)
  | ThenTheory (name, args) | ThenNamed (name, args) ->
    if args = []
      then Format.fprintf formatter "%s" name
      else Format.fprintf formatter "@[<h>%s(%a)@]" name (Utils.pp_list !T.pp_term#pp) args
  | ThenGC (gc, args) ->
    Format.fprintf formatter
      "@[<hov2>gc {%a@ @[<h>with %s(%a)}(%a)@]@]"
      (Utils.pp_list ~sep:" and " !T.pp_term#pp)
      (List.map (fun (p,args) -> Pattern.instantiate p args) gc.gc_eqns)
      gc.gc_ord (Utils.pp_list !T.pp_term#pp) gc.gc_prec
      (Utils.pp_list ~sep:" and " !T.pp_term#pp) args

let rec definition_to_json definition : json =
  match definition with
  | Named (name, pat) ->
    `List [`String "named"; `String name; Pattern.to_json pat]
  | Theory ((name, args), premises) ->
    `List (`String "theory" :: `String name :: `List (List.map T.to_json args) ::
          List.map premise_to_json premises)
  | Lemma ((pat, args), premises) ->
    `List (`String "lemma" :: `List (Pattern.to_json pat :: List.map T.to_json args) ::
          List.map premise_to_json premises)
  | GC (gc, premises) ->
      `Assoc ["gc", `Bool true;
              "theory", `String gc.gc_theory;
              "vars", `List (List.map T.to_json gc.gc_vars);
              "ord", `String gc.gc_ord;
              "prec", `List (List.map T.to_json gc.gc_prec);
              "premises", `List (List.map premise_to_json premises);
              "eqns", `List (List.map
                (fun (pat, args) -> `List (Pattern.to_json pat :: List.map T.to_json args))
                gc.gc_eqns);]
and premise_to_json (premise : premise) : json =
  match premise with
  | IfNamed (name, args) ->
    `List (`String "named" :: `String name :: List.map T.to_json args)
  | IfTheory (name, args) ->
    `List (`String "theory" :: `String name :: List.map T.to_json args)
  | IfPattern (pat, args) ->
    `List (`String "pattern" :: Pattern.to_json pat :: List.map T.to_json args)

let rec definition_of_json (json : json) : definition =
  match json with
  | `List [`String "named"; `String name; pat] ->
    Named (name, Pattern.of_json pat)
  | `List (`String "theory" :: `String name :: `List args :: premises) ->
    Theory ((name, List.map T.of_json args), List.map premise_of_json premises)
  | `List (`String "lemma" :: `List (pat :: args) :: premises) ->
    Lemma ((Pattern.of_json pat, List.map T.of_json args),
           List.map premise_of_json premises)
  | `Assoc l when List.mem_assoc "gc" l ->
    let gc_vars = List.map T.of_json (Json.Util.to_list (List.assoc "vars" l)) in
    let gc_theory = Json.Util.to_string (List.assoc "theory" l) in
    let gc_ord = Json.Util.to_string (List.assoc "ord" l) in
    let gc_prec = List.map T.of_json (Json.Util.to_list (List.assoc "prec" l)) in
    let premises = List.map premise_of_json
      (Json.Util.to_list (List.assoc "premises" l)) in
    let gc_eqns = List.map
      (function
        | `List (pat::args) -> (Pattern.of_json pat, List.map T.of_json args)
        | json -> raise (Json.Util.Type_error ("expected (pattern,terms)", json)))
      (Json.Util.to_list (List.assoc "eqns" l)) in
    GC ({ gc_ord; gc_theory; gc_vars; gc_prec; gc_eqns; }, premises)
  | _ -> raise (Json.Util.Type_error ("expected KB.definition", json))
and premise_of_json (json : json) : premise =
  match json with
  | `List (`String "named" :: `String name :: args) ->
    IfNamed (name, List.map T.of_json args)
  | `List (`String "theory" :: `String name :: args) ->
    IfTheory (name, List.map T.of_json args)
  | `List (`String "pattern" :: pat :: args) ->
    IfPattern (Pattern.of_json pat, List.map T.of_json args)
  | _ -> raise (Json.Util.Type_error ("expected KB.premise", json))

(** {2 Datalog atoms} *)

type atom =
| MString of string
| MPattern of Pattern.t
| MPatternVars of Pattern.t parametrized
| MTerm of term

let rec eq_atom a1 a2 = match a1, a2 with
  | MString s1, MString s2 -> s1 = s2
  | MPattern p1, MPattern p2 -> Pattern.eq_pattern p1 p2
  | MTerm t1, MTerm t2 -> t1 == t2
  | _ -> false

let rec hash_atom = function
  | MString s -> Hash.hash_string s
  | MPattern p -> Pattern.hash_pattern p
  | MPatternVars (p, vars) ->
    Hash.hash_list T.hash_term (Pattern.hash_pattern p) vars
  | MTerm t -> T.hash_term t

let rec pp_atom formatter a = match a with
  | MString s -> Format.pp_print_string formatter s
  | MPattern p -> Pattern.pp_pattern formatter p
  | MPatternVars (p,vars) ->
    Format.fprintf formatter "@[<h>%a[%a]@]" Pattern.pp_pattern p
      (Utils.pp_list !T.pp_term#pp) vars
  | MTerm t -> T.pp_term_debug#pp formatter t

let rec atom_to_json a : json = match a with
  | MString s -> `String s
  | MPattern p -> `Assoc ["pattern", Pattern.to_json p]
  | MPatternVars (p, vars) -> assert false (* TODO *)
  | MTerm t -> `Assoc ["term", T.to_json t]

let rec atom_of_json (json : json) : atom = match json with
  | `String s -> MString s
  | `Assoc ["pattern", p] -> MPattern (Pattern.of_json p)
  | `Assoc ["term", t] -> MTerm (T.of_json t)
  | _ -> raise (Json.Util.Type_error ("expected atom", json))

(** The Datalog prover that reasons over atoms. *)
module Logic = Datalog.Logic.Make(struct
  type t = atom
  let equal = eq_atom
  let hash = hash_atom
  let to_string a = Utils.sprintf "%a" pp_atom a
  let of_string s = atom_of_json (Json.from_string s)  (* should not happen *)
end)
  
(** {2 Conversion to Datalog} *)

(** Encode term into a Datalog atom. If [to_var] is true, then variables
    will be encoded into Datalog variables (otherwise to terms) *)
let encode_term ~to_var t = match t.term with
  | Var i when to_var && i >= 0 -> `Var (-(i*2)-1)
  | Var i when to_var && i < 0 -> `Var (i*2)
  | _ -> `Symbol (MTerm t)

(** Convert a Datalog atom back to a term of given sort, or raise Failure *)
let deencode_term t sort = match t with
  | `Var i when (i mod 2) = 0 -> T.mk_var (i/2) sort
  | `Var i -> T.mk_var (-(i+1)/2) sort
  | `Symbol (MTerm t) ->
    assert (t.sort == sort);
    t
  | `Symbol s -> failwith (Utils.sprintf "Datalog atom %a not a term" pp_atom s)

(** {3 Construction of atoms} *)

let atom_named name args =
  Logic.mk_literal (MString "named") (`Symbol (MString name) :: args)

let atom_theory name args =
  Logic.mk_literal (MString "theory") (`Symbol (MString name) :: args)

let atom_pattern pat args =
  Logic.mk_literal (MString "pattern") (`Symbol (MPattern pat) :: args)

let atom_gc ?(offset=(-1)) gc terms =
  assert (List.length gc.gc_vars = List.length terms);
  let args = [`Symbol (MString gc.gc_ord); `Symbol (MString gc.gc_theory)] in
  let args = args @ List.map (encode_term ~to_var:false) gc.gc_prec in
  let args = args @ List.map (fun (pat,vars) -> `Symbol (MPatternVars (pat, vars))) gc.gc_eqns in
  let args = args @ List.map (encode_term ~to_var:false) gc.gc_vars in
  let args = args @ List.map (encode_term ~to_var:true) terms in
  Logic.mk_literal (MString "gc") args

(** Convert the arguments into terms. Expected sorts are given and
    must match. *)
let extract_terms sorts args =
  assert (List.length sorts = List.length args);
  let terms = List.fold_left2
    (fun acc sort arg -> match arg with
      | `Symbol (MPattern _)
      | `Symbol (MPatternVars _)
      | `Symbol (MString _) -> assert false
      | `Symbol (MTerm t) ->
        assert (t.sort == sort);
        t::acc
      | `Var i -> (T.mk_var (-i) sort) :: acc)
    [] sorts args in
  List.rev terms

(** Convert the arguments into terms. Sorts are guessed/by default *)
let extract_terms_unsafe args =
  List.map
    (function
      | `Symbol (MPattern _)
      | `Symbol (MPatternVars _)
      | `Symbol (MString _) -> assert false
      | `Symbol (MTerm t) -> t
      | `Var i -> T.mk_var (-i) univ_)
    args

(** Translate a premise to a Datalog literal *)
let premise_to_datalog premise =
  let map_args args = List.map (encode_term ~to_var:true) args in
  match premise with
  | IfNamed (name, args) -> atom_named name (map_args args)
  | IfTheory (name, args) -> atom_theory name (map_args args)
  | IfPattern (p, args) -> atom_pattern p (map_args args)

let vars_of_premise premise =
  let seq =
    match premise with
    | IfNamed (_, args) -> Sequence.of_list args
    | IfTheory (_, args) -> Sequence.of_list args
    | IfPattern (_, args) -> Sequence.of_list args
  in Sequence.filter T.is_var seq

let apply_subst_to_premise ?renaming subst offset premise =
  let map_args args = List.map
    (fun t -> S.apply_subst ?renaming ~recursive:false subst (t,offset))
    args in
  match premise with
  | IfNamed (name, args) -> IfNamed (name, map_args args)
  | IfTheory (name, args) -> IfTheory (name, map_args args)
  | IfPattern (p, args) -> IfPattern (p, map_args args)

(** Rename variables in gc and premises, return the list of variables
    that map to [gc.gc_vars] and the new list of premises. *)
let rename_gc gc premises =
  let offset = T.max_var
    (Sequence.to_list (Sequence.flatMap vars_of_premise
      (Sequence.of_list premises))) + 1 in
  (* subst that maps variables to fresh variables *)
  let _, subst = List.fold_left
    (fun (i,subst) v ->
      let v' = T.mk_var i v.sort in
      i+1, S.bind ~recursive:false subst (v,1) (v',0))
    (offset,S.id_subst) gc.gc_vars in
  (* fresh variables *)
  let vars = List.map
    (fun v -> S.apply_subst ~recursive:false subst (v,1))
    gc.gc_vars in
  (* rename variables in premises *)
  let premises' = List.map (apply_subst_to_premise subst 1) premises in
  vars, premises'

(** Translate a definition into a Datalog clause *)
let definition_to_datalog definition =
  match definition with
  | Named (name, ((p, sorts) as pattern)) ->
    let vars = List.mapi (fun i sort -> encode_term ~to_var:true (T.mk_var i sort)) sorts in
    let concl = atom_named name vars in
    let premises = [atom_pattern pattern vars] in
    Logic.mk_clause concl premises
  | Lemma ((p, args), premises) ->
    let premises = List.map premise_to_datalog premises in 
    let concl = atom_pattern p (List.map (encode_term ~to_var:true) args) in
    Logic.mk_clause concl premises
  | Theory ((name, args), premises) ->
    let premises = List.map premise_to_datalog premises in 
    let concl = atom_theory name (List.map (encode_term ~to_var:true) args) in
    Logic.mk_clause concl premises
  | GC (gc, premises) ->
    (* rename variables, apply renaming to premises *)
    let vars, premises' = rename_gc gc premises in
    let premises' = List.map premise_to_datalog premises' in 
    let concl = atom_gc gc vars in
    Logic.mk_clause concl premises'

let definition_to_goals definition =
  let convert_list l = List.mapi (fun i _ -> `Var (-i-2)) l in
  match definition with
  | Named (name, (p, sorts)) -> []
  | Lemma ((_, args), _) ->
    [Logic.mk_literal (MString "pattern") ((`Var (-1)) :: convert_list args)]
  | Theory ((_, args), _) ->
    [Logic.mk_literal (MString "theory") ((`Var (-1)) :: convert_list args)]
  | GC (gc,_) ->
    let n = List.length gc.gc_eqns + List.length gc.gc_vars +
      List.length gc.gc_prec + 1 in
    let args = Sequence.unfoldr
      (fun i -> if i >= n then None else Some (`Var (-i-2), i+1))
      0 in
    [Logic.mk_literal (MString "gc") ((`Var (-1)) :: Sequence.to_list args)]

(** Convert a meta-fact to a Datalog fact *)
let fact_to_datalog fact =
  match fact with
  | ThenPattern (p, args) ->
    atom_pattern p (List.map (encode_term ~to_var:true) args)
  | ThenTheory (name, args) ->
    atom_theory name (List.map (encode_term ~to_var:true) args)
  | ThenGC _ ->
    failwith "Meta.KB.fact_to_datalog makes no sense for ThenGC"
  | ThenNamed _ ->
    failwith "Meta.KB.fact_to_datalog makes no sense for ThenNamed"

(** Try to convert back a Datalog fact into a meta-fact *)
let of_datalog lit =
  match Logic.open_literal lit with
  | MString "pattern", (`Symbol (MPattern p) :: args) ->
    let terms = extract_terms (snd p) args in
    Some (ThenPattern (p, terms))
  | MString "named", (`Symbol (MString name) :: args) ->
    let terms = extract_terms_unsafe args in
    Some (ThenNamed (name, terms))
  | MString "theory", (`Symbol (MString name) :: args) ->
    let terms = extract_terms_unsafe args in
    Some (ThenTheory (name, terms))
  | MString "gc", (`Symbol (MString gc_ord) :: `Symbol (MString gc_theory) :: args) ->
    (* extract (list of terms, list of patterns, list of terms) . [at_prec]
       is true if we are reading the first sequence of terms, ie
       the precedence. *) 
    let rec extract at_prec (prec,pats,vars) l = match l with
    | [] -> List.rev prec, List.rev pats, List.rev vars
    | (`Symbol (MPatternVars (p,p_vars)))::l' -> extract false (prec,(p,p_vars)::pats,vars) l'
    | (`Symbol (MTerm t))::l' when at_prec -> extract true (t::prec,pats,vars) l'
    | (`Symbol (MTerm t))::l' -> extract false (prec,pats,t::vars) l'
    | (`Symbol atom)::_ -> failwith (Utils.sprintf "bad atom %a" pp_atom atom)
    | (`Var _)::_ -> failwith "unexpected variable"
    in
    let gc_prec, gc_eqns, terms = extract true ([],[],[]) args in
    Utils.debug 4 "got @[<h>prec %a, args %a@]"
      (Utils.pp_list !T.pp_term#pp) gc_prec
      (Utils.pp_list !T.pp_term#pp) terms;
    (* split gc_vars into proper variables, and their arguments *)
    let n = List.length terms in
    assert ((n mod 2) = 0);
    let gc_vars = Utils.list_take (n/2) terms
    and args = Utils.list_drop (n/2) terms in
    Some (ThenGC ({ gc_prec; gc_theory; gc_eqns; gc_vars; gc_ord; }, args))
  | _ -> None 

(** {2 Knowledge Base} *)

module DefinitionSet = Sequence.Set.Make(struct
  type t = definition
  let compare = compare_definitions
end)

type t = DefinitionSet.set
  (** The knowledge base. *)

let empty = DefinitionSet.empty

let add_definition kb d = DefinitionSet.add d kb

let add_definitions kb seq = Sequence.fold add_definition kb seq

let to_seq kb = DefinitionSet.to_seq kb

let of_seq kb definitions =
  DefinitionSet.union kb (DefinitionSet.of_seq definitions)

(** {2 Printing/parsing} *)

let pp formatter kb =
  Format.fprintf formatter "@[<v2>KB:@;%a@]"
    (Sequence.pp_seq ~sep:"" pp_definition) (to_seq kb)

let to_json kb : json Stream.t =
  let definitions = Sequence.map definition_to_json (to_seq kb) in
  Sequence.to_stream definitions

let of_json kb (json : json Stream.t) : t =
  let seq = Sequence.of_stream json in
  let seq = Sequence.map definition_of_json seq in
  of_seq kb seq

(** {2 Saving/restoring KB from disk} *)

let save ~file kb =
  try
    let out = Gzip.open_out file in
    let data = Json.stream_to_string (to_json kb) in
    Utils.debug 1 "%% %d bytes for storing the raw KB" (String.length data);
    Gzip.output out data 0 (String.length data);
    Utils.debug 1 "%% wrote KB";
    Gzip.close_out out
  with Gzip.Error e | Zlib.Error (e, _) ->
    Utils.debug 0 "%% error trying to write KB to %s: %s" file e;
    ()

let restore ~file kb =
  try
    let input = Gzip.open_in file in
    (* parse JSON steam *)
    let lexbuf = Lexing.from_function (fun s len -> Gzip.input input s 0 len) in
    let lexer = Json.init_lexer () in
    let stream : json Stream.t = Json.stream_from_lexbuf lexer lexbuf in
    let kb = of_json kb stream in
    Gzip.close_in input;
    kb
  with Gzip.Error e | Zlib.Error (e, _) ->
    Utils.debug 0 "%% error trying to read KB from %s: %s" file e;
    kb
    
