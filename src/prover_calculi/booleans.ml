
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

type selection_setting = Any | NotConnective | Large
let _axioms_enabled = ref false
let cased_term_selection = ref Any

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let (=~),(/~) = Literal.mk_eq, Literal.mk_neq
  let (@:) = T.app_builtin ~ty:Type.prop
  let no a = a =~ T.false_
  let yes a = a =~ T.true_
  let imply a b = Builtin.Imply @: [a;b]
  let const_true p = T.fun_ (List.hd @@ fst @@ Type.open_fun (T.ty p)) T.true_

  let true_not_false = [T.true_ /~ T.false_]
  let true_or_false a = [yes a; a =~ T.false_]
  let imp_true1 a b = [yes a; yes(imply a b)]
  let imp_true2 a b = [no b; yes(imply a b)]
  let imp_false a b = [no(imply a b); no a; yes b]
  let all_true p = [p /~ const_true p; yes(Builtin.ForallConst@:[p])]
  let all_false p = [no(Builtin.ForallConst@:[p]); p =~ const_true p]
  let eq_true x y = [x/~y; yes(Builtin.Eq@:[x;y])]
  let eq_false x y = [no(Builtin.Eq@:[x;y]); x=~y]
  let and_ a b = [Builtin.And @: [a;b] 
                    =~  imply (imply a (imply b T.false_)) T.false_]
  let or_ a b = [Builtin.Or @: [a;b] 
                    =~  imply (imply a T.false_) b] 

  let and_true a  = [Builtin.And @: [T.true_; a] =~ a]
  let and_false a  = [Builtin.And @: [T.false_; a] =~ T.false_]
  
  let exists t = 
    let t2bool = Type.arrow [t] Type.prop in
    [T.app_builtin ~ty:(Type.arrow [t2bool] Type.prop) Builtin.ExistsConst [] =~ T.fun_ t2bool
      (Builtin.Not @:[Builtin.ForallConst @:[T.fun_ t (Builtin.Not @:[T.app (T.bvar t2bool 1) [T.bvar t 0]])]])]
  
  let as_clause c = Env.C.create ~penalty:1 ~trail:Trail.empty c Proof.Step.trivial

  let create_clauses () = 
    (*let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let b = T.var (HVar.make ~ty:Type.prop 1) in
    let p = T.var (HVar.make ~ty:(Type.arrow [alpha] Type.prop) 1) in
    let x = T.var (HVar.make ~ty:alpha 1) in
    let y = T.var (HVar.make ~ty:alpha 2) in*)
    let a = T.var (HVar.make ~ty:Type.prop 0) in
    [
      [Builtin.And @:[T.true_; a] =~ a];
	  [Builtin.And @:[T.false_; a] =~ T.false_];
	  [Builtin.Or @:[T.true_; a] =~ T.true_];
	  [Builtin.Or @:[T.false_; a] =~ a];
	  [Builtin.Imply @:[T.true_; a] =~ a];
	  [Builtin.Imply @:[T.false_; a] =~ T.true_];
	  [Builtin.Not @:[T.true_] =~ T.false_];
	  [Builtin.Not @:[T.false_] =~ T.true_];
	  (*
	  imp_true1 a b; imp_true2 a b; imp_false a b; 
      and_ a b;
      all_true p; all_false p;
	  eq_true x y; eq_false x y; 
      (*not; exists alpha;*)
      or_ a b;
      (*and_false a; and_true a;*)
	  *)
    ] |> List.map as_clause |> Iter.of_list

  let bool_cases(c: C.t) : C.t list =
  let term_as_true = Hashtbl.create 8 in
  let term_as_false = Hashtbl.create 4 in
	let rec find_bools top t =
		let can_be_cased = Type.is_prop(T.ty t) && T.DB.is_closed t && not top in
		(* Add only propositions. *)
		let add = if can_be_cased then Hashtbl.add term_as_true else fun _ _ -> () in
		let yes = if can_be_cased then yes else fun _ -> yes T.true_ in
		(* Stop recursion in combination of certain settings. *)
		let inner f x = if can_be_cased && !cased_term_selection = Large then () else List.iter(f false) x in
		match T.view t with
			| DB _ | Var _ -> ()
			| Const _ -> add t (yes t)
			| Fun(_,b) -> find_bools false b
			| App(f,ps) -> add t (yes t); inner find_bools (f::ps)
			| AppBuiltin(f,ps) ->
				inner find_bools ps;
				match f with
					| Builtin.True | Builtin.False -> ()
					| Builtin.Eq | Builtin.Neq | Builtin.Equiv | Builtin.Xor ->
						(match ps with 
              | [x;y] when (!cased_term_selection != NotConnective || Type.is_prop(T.ty x)) ->
                if f = Builtin.Neq || f = Builtin.Xor then(
                  if can_be_cased then Hashtbl.add term_as_false t (x =~ y);
                  add t (x /~ y)
                )else
                  add t (x =~ y)
              | _ -> ())
					| Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
						if !cased_term_selection != NotConnective then add t (yes t) else()
					| _ -> add t (yes t)
	in
	Literals.Seq.terms(C.lits c) |> Iter.iter(find_bools true);
	let case polarity b b_lit clauses =
		let proof = Proof.Step.inference[C.proof_parent c]
			~rule:(Proof.Rule.mk"bool_cases")
		in
		C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
			(b_lit :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:polarity)))
		proof :: clauses
	in
	Hashtbl.fold(case T.false_) term_as_true [] @
	Hashtbl.fold(case T.true_) term_as_false []

  let simpl_eq_subterms c =
    let simplified = ref false in
    let new_lits = 
      C.Seq.terms c |>
      Iter.flat_map T.Seq.subterms
      |> Iter.fold (fun acc t -> 
        match T.view t with
        | T.AppBuiltin(hd, [lhs;rhs]) when T.equal lhs rhs -> 
            if Builtin.equal hd Builtin.Eq  || Builtin.equal hd Builtin.Equiv then (
              simplified := true;
              Literals.map (T.replace ~old:t ~by:T.true_) acc
            ) else if Builtin.equal hd Builtin.Neq  || Builtin.equal hd Builtin.Xor then (
              simplified := true;
              Literals.map (T.replace ~old:t ~by:T.false_) acc
            ) else  acc
        | _ -> acc) (C.lits c)
       in
    if not (!simplified) then (
      SimplM.return_same c
    ) else (
      let proof = Proof.Step.inference [C.proof_parent c] ~rule:(Proof.Rule.mk "simplify trivial (in)equalities") in
      let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) (Array.to_list new_lits) proof in
      SimplM.return_new new_
    )


  let setup () =
	if !_axioms_enabled then(
		Env.ProofState.ActiveSet.add (create_clauses () );
		Env.add_unary_inf "bool_cases" bool_cases;
    	Env.add_basic_simplify simpl_eq_subterms;
	)
end


let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "bool";
      env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--boolean-axioms", Arg.Bool (fun b -> _axioms_enabled := b), 
      " enable/disable boolean axioms";
      "--bool-subterm-selection", 
      Arg.Symbol(["A"; "N"; "L"], (fun opt -> if opt = "A" then cased_term_selection := Any
                                            else if opt = "N" then cased_term_selection := NotConnective
                                            else cased_term_selection := Large)), 
      " select boolean subterm selection criterion: A for any, N for not a connective and L for large"
        ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Extensions.register extension