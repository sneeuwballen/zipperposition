
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

let _axioms_enabled = ref false
(* Controls allowed shape of b in boolean case paramodulation C[b] ⟹ b ∨ C[⊥].
0: b is any non-variable boolean term
1: b is not a connective
2: b does not occure only inside other boolean terms *)
let cased_term_selection = ref 0

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
  
  
  (*let not = [T.app_builtin ~ty:(Type.arrow [Type.prop] Type.prop) Builtin.Not [] =~ 
             T.fun_ Type.prop (imply (T.bvar ~ty:Type.prop 0) T.false_)]*)
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
    let assume = Hashtbl.create 8 in
	let rec find_bools top t =
		let ok = Type.is_prop(T.ty t) && T.DB.is_closed t && not top in
		(* Add only propositions. *)
		let add = if ok then Hashtbl.add assume else fun _ _ -> () in
		let yes = if ok then yes else fun _ -> yes T.true_ in
		(* Stop recursion in combination of certain settings. *)
		let inner f x = if ok && !cased_term_selection = 2 then () else List.iter(f false) x in
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
						(match ps with [x;y] when Type.is_prop(T.ty x) && !cased_term_selection!=1 ->
							add t (Literal.mk_lit x y (f = Builtin.Eq || f = Builtin.Equiv))
						|_->())
					| Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
						if !cased_term_selection!=1 then add t (yes t) else()
					| _ -> add t (yes t)
	in
        (*let sub_terms =
      C.Seq.terms c
      |> Iter.flat_map(fun t ->
           T.Seq.subterms_depth t
           |> Iter.filter_map (fun (t,d) -> if d>0 then Some t else None))
      |> Iter.filter(fun t ->
           Type.is_prop(T.ty t) &&
           T.DB.is_closed t &&
           begin match T.view t with
             | T.Const _ | T.App _ -> Some(t =~ T.true_)
             (*| T.AppBuiltin ((Builtin.True | Builtin.False | Builtin.And), _) -> false*)
			 | T.Var _ | T.DB _ -> None
             | T.Fun _ -> assert false (* by typing *)
           	 | T.AppBuiltin (op, ps) -> match op with
				| Builtin.True | Builtin.False -> None
				| Builtin.Eq | Builtin.Neq | Builtin.Equiv | Builtin.Xor -> if Type.is_prop(T.ty(List.hd ps)) then mk_lit 
        end)
      |> T.Set.of_seq
    in*)
	Literals.Seq.terms(C.lits c) |> Iter.iter(find_bools true);
	Hashtbl.fold(fun b b_true clauses ->
		let proof = Proof.Step.inference[C.proof_parent c]
			~rule:(Proof.Rule.mk"bool_cases")
		in
		let new' = C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
			(b_true :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:T.false_)))
		proof
		in
		new' :: clauses
	) assume []


  let setup () =
	if !_axioms_enabled then(
		Env.ProofState.PassiveSet.add (create_clauses () );
		Env.add_unary_inf "bool_cases" bool_cases;
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
      " enable/disable boolean axioms"  ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
    _axioms_enabled := false
  );
  Extensions.register extension