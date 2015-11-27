
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

(** {1 Preprocessing for E}

We preprocess the work for E in two ways:
- removing arithmetic-specific things to replace them
   with rewrite rules and a symbol encoding of numbers
- detecting algebraic theories and providing E with rewrite systems
   LPO-oriented on the fly
*)

open Logtk
open Logtk_parsers
open Logtk_meta
open Logtk_solving

module PT = PrologTerm
module HOT = HOTerm
module E = CCError
module Loc = ParseLocation

(** OPTIONS *)

let theory_files = ref []
let files = ref []
let balanced_int_file = ref "data/rewriting/balanced_int.p"
let timeout = ref None
let flag_print_theory = ref false
let flag_print_signature = ref false
let flag_print_rules = ref false
let flag_print_problem = ref false
let flag_print_detected = ref false
let flag_balanced_arith = ref false
let flag_print_precedence = ref false
let flag_print_e_output = ref false
let flag_use_ord = ref true

let add_file f = files := f :: !files
let add_theory f = theory_files := f :: !theory_files

let options =
  [ "-theory", Arg.String add_theory, "use given theory file"
  ; "-balanced-arith", Arg.Set flag_balanced_arith, "enable balanced-int arith"
  ; "-balanced-rules-file", Arg.Set_string balanced_int_file,
      "file storing the rewrite rule for balanced-int encoding"
  ; "-timeout", Arg.Int (fun n -> timeout := Some n), "timeout (in seconds)"
  ; "-print-problem", Arg.Set flag_print_problem, "print the final problem sent to E"
  ; "-print-theory", Arg.Set flag_print_theory, "print the whole theory"
  ; "-print-signature", Arg.Set flag_print_signature, "print the signature of the theory"
  ; "-print-rules", Arg.Set flag_print_rules, "print the rewrite rules"
  ; "-print-detected", Arg.Set flag_print_detected, "print the detected theories and others"
  ; "-print-precedence", Arg.Set flag_print_precedence, "print precedence given to E"
  ; "-print-e-output", Arg.Set flag_print_e_output, "print output of E"
  ; "-no-ord", Arg.Clear flag_use_ord, "disable LPO ordering generation"
  ] @ Options.global_opts

(** MAIN OPERATIONS *)

(* parse the given theory files into the prover *)
let parse_theory_files prover files =
  E.fold_l
    (fun p file -> E.map fst (Prover.parse_file p file))
    prover files

(* parse several TPTP files into declarations *)
let parse_tptp_files files =
  let q = Queue.create () in
  let res = E.(
    fold_l
      (fun () file ->
        Util_tptp.parse_file ~recursive:true file
        >>= fun decls ->
        Queue.push decls q;
        E.return ()
      ) () files
    )
  in match res with
  | `Error msg -> E.fail msg
  | `Ok () ->
      E.return (Sequence.of_queue q |> Sequence.flatten)

(* extract clauses from decls *)
let clauses_of_decls decls =
  fun k ->
    let a = object
      inherit [unit] Ast_tptp.Typed.visitor
      method clause () role c = k (role, c)
    end in
    decls (a#visit ())

(* result of detecting theories *)
type detection_result = {
  lemmas : Plugin.foclause Sequence.t;
  theories : (string * Type.t list * HOT.t) Sequence.t;
  axioms : (string * Type.t list * HOT.t) Sequence.t;
  rewrite : (FOTerm.t * FOTerm.t) list Sequence.t;
  pre_rewrite : HORewriting.t Sequence.t;
}

let merge_detection_result a b =
  { lemmas = Sequence.append a.lemmas b.lemmas
  ; theories = Sequence.append a.theories b.theories
  ; axioms = Sequence.append a.axioms b.axioms
  ; rewrite = Sequence.append a.rewrite b.rewrite
  ; pre_rewrite = Sequence.append a.pre_rewrite b.pre_rewrite
  }

(* convert a sequence of clauses to a sequence of untyped declarations *)
let erase_types decls =
  let module M = Ast_tptp.Map(Ast_tptp.Typed)(Ast_tptp.Untyped) in
  Sequence.map
    (M.map ~form:(Formula.FO.to_prolog ~depth:0)
      ~ho:(fun _ -> failwith "HOT.to_prolog missing")
      ~ty:(Type.Conv.to_prolog ~curry:false ~depth:0))
    decls

(** STATE: STORE RULES, AXIOMS, PRE-REWRITE RULES *)

module State = struct
  type t = {
    prover : Prover.t;
    detected : detection_result;
    rewrite : (FOTerm.t * FOTerm.t) list;  (* all rewrite rules *)
    pre_rewrite : HORewriting.t;   (* FIXME: rewrite on formulas/clauses, not HOTerm *)
    axioms : Formula.FO.t list;  (* other axioms *)
    signature : Signature.t;
  }

  let empty = {
    prover = Prover.empty;
    detected = {
      lemmas = Sequence.empty;
      theories = Sequence.empty;
      axioms = Sequence.empty;
      rewrite = Sequence.empty;
      pre_rewrite = Sequence.empty;
    };
    rewrite=[];
    pre_rewrite=HORewriting.empty;
    axioms=[];
    signature=Signature.TPTP.base;
  }

  let of_prover prover = { empty with prover; }
end

(* detect theories in clauses, update state *)
let detect_theories ~st clauses =
  let facts = clauses
    |> Sequence.map Plugin.holds#to_fact
    |> Sequence.map Reasoner.Clause.fact
  in
  (* add clauses (ignore prover) *)
  let prover', consequences = Prover.Seq.of_seq st.State.prover facts in
  let consequence_terms = Sequence.map fst consequences in
  (* filter theories, axioms, lemmas... *)
  let theories = Sequence.fmap Plugin.theory#of_fact consequence_terms
  and lemmas = Sequence.fmap Plugin.lemma#of_fact consequence_terms
  and axioms = Sequence.fmap Plugin.axiom#of_fact consequence_terms
  and rewrite = Sequence.fmap Plugin.rewrite#of_fact consequence_terms
  and pre_rewrite = Sequence.fmap Plugin.pre_rewrite#of_fact consequence_terms
  in
  {st with
    State.prover=prover';
    State.detected =
      merge_detection_result st.State.detected
      { theories; lemmas; axioms; rewrite; pre_rewrite; };
    State.rewrite = List.rev_append
      (Sequence.to_rev_list rewrite |> List.concat |> List.rev)
      st.State.rewrite;
    State.pre_rewrite = Sequence.fold HORewriting.merge
      st.State.pre_rewrite pre_rewrite;
  }

(** ENCODING ARITH *)

module BalancedInt = struct
  module T = FOTerm
  module F = Formula.FO

  (* intermediate representation *)
  type b_int = Zero | Z1 of b_int | Z0 of b_int | Zj of b_int

  let __3 = Z.of_int 3

  (* convert Z.t into b_int *)
  let rec bint_of_int n = match Z.sign n with
    | 0 -> Zero
    | s when s < 0 -> z_opp (bint_of_int (Z.neg n))
    | _ ->
        let q3, rem3 = Z.div_rem n __3 in
        match Z.to_int rem3 with
        | 0 -> Z0 (bint_of_int q3)
        | 1 -> Z1 (bint_of_int q3)
        | 2 -> Zj (bint_of_int (Z.add Z.one q3))
        | _ -> assert false
  and z_opp = function
  | Zero -> Zero
  | Zj x -> Z1 (z_opp x)
  | Z0 x -> Z0 (z_opp x)
  | Z1 x -> Zj (z_opp x)
  and int_of_bint = function
  | Zero -> Z.zero
  | Z0 x -> Z.mul __3 (int_of_bint x)
  | Z1 x -> Z.add (Z.mul __3 (int_of_bint x)) Z.one
  | Zj x -> Z.sub (Z.mul __3 (int_of_bint x)) Z.minus_one

  (* convert b_int into term *)
  let __ty1 = Type.(TPTP.int <=. TPTP.int)
  let __z_0 = T.const ~ty:Type.TPTP.int (Symbol.of_string "z_0")
  let __z_3 = T.const ~ty:__ty1 (Symbol.of_string "z_3")
  let __z_3p1 = T.const ~ty:__ty1 (Symbol.of_string "z_3p1")
  let __z_3m1 = T.const ~ty:__ty1 (Symbol.of_string "z_3m1")

  let __mk_z3 x = T.app __z_3 [x]
  let __mk_z3p1 x = T.app __z_3p1 [x]
  let __mk_z3m1 x = T.app __z_3m1 [x]

  let rec term_of_b_int = function
    | Zero -> __z_0
    | Z0 x -> __mk_z3 (term_of_b_int x)
    | Z1 x -> __mk_z3p1 (term_of_b_int x)
    | Zj x -> __mk_z3m1 (term_of_b_int x)

  (* conversion of terms *)

  let __ty2 = Type.(TPTP.int <== [TPTP.int; TPTP.int])
  let __ty_bool = Type.const (Symbol.of_string "z_bool")
  let __ty2b = Type.(__ty_bool <== [TPTP.int; TPTP.int])

  let __z_opp = T.const ~ty:__ty1 (Symbol.of_string "z_opp")
  let __z_plus = T.const ~ty:__ty2 (Symbol.of_string "z_plus")
  let __z_minus = T.const ~ty:__ty2 (Symbol.of_string "z_minus")
  let __z_mult = T.const ~ty:__ty2 (Symbol.of_string "z_mult")

  let __b_true = T.const ~ty:__ty_bool (Symbol.of_string "b_true")
  let __b_false = T.const ~ty:__ty_bool (Symbol.of_string "b_false")
  let __b_istrue =
    T.const ~ty:Type.(TPTP.o <=. __ty_bool) (Symbol.of_string "b_istrue")

  let __z_less = T.const ~ty:__ty2b (Symbol.of_string "z_lt")
  let __z_lesseq = T.const ~ty:__ty2b (Symbol.of_string "z_leq")
  let __z_greater = T.const ~ty:__ty2b (Symbol.of_string "z_gt")
  let __z_greatereq = T.const ~ty:__ty2b (Symbol.of_string "z_geq")

  let __mk_istrue t = T.app __b_istrue [t]

  module SA = Symbol.TPTP.Arith

  let rec convert_term t =
    let hd, tyargs, args = T.open_app t in
    match T.view hd, args with
    | T.Const s, [a;b] when Symbol.equal s SA.sum ->
        T.app __z_plus [convert_term a; convert_term b]
    | T.Const s, [a;b] when Symbol.equal s SA.difference ->
        T.app __z_minus [convert_term a; convert_term b]
    | T.Const s, [a;b] when Symbol.equal s SA.product ->
        T.app __z_mult [convert_term a; convert_term b]
    | T.Const s, [a] when Symbol.equal s SA.uminus ->
        T.app __z_opp [convert_term a]
    | T.Const s, [a;b] when Symbol.equal s SA.less ->
        __mk_istrue (T.app __z_less [convert_term a; convert_term b])
    | T.Const s, [a;b] when Symbol.equal s SA.lesseq ->
        __mk_istrue (T.app __z_lesseq [convert_term a; convert_term b])
    | T.Const s, [a;b] when Symbol.equal s SA.greater ->
        __mk_istrue (T.app __z_greater [convert_term a; convert_term b])
    | T.Const s, [a;b] when Symbol.equal s SA.greatereq ->
        __mk_istrue (T.app __z_greatereq [convert_term a; convert_term b])
    | T.Const (Symbol.Int n), [] ->
        (* convert integer literal *)
        term_of_b_int (bint_of_int n)
    | T.Const (Symbol.Rat _), _ ->
        failwith "cannot deal with rationnals"
    | (T.Var _ | T.BVar _), [] -> t
    | _ -> T.app_full hd tyargs (List.map convert_term args)

  let convert_form = F.map convert_term

  let convert_decl d = Ast_tptp.Typed.map ~form:convert_form d

  (* signature+list of rules from file. *)
  let rules ~signature ~filename =
    E.(
        begin try E.return (open_in filename)
        with Sys_error msg ->
          let msg = Printf.sprintf "could not open file %s: %s" filename msg in
          E.fail msg
        end >>= fun ic ->
        RewriteRules.parse_file filename ic >>=
        RewriteRules.rules_of_pairs signature >>= fun (signature,rules') ->
        E.return (signature, rules')
    )

  (* axioms:
      istrue(b_true)
      ~istrue(b_false)
      !X:bool,  X=b_true | X=b_false
  *)
  let axioms =
    let b = T.var ~ty:__ty_bool 0 in
    let x = T.var ~ty:Type.TPTP.int 0 in
    let y = T.var ~ty:Type.TPTP.int 1 in
    let z = T.var ~ty:Type.TPTP.int 2 in
    [ F.Base.atom (__mk_istrue __b_true)
    ; F.Base.not_ (F.Base.atom (__mk_istrue __b_false))
    ; F.Base.or_ [F.Base.eq b __b_true; F.Base.eq b __b_false]
    ; F.Base.neq __b_true __b_false
    ; F.Base.or_ [F.Base.neq (__mk_z3 x) x; F.Base.eq x __z_0]  (* x!=0 -> 3x != x *)
    ; F.Base.neq (__mk_z3p1 x) x
    ; F.Base.neq (__mk_z3m1 x) x
    ; F.Base.eq (T.app __z_plus [x; T.app __z_plus [y;z]])  (* + is AC *)
                (T.app __z_plus [T.app __z_plus [x;y]; z])
    ; F.Base.eq (T.app __z_plus [x;y]) (T.app __z_plus [y;x])
    ; F.Base.eq (T.app __z_mult [x; T.app __z_mult [y;z]])  (* × is AC *)
                (T.app __z_mult [T.app __z_mult [x;y]; z])
    ; F.Base.eq (T.app __z_mult [x;y]) (T.app __z_mult [y;x])
    ]

  (* parse axioms from fil, update state. *)
  let axioms_and_rules ~filename st =
    E.(
      rules ~signature:st.State.signature ~filename
      >>= fun (signature,rules) ->
      let st = {st with
      State.signature = signature;
        State.axioms = List.rev_append axioms st.State.axioms;
        State.rewrite = List.rev_append rules st.State.rewrite;
      } in
      E.return st
    )
end

(* tff type decls *)
let _tff_type_decls =
  let ty2int = Type.(TPTP.int <== [TPTP.int; TPTP.int]) in
  [ Symbol.TPTP.Arith.less, Type.(TPTP.o <== [TPTP.int; TPTP.int])
  ; Symbol.TPTP.Arith.lesseq, Type.(TPTP.o <== [TPTP.int; TPTP.int])
  ; Symbol.TPTP.Arith.greater, Type.(TPTP.o <== [TPTP.int; TPTP.int])
  ; Symbol.TPTP.Arith.greatereq, Type.(TPTP.o <== [TPTP.int; TPTP.int])
  ; Symbol.TPTP.Arith.uminus, Type.(TPTP.int <=. TPTP.int)
  ; Symbol.TPTP.Arith.sum, ty2int
  ; Symbol.TPTP.Arith.difference, ty2int
  ; Symbol.TPTP.Arith.product, ty2int
  ; Symbol.TPTP.Arith.quotient, ty2int
  ; Symbol.TPTP.Arith.quotient_e, ty2int
  ; Symbol.TPTP.Arith.quotient_f, ty2int
  ; Symbol.TPTP.Arith.quotient_t, ty2int
  ; Symbol.TPTP.Arith.remainder_e, ty2int
  ; Symbol.TPTP.Arith.remainder_f, ty2int
  ; Symbol.TPTP.Arith.remainder_t, ty2int
  ] |> Sequence.of_list
    |> Sequence.mapi
      (fun i (s,ty) ->
        let name = Ast_tptp.NameString (CCFormat.sprintf "hyst_ty_decl_%d" i) in
        Ast_tptp.Typed.TypeDecl(name, Symbol.TPTP.to_string s, ty, []))

let _tff_type_decls_untyped =
  erase_types _tff_type_decls

(* encoding of Int arithmetic into TFF0 *)
let convert_balanced_arith decls =
  (* convert declarations (encode ints, etc.) *)
  let decls = Sequence.map BalancedInt.convert_decl decls in
  Sequence.persistent decls

(* given a partial ordering from LPO, build arguments for E *)
let precedence_to_E_arg prec =
  match prec with
  | [] -> []
  | _ ->
      let prec_str = String.concat ","
        (List.map
          (fun (l,r) -> CCFormat.sprintf "%a>%a" Symbol.pp l Symbol.pp r)
          prec)
      in
      ["--term-ordering=LPO4"; "--precedence='" ^ prec_str ^ "'"; "-G"; "invfreq" ]

(* list of rules -> list of formulas *)
let axioms_of_rules rules =
  List.map (fun (l,r) -> Formula.FO.Base.eq l r) rules

(* list of axioms -> sequence of TPTP declarations *)
let axioms_to_decls axioms =
  axioms
    |> Sequence.of_list
    |> Sequence.mapi
      (fun i f ->
        let name = Ast_tptp.NameString (CCFormat.sprintf "hyst_axiom_%d" i) in
        Ast_tptp.Typed.TFF(name, Ast_tptp.R_axiom, f, []))

(* is the type a boring FOF type? *)
let _standard_tptp_ty ty =
  Type.Seq.sub ty
    |> Sequence.for_all
      (fun ty -> match Type.view ty with
        | Type.App (s, _) -> Symbol.equal s Symbol.TPTP.i || Symbol.equal s Symbol.TPTP.o
        | _ -> true)

(* type declarations to pre-prend *)
let ty_declarations decls =
  let signature = decls
    |> Sequence.flatMap Ast_tptp.Typed.Seq.forms
    |> Sequence.flatMap Formula.FO.Seq.terms
    |> Sequence.flatMap FOTerm.Seq.typed_symbols
    |> Signature.Seq.of_seq
  in
  (* remove "standard" types that don't need be declared *)
  let signature = Signature.diff signature Signature.TPTP.base in
  let signature = Signature.filter signature
    (fun _ ty -> not (_standard_tptp_ty ty)) in
  signature
    |> Signature.Seq.to_seq
    |> Sequence.mapi
        (fun i (s,ty) ->
          let name = Ast_tptp.NameString (CCFormat.sprintf "hyst_decl_%d" i) in
          Ast_tptp.Typed.TypeDecl (name, Symbol.to_string s, ty, []))

(** PRINTERS *)

(* print content of the reasoner *)
let print_theory r =
  Reasoner.Seq.to_seq r
    |> Format.printf "theory: @[%a@]@." (CCFormat.seq ~sep:" " Reasoner.Clause.pp);
  ()

let pp_theory_axiom out (name, _, t) =
  Format.fprintf out "%s %a" name HOT.pp t

let pp_rewrite_system out l =
  Format.fprintf out "rewrite system\n    ";
  CCFormat.list ~sep:" "
    (fun out (a,b) -> Format.fprintf out "%a --> %a" FOTerm.pp a FOTerm.pp b)
    out l

let pp_pre_rewrite_system buf l =
  HORewriting.pp buf l

let print_result {theories; lemmas; axioms; rewrite; pre_rewrite; } =
  Format.printf "axioms: @[%a@]@."
    (CCFormat.seq ~sep:"  " pp_theory_axiom) axioms;
  Format.printf "theories: @[%a@]@."
    (CCFormat.seq ~sep:"\n  " pp_theory_axiom) theories;
  Format.printf "lemmas: @[%a@]@."
    (CCFormat.seq ~sep:"\n  " (Encoding.pp_clause FOTerm.pp)) lemmas;
  Format.printf "rewrite systems: @[%a@]@."
    (CCFormat.seq ~sep:"\n  " pp_rewrite_system) rewrite;
  Format.printf "pre-rewrite systems: @[%a@]@."
    (CCFormat.seq ~sep:"\n  " pp_pre_rewrite_system) pre_rewrite;
  ()

let print_signature signature =
  Format.printf "signature: @[%a@]@."
    (CCFormat.seq ~sep:" "
      (fun out (a,b) -> Format.fprintf out "%a : %a" Symbol.pp a Type.pp b))
    (Signature.Seq.to_seq signature)

let print_problem decls =
  Format.printf "problem: @[%a@]@."
    (CCFormat.seq ~sep:"  " Ast_tptp.Untyped.pp) decls

let print_precedence prec =
  Format.printf "precedence: @[%a@]@." Lpo.Solution.pp prec

(** MAIN *)

let parse_args () =
  let help_msg = "hysteresis: preprocessor for E" in
  Arg.parse options add_file help_msg;
  if !theory_files = []
    then theory_files := ["data/builtin.theory"];
  ()

let reduce_to_cnf_with_E decls =
  (* use E to reduce to CNF, also declare types of TFF predicates *)
  let decls' = Sequence.append _tff_type_decls_untyped decls in
  Util.debug 5 "cnf decls:\n  %a\n"
    (fun k->k (CCFormat.seq ~sep:" " Ast_tptp.Untyped.pp) decls');
  CallProver.Eprover.cnf (* XXX ~opts:["--free-numbers"] *) decls'

let main () =
  E.(
    (* parse theory, obtain a loaded meta-prover *)
    Util.debug 2 "load theory files..." (fun _ ->());
    parse_theory_files Prover.empty !theory_files
    >>= fun prover ->
    if !flag_print_theory
      then print_theory (Prover.reasoner prover);
    if !flag_print_signature
      then print_signature (Prover.signature prover);
    (* parse problem *)
    Util.debug 2 "read problem files..." (fun _ ->());
    parse_tptp_files !files
    >>= fun decls ->
    (* extract into typed clauses *)
    Util.debug 2 "infer types..." (fun _ ->());
    Util_tptp.infer_types (`sign Signature.TPTP.Arith.full) decls
    >>= fun (signature, decls) ->
    Util.debug 2 "reduce to CNF..." (fun _ ->());
    (* reduce to CNF *)
    let _signature, decls = Util_tptp.to_cnf signature decls in
    (* global state *)
    let state = State.of_prover prover in
    (* detect theories (selecting only clauses) *)
    let state = decls
      |> Sequence.fmap
        (function
          | Ast_tptp.Typed.CNF(_,_,c,_) -> Some c
          | _ -> None)
      |> Sequence.map Encoding.foclause_of_clause
      |> detect_theories ~st:state
    in
    if !flag_print_detected
      then print_result state.State.detected;
    (* enable arith, if required. This might modify state by adding
       rewrite rules and axioms, and also encode declarations *)
    begin if !flag_balanced_arith
      then
        BalancedInt.axioms_and_rules ~filename:!balanced_int_file state
        >>= fun state ->
        let decls = convert_balanced_arith decls in
        E.return (state,decls)
      else E.return (state,decls)
    end
    >>= fun (state,decls) ->
    (* TODO: preprocess *)

    (* orient rewrite system to get a precedence (if flag enabled) *)
    let precedence_args =
      if !flag_use_ord then begin
        Util.debug 2 "orient rewrite rules..." (fun _ ->());
        let orders =
          state.State.rewrite
            |> Lpo.FO.orient_lpo_list
            |> Lpo.solve_multiple
        in
        let precedence = match orders with
          | lazy (LazyList.Cons (prec, _)) ->
              if !flag_print_precedence
                then print_precedence prec;
              if state.State.rewrite <> []
                then print_endline "found ordering for rules";
              prec
          | lazy LazyList.Nil ->
              if state.State.rewrite <> []
                then print_endline "no ordering found for rules";
              []
        in precedence_to_E_arg precedence
      end
      else ["--auto"]
    in
    let args = precedence_args @ ["--memory-limit=512"; "--proof-object"] in
    (* build final sequence of declarations: add rules/axioms, then add
      type declarations *)
    Util.debug 5 "build prelude..." (fun _ ->());
    let prelude_decls =
      axioms_of_rules state.State.rewrite
      |> List.rev_append state.State.axioms
      |> axioms_to_decls
    in
    Util.debug 5 "prelude built" (fun _ ->());
    let decls = Sequence.append prelude_decls decls in
    Util.debug 5 "all decls:\n  %a\n"
      (fun k->k (CCFormat.seq ~sep:" " Ast_tptp.Typed.pp) decls);
    let decls = Sequence.append (ty_declarations decls) decls in
    Util.debug 5 "erase types..." (fun _ -> ());
    let final_decls = erase_types decls in
    (* yield to E *)
    if !flag_print_problem
      then print_problem final_decls;
    let final_decls = List.rev (Sequence.to_rev_list final_decls) in
    Util.debug 1 "call prover now" (fun _ -> ());
    flush stdout;
    CallProver.call_with_out ~args ?timeout:!timeout ~prover:CallProver.Prover.p_E final_decls
  )

let () =
  parse_args ();
  let res = main () in
  match res with
  | `Error msg ->
      print_endline msg;
      exit 1
  | `Ok (CallProver.Sat, _) ->
      print_endline "result: sat."
  | `Ok (CallProver.Unsat, out) ->
      print_endline "result: unsat.";
      if !flag_print_e_output
        then (print_endline "output of E:"; print_endline out)
  | `Ok (CallProver.Unknown, _) ->
      print_endline "result: unknown."
  | `Ok (CallProver.Error s, _) ->
      print_endline ("E failed with error: " ^ s);
      exit 1
