
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Applicative Encoding} *)

open Logtk
open Libzipperposition

let section = Util.Section.make ~parent:Const.section "eq_encode"

let mode_ : [ `None | `Extensional | `Intensional ] ref = ref `None

module T = TypedSTerm

let enabled_ = ref false
let refl_ = ref true
let symm_ = ref true
let trans_ = ref false

let (==>) = T.Ty.(==>)

let decl id ty = Statement.ty_decl ~proof:Proof.Step.trivial id ty

let eq_id = ID.make "$_eq_proxy"
let eq_type = 
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let body = [T.var alpha; T.var alpha] ==> T.Ty.prop in
  T.Ty.close_forall body
let eq_term = T.const ~ty:eq_type eq_id
let eq_clone_tydecl = decl eq_id eq_type

(** Type declarations for these new symbols *)
let ty_decls = 
  Iter.singleton eq_clone_tydecl

let eq_properties () =
  let alpha_var = Var.make ~ty:T.tType (ID.make "alpha") in
  let alpha = T.var alpha_var  in
  let x = T.var (Var.make ~ty:alpha (ID.make "X")) in
  let y = T.var (Var.make ~ty:alpha (ID.make "Y")) in
  let z = T.var (Var.make ~ty:alpha (ID.make "Y")) in

  let app_eq args = T.app ~ty:T.Ty.prop eq_term (alpha::args) in

  let refl =
    [SLiteral.atom_true (app_eq [x; x])] in
  let symm =
    [SLiteral.atom_false (app_eq [x; y]);
     SLiteral.atom_true (app_eq [y; x])] in
  let trans =
    [SLiteral.atom_false (app_eq [x; y]);
     SLiteral.atom_false (app_eq [y; z]);
     SLiteral.atom_true (app_eq [x; z])] in
  
  Iter.of_list ((if !refl_ then [refl] else [])
                 @ (if !symm_ then [symm] else [])
                 @ (if !trans_ then [trans] else []) )

(** Encode a literal *)
let eq_encode_lit lit = 
  Util.debugf ~section 2 "# Encoding Literal %a" (fun k -> k (SLiteral.pp T.pp) lit);
  match lit with
  | SLiteral.Eq(lhs, rhs) | SLiteral.Neq(lhs, rhs) ->
    let sign = match lit with SLiteral.Eq _ -> true | _ -> false in
    let ty = T.ty_exn lhs in
    let encoded = T.app ~ty:T.Ty.prop eq_term [ty; lhs; rhs] in
    SLiteral.atom encoded sign
  | _ -> lit

(** Encode a clause *)
let eq_encode_lits lits = List.map eq_encode_lit lits


exception E_i of ((T.t SLiteral.t) list, T.t, T.t) Statement.t


let pp_in pp_f pp_t pp_ty = function
  | Output_format.O_zf -> Statement.ZF.pp pp_f pp_t pp_ty
  | Output_format.O_tptp -> Statement.TPTP.pp pp_f pp_t pp_ty
  | Output_format.O_normal -> Statement.pp pp_f pp_t pp_ty
  | Output_format.O_none -> CCFormat.silent

let pp_clause_in o =
  let pp_t = T.pp_in o in
  pp_in (Util.pp_list ~sep:" ∨ " (SLiteral.pp_in o pp_t)) pp_t pp_t o

let res_tc =
  Proof.Result.make_tc
    ~of_exn:(function E_i c -> Some c | _ -> None)
    ~to_exn:(fun i -> E_i i)
    ~compare:compare
    ~pp_in:pp_clause_in
    ~is_stmt:true
    ~name:Statement.name
    ~to_form:(fun ~ctx st ->
        let conv_c (c:(T.t SLiteral.t) list) : _ =
          c 
          |> List.map SLiteral.to_form
          |> T.Form.or_
        in
        Statement.Seq.forms st
        |> Iter.map conv_c
        |> Iter.to_list
        |> T.Form.and_)
    ()

(** encode a statement *)
let eq_encode_stmt stmt =
  let as_proof = Proof.S.mk (Statement.proof_step stmt) (Proof.Result.make res_tc stmt) in
  let proof = Proof.Step.esa ~rule:(Proof.Rule.mk "eq_encode") [as_proof |> Proof.Parent.from] in
  match Statement.view stmt with
  | Statement.Data _ -> failwith "Not implemented: Data"
  | Statement.Lemma _ -> failwith "Not implemented: Lemma"
  | Statement.Goal lits -> failwith "Not implemented: Goal"
  | Statement.Def _ | Statement.Rewrite _  | Statement.TyDecl _ -> stmt
  | Statement.NegatedGoal (skolems,clauses) -> 
    Statement.neg_goal ~proof ~skolems (List.map eq_encode_lits clauses)
  | Statement.Assert lits -> Statement.assert_ ~proof (eq_encode_lits lits)


let extension =
  let modifier seq =
    if !enabled_ then (
      let seq = Iter.map eq_encode_stmt seq in
      Util.debug ~section 2 "Start eq encoding";
      let axioms = 
        Iter.map (Statement.assert_ ~proof:Proof.Step.trivial) (eq_properties ()) in
      (* Add type declarations *)
      let seq = Iter.append ty_decls (Iter.append axioms seq) in
      (* Add extensionality axiom *)
      Util.debug ~section 2 "Finished eq encoding"; 
      seq
    ) else seq
  in
  Extensions.(
    { default with name="eq_encode"; post_cnf_modifiers=[modifier]; }
  )

let () =
  Options.add_opts
    [ "--eq-encode", Arg.Bool ((:=) enabled_), " enable/disable replacing equality by proxies";
      "--eq-encode-refl", Arg.Bool ((:=) refl_), " enable/disable eq proxy reflexivity axiom";
      "--eq-encode-symm", Arg.Bool ((:=) symm_), " enable/disable eq proxy symmetry axiom";
      "--eq-encode-trans", Arg.Bool ((:=) trans_), " enable/disable eq proxy transitivity axiom";]