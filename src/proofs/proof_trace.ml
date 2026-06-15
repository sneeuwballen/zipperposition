(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Encoder} *)

open Logtk
module E = Zipperposition_mdag.Encode

type offset = E.offset

external term_of_type : Type.t -> Term.t = "%identity"

module Term_cache = Ephemeron.K1.Make (Term)
module Name_cache = Ephemeron.K1.Make (Name)
module Lit_tbl = CCHashtbl.Make (Literal)
module Int_tbl = CCHashtbl.Make (CCInt)
module Type_cache = CCHashtbl.Make (Type)

type stats = {
  n_steps: int;
  n_terms: int;
}

type t = {
  enc: E.t;
  oc: out_channel;
  tbl_term: offset Term_cache.t;
  tbl_name: offset Name_cache.t;
  tbl_lit: offset Lit_tbl.t;
  tbl_type: offset Type_cache.t;
  mutable n_steps: int;
  mutable n_terms: int;
}

let create oc : t =
  let out =
    object
      method write (s : bytes) (ofs : int) (len : int) = output oc s ofs len
    end
  in
  {
    enc = E.create ~out ();
    oc;
    tbl_term = Term_cache.create 1024;
    tbl_name = Name_cache.create 256;
    tbl_lit = Lit_tbl.create 256;
    tbl_type = Type_cache.create 64;
    n_steps = 0;
    n_terms = 0;
  }

let close self =
  E.flush self.enc;
  close_out self.oc

(** {2 Names} *)

let emit_name self (n : Name.t) : offset =
  match Name_cache.find_opt self.tbl_name n with
  | Some off -> off
  | None ->
    let off =
      E.write_node self.enc "n" (fun enc -> E.string enc (Name.to_string n))
    in
    Name_cache.add self.tbl_name n off;
    off

(** {2 Types} *)

let rec emit_type self (ty : Type.t) : offset =
  match Type_cache.find self.tbl_type ty with
  | off -> off
  | exception Not_found ->
    let off =
      match Type.view ty with
      | Type.Builtin Type.TType -> E.write_node self.enc "ty.type" ignore
      | Type.Builtin Type.Prop -> E.write_node self.enc "ty.prop" ignore
      | Type.Builtin Type.Int -> E.write_node self.enc "ty.int" ignore
      | Type.Builtin Type.Rat -> E.write_node self.enc "ty.rat" ignore
      | Type.Builtin Type.Term -> E.write_node self.enc "ty.term" ignore
      | Type.Builtin Type.Real -> E.write_node self.enc "ty.real" ignore
      | Type.Fun ([ dom ], cod) ->
        E.write_node self.enc "ty.arrow" (fun enc ->
            E.ref enc (emit_type self dom);
            E.ref enc (emit_type self cod))
      | Type.Var _ | Type.DB _ | Type.App _ | Type.Fun _ | Type.Forall _ ->
        emit_term self (term_of_type ty)
    in
    Type_cache.add self.tbl_type ty off;
    off

and emit_term self (t : Term.t) : offset =
  match Term_cache.find_opt self.tbl_term t with
  | Some off -> off
  | None ->
    let off = emit_term_noncached self t in
    self.n_terms <- 1 + self.n_terms;
    Term_cache.add self.tbl_term t off;
    off

and emit_term_noncached self t =
  match Term.view t with
  | Term.Var v ->
    let ty_off = emit_type self (HVar.ty v) in
    E.write_node self.enc "t.v" (fun enc ->
        E.int enc (HVar.id v);
        E.ref enc ty_off)
  | Term.DB i ->
    let ty_off = emit_type self (Term.ty t) in
    E.write_node self.enc "t.d" (fun enc ->
        E.int enc i;
        E.ref enc ty_off)
  | Term.Const name ->
    let n_off = emit_name self name in
    let ty_off = emit_type self (Term.ty t) in
    E.write_node self.enc "t.c" (fun enc ->
        E.ref enc n_off;
        E.ref enc ty_off)
  | Term.App (hd, args) ->
    let hd_off = emit_term self hd in
    let arg_offs = List.map (emit_term self) args in
    E.write_node self.enc "t.@" (fun enc ->
        E.ref enc hd_off;
        List.iter (fun a -> E.ref enc a) arg_offs)
  | Term.Fun (ty, body) ->
    let ty_off = emit_type self ty in
    let body_off = emit_term self body in
    E.write_node self.enc "t.f" (fun enc ->
        E.ref enc ty_off;
        E.ref enc body_off)
  | Term.AppBuiltin (b, args) ->
    let arg_offs = List.map (emit_term self) args in
    E.write_node self.enc "t.b" (fun enc ->
        E.string enc (Builtin.to_string b);
        List.iter (fun a -> E.ref enc a) arg_offs)

(** {2 Literals} *)

let emit_literal self (lit : Literal.t) : offset =
  match Lit_tbl.find self.tbl_lit lit with
  | off -> off
  | exception Not_found ->
    let off =
      match lit with
      | Literal.True -> E.write_node self.enc "l.t" ignore
      | Literal.False -> E.write_node self.enc "l.f" ignore
      | Literal.Equation (t1, t2, sign) ->
        E.write_node self.enc
          (if sign then
             "l.="
           else
             "l.!=")
          (fun enc ->
            E.ref enc (emit_term self t1);
            E.ref enc (emit_term self t2))
    in
    Lit_tbl.add self.tbl_lit lit off;
    off

(** {2 Clauses} *)

let emit_clause self (lits : Literal.t array) =
  E.write_node self.enc "cl" (fun enc ->
      Array.iter (fun lit -> E.ref enc (emit_literal self lit)) lits)

(** {2 Substitutions} *)

external term_var_to_subst_var : Term.var -> Subst.var = "%identity"
external fo_term_to_subst_term : Subst.FO.term -> Subst.term = "%identity"

let emit_subst self (subst : Subst.Projection.t) (parent_vars : Term.var list) :
    offset =
  let full_bindings =
    let existing_bindings = Subst.Projection.bindings subst in
    let bound_vars = List.map fst existing_bindings in
    let extra_bindings =
      List.filter_map
        (fun (v : Term.var) ->
          let v' = term_var_to_subst_var v in
          if List.mem v' bound_vars then
            None
          else (
            (* Check if [v] is actually in the substitution's domain.
               If so, it's bound (even if the image happens to have
               the same HVar.id as [v] — they're different clauses). *)
            let is_bound =
              Subst.mem
                (Subst.Projection.subst subst)
                (v', Subst.Projection.scope subst)
            in
            if is_bound then
              None
            else (
              let ty = HVar.ty v in
              let t =
                Subst.FO.apply
                  (Subst.Projection.renaming subst)
                  (Subst.Projection.subst subst)
                  (Term.var_of_int ~ty (HVar.id v), Subst.Projection.scope subst)
              in
              Some (v', fo_term_to_subst_term t)
            )
          ))
        parent_vars
    in
    existing_bindings @ extra_bindings
  in
  E.write_node self.enc "s" (fun enc ->
      List.iter
        (fun (v, t) ->
          let ty = Type.of_term_unsafe (HVar.ty v) in
          E.ref enc (emit_term self (Term.var_of_int ~ty (HVar.id v)));
          E.ref enc (emit_term self (Term.of_term_unsafe t)))
        full_bindings)

(** {2 Proof steps} *)

let tag_to_int = function
  | Builtin.Tag.T_lia -> 0
  | T_lra -> 1
  | T_ho -> 2
  | T_live_cnf -> 3
  | T_ho_norm -> 4
  | T_dont_increase_depth -> 5
  | T_ext -> 6
  | T_ind -> 7
  | T_data -> 8
  | T_distinct -> 9
  | T_cannot_orphan -> 10
  | T_ac _ -> 11

let role_to_str = function
  | Proof.R_assert -> "a"
  | Proof.R_goal -> "g"
  | Proof.R_def -> "d"
  | Proof.R_decl -> "D"
  | Proof.R_lemma -> "l"

let emit_parents_list enc parents =
  List.iter
    (fun (p_off, s_off) ->
      E.ref enc p_off;
      match s_off with
      | Some o -> E.ref enc o
      | None -> E.null enc)
    parents

let emit_rule_with_tags self cmd rule clause_off parents tags =
  E.write_node self.enc cmd (fun enc ->
      E.string enc (Proof.Rule.name rule);
      E.ref enc clause_off;
      emit_parents_list enc parents;
      E.string enc "|";
      List.iter (fun t -> E.int enc (tag_to_int t)) tags)

let emit_step self ~clause_off ~parents (step : Proof.Step.t) : offset =
  self.n_steps <- 1 + self.n_steps;
  match Proof.Step.kind step with
  | Proof.Intro (_, role) ->
    E.write_node self.enc "s.i" (fun enc ->
        E.string enc (role_to_str role);
        E.ref enc clause_off)
  | Proof.Inference (rule, tags) ->
    emit_rule_with_tags self "s.inf" rule clause_off parents tags
  | Proof.Simplification (rule, tags) ->
    emit_rule_with_tags self "s.simp" rule clause_off parents tags
  | Proof.Esa rule ->
    E.write_node self.enc "s.esa" (fun enc ->
        E.string enc (Proof.Rule.name rule);
        E.ref enc clause_off;
        emit_parents_list enc parents)
  | Proof.Trivial ->
    E.write_node self.enc "s.triv" (fun enc -> E.ref enc clause_off)
  | Proof.By_def id ->
    E.write_node self.enc "s.def" (fun enc ->
        E.string enc (Name.to_string id);
        E.ref enc clause_off)
  | Proof.Define (id, _) ->
    E.write_node self.enc "s.def" (fun enc ->
        E.string enc (Name.to_string id);
        E.ref enc clause_off;
        emit_parents_list enc parents)

let rec collect_terms_in_term (acc : Term.Set.t ref) (t : Term.t) =
  if not (Term.Set.mem t !acc) then (
    acc := Term.Set.add t !acc;
    match Term.view t with
    | Term.Var _v -> ()
    | Term.DB _ -> ()
    | Term.Const _ -> ()
    | Term.App (hd, args) ->
      collect_terms_in_term acc hd;
      List.iter (collect_terms_in_term acc) args
    | Term.Fun (_ty, body) -> collect_terms_in_term acc body
    | Term.AppBuiltin (_, args) -> List.iter (collect_terms_in_term acc) args
  )

let rec collect_types_in_term (acc : Type.Set.t ref) (t : Term.t) =
  match Term.view t with
  | Term.Var v -> collect_types acc (HVar.ty v)
  | Term.DB _ -> collect_types acc (Term.ty t)
  | Term.Const _ -> collect_types acc (Term.ty t)
  | Term.App (hd, args) ->
    collect_types_in_term acc hd;
    List.iter (collect_types_in_term acc) args
  | Term.Fun (ty, body) ->
    collect_types acc ty;
    collect_types_in_term acc body
  | Term.AppBuiltin (_, args) -> List.iter (collect_types_in_term acc) args

and collect_types (acc : Type.Set.t ref) (ty : Type.t) =
  if not (Type.Set.mem ty !acc) then (
    acc := Type.Set.add ty !acc;
    match Type.view ty with
    | Type.Var _ | Type.DB _ | Type.Builtin _ -> ()
    | Type.Fun (dom_list, cod) ->
      List.iter (collect_types acc) dom_list;
      collect_types acc cod
    | Type.App (_, args) -> List.iter (collect_types acc) args
    | Type.Forall ty' -> collect_types acc ty'
  )

let rec collect_names_in_term (acc : Name.Set.t ref) (t : Term.t) =
  match Term.view t with
  | Term.Const name -> acc := Name.Set.add name !acc
  | Term.App (hd, args) ->
    collect_names_in_term acc hd;
    List.iter (collect_names_in_term acc) args
  | Term.Fun (ty, body) ->
    collect_names_in_term acc (term_of_type ty);
    collect_names_in_term acc body
  | Term.AppBuiltin (_, args) -> List.iter (collect_names_in_term acc) args
  | _ -> ()

let collect_terms_in_literal acc = function
  | Literal.True | Literal.False -> ()
  | Literal.Equation (t1, t2, _) ->
    collect_terms_in_term acc t1;
    collect_terms_in_term acc t2

let collect_types_in_literal acc = function
  | Literal.True | Literal.False -> ()
  | Literal.Equation (t1, t2, _) ->
    collect_types_in_term acc t1;
    collect_types_in_term acc t2

let collect_names_in_literal acc = function
  | Literal.True | Literal.False -> ()
  | Literal.Equation (t1, t2, _) ->
    collect_names_in_term acc t1;
    collect_names_in_term acc t2

(** {2 Emit proof DAG} *)

let rec collect (seen : unit Int_tbl.t) acc (p : Proof.t) =
  let id = Proof.S.hash p in
  if not (Int_tbl.mem seen id) then (
    Int_tbl.add seen id ();
    List.iter
      (fun parent -> collect seen acc (Proof.Parent.proof parent))
      (Proof.Step.parents (Proof.S.step p));
    acc := p :: !acc
  )

let emit_proof self ~get_lits (root : Proof.t) : offset * stats =
  let seen = Int_tbl.create 64 in
  let steps = ref [] in
  collect seen steps root;
  let name_set = ref Name.Set.empty in
  let term_set = ref Term.Set.empty in
  let type_set = ref Type.Set.empty in
  List.iter
    (fun p ->
      Array.iter
        (fun lit ->
          collect_terms_in_literal term_set lit;
          collect_names_in_literal name_set lit;
          collect_types_in_literal type_set lit)
        (get_lits p))
    !steps;
  Name.Set.iter (fun n -> ignore (emit_name self n)) !name_set;
  Type.Set.iter (fun ty -> ignore (emit_type self ty)) !type_set;
  Term.Set.iter (fun t -> ignore (emit_term self t)) !term_set;
  List.iter
    (fun p ->
      Array.iter (fun lit -> ignore (emit_literal self lit)) (get_lits p))
    !steps;
  let step_offs : offset Int_tbl.t = Int_tbl.create 64 in
  List.iter
    (fun p ->
      let step = Proof.S.step p in
      let proof_id = Proof.S.hash p in
      let clause_off = emit_clause self (get_lits p) in
      let parents =
        List.map
          (fun parent ->
            let parent_proof = Proof.Parent.proof parent in
            let p_off = Int_tbl.find step_offs (Proof.S.hash parent_proof) in
            let s_off =
              match Proof.Parent.subst parent with
              | None -> None
              | Some s ->
                let parent_lits = get_lits parent_proof in
                let parent_vars =
                  Array.fold_left
                    (fun acc lit ->
                      Term.VarSet.union acc (Term.vars (Literal.to_ho_term lit)))
                    Term.VarSet.empty parent_lits
                  |> Term.VarSet.to_list
                in
                Some (emit_subst self s parent_vars)
            in
            p_off, s_off)
          (Proof.Step.parents step)
      in
      Int_tbl.add step_offs proof_id (emit_step self ~clause_off ~parents step))
    (List.rev !steps);
  let root_off = Int_tbl.find step_offs (Proof.S.hash root) in
  let result_off =
    E.write_node self.enc "result.unsat" (fun enc -> E.ref enc root_off)
  in
  let footer_off =
    E.write_node self.enc "zip.footer" (fun enc ->
        E.string enc "result";
        E.string enc "unsat";
        E.string enc "result-offset";
        E.ref enc result_off;
        E.string enc "tool";
        E.string enc "zipperposition")
  in
  let u32_bytes = Bytes.create 4 in
  Bytes.set_int32_le u32_bytes 0 (Int32.of_int (footer_off :> int));
  let _ : offset =
    E.write_node self.enc "mdag.end" (fun enc ->
        E.blob enc (Bytes.to_string u32_bytes))
  in
  E.flush self.enc;
  let stats = { n_steps = self.n_steps; n_terms = self.n_terms } in
  result_off, stats
