(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Decoder — Clause-Based} *)

open Logtk
module D = Zipperposition_mdag.Decode

let section = Util.Section.make "proof_trace_decode"

module Int_tbl = CCHashtbl.Make (CCInt)

module HVar_cache = CCHashtbl.Make (struct
  type t = int * Type.t

  let equal (i1, t1) (i2, t2) = i1 = i2 && t1 == t2
  let hash (i, t) = (46537 * i) + Hashtbl.hash t
end)

type t = {
  data: D.t;
  total_len: int;
  mutable node_cache: (string * D.value array) Int_tbl.t;
  mutable term_cache: Term.t Int_tbl.t;
  mutable name_cache: Name.t Int_tbl.t;
  mutable clause_cache: Literal.t array Int_tbl.t;
  mutable subst_cache: Subst.Projection.t Int_tbl.t;
  mutable hvar_cache: Term.var HVar_cache.t;
  mutable proof_cache: LLProof.t Int_tbl.t;
}

type cursor = {
  args: D.value array;
  mutable pos: int;
}

let read_val (c : cursor) : D.value =
  if c.pos >= Array.length c.args then
    D.Stop
  else (
    let v = c.args.(c.pos) in
    c.pos <- c.pos + 1;
    v
  )

let read_int (c : cursor) : int =
  match read_val c with
  | D.Int64 i -> Int64.to_int i
  | _ -> raise (D.Fail ("expected int", 0))

let read_ref (c : cursor) : D.offset =
  match read_val c with
  | D.Ref off -> off
  | D.Null -> -1
  | _ -> raise (D.Fail ("expected ref", 0))

let read_string (c : cursor) : string =
  match read_val c with
  | D.String s -> s
  | _ -> raise (D.Fail ("expected string", 0))

let read_all_refs (c : cursor) : D.offset list =
  let rec collect acc =
    if c.pos >= Array.length c.args then
      List.rev acc
    else (
      match c.args.(c.pos) with
      | D.Ref off ->
        c.pos <- c.pos + 1;
        collect (off :: acc)
      | _ -> raise (D.Fail ("expected ref", 0))
    )
  in
  collect []

let decode_node_args (data : D.t) (off : D.offset) : string * D.value array =
  D.read_node data off (fun dec cmd ->
      let rec collect acc =
        match D.read dec with
        | D.Stop -> Array.of_list (List.rev acc)
        | v -> collect (v :: acc)
      in
      cmd, collect [])

let cursor_of_offset (st : t) (off : D.offset) : cursor =
  match Int_tbl.find st.node_cache off with
  | _, args -> { args; pos = 0 }
  | exception Not_found ->
    let cmd, args = decode_node_args st.data off in
    Int_tbl.add st.node_cache off (cmd, args);
    { args; pos = 0 }

let cmd_of_offset (st : t) (off : D.offset) : string =
  match Int_tbl.find st.node_cache off with
  | cmd, _ -> cmd
  | exception Not_found ->
    let cmd, args = decode_node_args st.data off in
    Int_tbl.add st.node_cache off (cmd, args);
    cmd

external term_of_type : Type.t -> Term.t = "%identity"

let ty_of_term (t : Term.t) : Type.t =
  Type.of_term_unsafe (t : Term.t :> InnerTerm.t)

let builtin_of_string s =
  match s with
  | "¬" -> Some Builtin.Not
  | "∧" -> Some Builtin.And
  | "∨" -> Some Builtin.Or
  | "⇒" -> Some Builtin.Imply
  | "≡" -> Some Builtin.Equiv
  | "<~>" -> Some Builtin.Xor
  | "=" -> Some Builtin.Eq
  | "≠" -> Some Builtin.Neq
  | ":" -> Some Builtin.HasType
  | "true" -> Some Builtin.True
  | "false" -> Some Builtin.False
  | "→" -> Some Builtin.Arrow
  | "_" -> Some Builtin.Wildcard
  | "Ms" -> Some Builtin.Multiset
  | "type" -> Some Builtin.TType
  | "prop" -> Some Builtin.Prop
  | "ι" -> Some Builtin.Term
  | "·∀" -> Some Builtin.ForallConst
  | "·∃" -> Some Builtin.ExistsConst
  | "·ε" -> Some Builtin.ChoiceConst
  | "★" -> Some Builtin.Grounding
  | "int" -> Some Builtin.TyInt
  | "rat" -> Some Builtin.TyRat
  | "real" -> Some Builtin.TyReal
  | "floor" -> Some Builtin.Floor
  | "ceiling" -> Some Builtin.Ceiling
  | "truncate" -> Some Builtin.Truncate
  | "round" -> Some Builtin.Round
  | "prec" -> Some Builtin.Prec
  | "succ" -> Some Builtin.Succ
  | "uminus" -> Some Builtin.Uminus
  | "×" -> Some Builtin.Product
  | "quotient_e" -> Some Builtin.Quotient_e
  | "quotient_t" -> Some Builtin.Quotient_t
  | "quotient_f" -> Some Builtin.Quotient_f
  | "remainder_e" -> Some Builtin.Remainder_e
  | "remainder_t" -> Some Builtin.Remainder_t
  | "remainder_f" -> Some Builtin.Remainder_f
  | "is_int" -> Some Builtin.Is_int
  | "is_rat" -> Some Builtin.Is_rat
  | "to_int" -> Some Builtin.To_int
  | "to_rat" -> Some Builtin.To_rat
  | "≤" -> Some Builtin.Lesseq
  | "≥" -> Some Builtin.Greatereq
  | "<box>" -> Some Builtin.Box_opaque
  | "distinct" -> Some Builtin.Distinct
  | "B" -> Some Builtin.BComb
  | "C" -> Some Builtin.CComb
  | "I" -> Some Builtin.IComb
  | "K" -> Some Builtin.KComb
  | "S" -> Some Builtin.SComb
  | "+" -> Some Builtin.Sum
  | "-" -> Some Builtin.Difference
  | "/" -> Some Builtin.Quotient
  | "<" -> Some Builtin.Less
  | ">" -> Some Builtin.Greater
  | _ ->
    if String.length s > 3 && String.sub s 0 3 = "db_" then (
      try
        Some
          (Builtin.Pseudo_de_bruijn
             (int_of_string (String.sub s 3 (String.length s - 3))))
      with _ -> None
    ) else if String.length s > 0 && s.[0] >= '0' && s.[0] <= '9' then
      if String.contains s '/' then (
        try Some (Builtin.Rat (Q.of_string s))
        with _ -> Some (Builtin.Int (Z.of_string s))
      ) else (
        try Some (Builtin.Int (Z.of_string s)) with _ -> None
      )
    else
      None

let tag_of_int = function
  | 0 -> Builtin.Tag.T_lia
  | 1 -> T_lra
  | 2 -> T_ho
  | 3 -> T_live_cnf
  | 4 -> T_ho_norm
  | 5 -> T_dont_increase_depth
  | 6 -> T_ext
  | 7 -> T_ind
  | 8 -> T_data
  | 9 -> T_distinct
  | 10 -> T_cannot_orphan
  | 11 -> T_ac (Name.make "?ac")
  | n -> raise (D.Fail (Printf.sprintf "unknown tag int: %d" n, 0))

let role_of_string = function
  | "a" -> Proof.R_assert
  | "g" -> Proof.R_goal
  | "d" -> Proof.R_def
  | "D" -> Proof.R_decl
  | "l" -> Proof.R_lemma
  | s -> raise (D.Fail ("unknown role: " ^ s, 0))

let builtin_result_ty (b : Builtin.t) : Type.t =
  match Builtin.ty b with
  | `Int -> Type.int
  | `Rat -> Type.rat
  | `Other ->
    (match b with
    | Arrow | TType | Prop | Term | TyInt | TyRat | TyReal | Multiset | HasType
      ->
      Type.tType
    | _ -> Type.prop)

(* ── recursive decoders ── *)

let rec decode_term (st : t) (off : D.offset) : Term.t =
  if off = -1 then raise (D.Fail ("null ref in decode_term", off));
  match Int_tbl.find st.term_cache off with
  | t -> t
  | exception Not_found ->
    let cmd = cmd_of_offset st off in
    let c = cursor_of_offset st off in
    let t = decode_term_node st c cmd off in
    Int_tbl.add st.term_cache off t;
    t

and decode_term_node (st : t) (c : cursor) (cmd : string) (off : D.offset) :
    Term.t =
  match cmd with
  | "t.v" ->
    let id = read_int c in
    let ty_ref = read_ref c in
    let ty = ty_of_term (decode_term st ty_ref) in
    let hvar =
      let key = id, ty in
      match HVar_cache.find_opt st.hvar_cache key with
      | Some v -> v
      | None ->
        let t = Term.var_of_int ~ty id in
        let v =
          match Term.view t with
          | Term.Var v -> v
          | _ -> assert false
        in
        HVar_cache.replace st.hvar_cache key v;
        v
    in
    Term.var hvar
  | "t.d" ->
    let i = read_int c in
    let ty_ref = read_ref c in
    let ty = ty_of_term (decode_term st ty_ref) in
    Term.bvar ~ty i
  | "t.c" ->
    let name_ref = read_ref c in
    let ty_ref = read_ref c in
    let name = decode_name st name_ref in
    let ty = ty_of_term (decode_term st ty_ref) in
    Term.const ~ty name
  | "t.@" ->
    let hd_ref = read_ref c in
    let hd = decode_term st hd_ref in
    let arg_refs = read_all_refs c in
    let args = List.map (decode_term st) arg_refs in
    Term.app hd args
  | "t.f" ->
    let ty_ref = read_ref c in
    let body_ref = read_ref c in
    let ty = ty_of_term (decode_term st ty_ref) in
    let body = decode_term st body_ref in
    Term.fun_ ty body
  | "t.b" ->
    let b_str = read_string c in
    let b =
      match builtin_of_string b_str with
      | Some b -> b
      | None -> raise (D.Fail ("unknown builtin: " ^ b_str, off))
    in
    let arg_refs = read_all_refs c in
    let args = List.map (decode_term st) arg_refs in
    Term.app_builtin ~ty:(builtin_result_ty b) b args
  | "ty.type" -> term_of_type Type.tType
  | "ty.prop" -> term_of_type Type.prop
  | "ty.int" -> term_of_type Type.int
  | "ty.rat" -> term_of_type Type.rat
  | "ty.term" -> term_of_type Type.term
  | "ty.real" -> term_of_type Type.real
  | "ty.arrow" ->
    let dom_ref = read_ref c in
    let cod_ref = read_ref c in
    let dom = ty_of_term (decode_term st dom_ref) in
    let cod = ty_of_term (decode_term st cod_ref) in
    term_of_type (Type.arrow [ dom ] cod)
  | "ty.name" ->
    let name_ref = read_ref c in
    (* type constants fall through to term encoding in the encoder;
       this case is never reached in the current format.  If added later,
       this must decode the named type properly, not just return tType. *)
    let name = decode_name st name_ref in
    raise (D.Fail ("ty.name not implemented yet: " ^ Name.to_string name, off))
  | _ -> raise (D.Fail ("unexpected term command: " ^ cmd, off))

and decode_name (st : t) (off : D.offset) : Name.t =
  match Int_tbl.find st.name_cache off with
  | n -> n
  | exception Not_found ->
    let cmd = cmd_of_offset st off in
    let c = cursor_of_offset st off in
    let n =
      match cmd with
      | "n" ->
        let s = read_string c in
        Name.make s
      | _ -> raise (D.Fail ("expected name node, got: " ^ cmd, off))
    in
    Int_tbl.add st.name_cache off n;
    n

and decode_clause (st : t) (off : D.offset) : Literal.t array =
  if off = -1 then raise (D.Fail ("null ref in decode_clause", off));
  match Int_tbl.find st.clause_cache off with
  | lits -> lits
  | exception Not_found ->
    let c = cursor_of_offset st off in
    let refs = read_all_refs c in
    let lits = Array.of_list (List.map (decode_literal st) refs) in
    Int_tbl.add st.clause_cache off lits;
    lits

and decode_literal (st : t) (off : D.offset) : Literal.t =
  let cmd = cmd_of_offset st off in
  let c = cursor_of_offset st off in
  match cmd with
  | "l.t" -> Literal.mk_tauto
  | "l.f" -> Literal.mk_absurd
  | "l.=" ->
    let t1_ref = read_ref c in
    let t2_ref = read_ref c in
    let t1 = decode_term st t1_ref in
    let t2 = decode_term st t2_ref in
    Literal.mk_eq t1 t2
  | "l.!=" ->
    let t1_ref = read_ref c in
    let t2_ref = read_ref c in
    let t1 = decode_term st t1_ref in
    let t2 = decode_term st t2_ref in
    Literal.mk_neq t1 t2
  | _ -> raise (D.Fail ("unexpected literal command: " ^ cmd, off))

and decode_subst (st : t) (off : D.offset) : Subst.Projection.t =
  if off = -1 then raise (D.Fail ("null in decode_subst", off));
  match Int_tbl.find st.subst_cache off with
  | s -> s
  | exception Not_found ->
    let c = cursor_of_offset st off in
    let scope = 0 in
    let bindings = ref [] in
    while c.pos < Array.length c.args do
      let var_ref = read_ref c in
      let term_ref = read_ref c in
      let var_term = decode_term st var_ref in
      let v =
        match Term.view var_term with
        | Term.Var v -> v
        | _ -> raise (D.Fail ("expected variable in substitution", off))
      in
      let rhs = decode_term st term_ref in
      let is_identity =
        match Term.view rhs with
        | Term.Var v2 -> HVar.equal Type.equal v v2
        | _ -> false
      in
      if not is_identity then
        bindings := (Scoped.make v scope, Scoped.make rhs scope) :: !bindings
    done;
    let fo_subst = Subst.FO.of_list' (List.rev !bindings) in
    let proj =
      Subst.Projection.make Subst.Renaming.none (Scoped.make fo_subst scope)
    in
    Int_tbl.add st.subst_cache off proj;
    proj

(** Convert a decoded [Subst.Projection.t] to an [LLProof.inst]. Reads raw
    bindings without applying the substitution recursively, since the MDAG trace
    already contains the fully-applied terms. *)
and subst_to_inst (proj : Subst.Projection.t) : LLProof.inst =
  Subst.fold
    (fun acc (v, _sc_v) (t, _sc_t) ->
      if _sc_v = proj.scope then
        (Type.cast_var_unsafe v, Term.of_term_unsafe t) :: acc
      else
        acc)
    []
    (Subst.Projection.subst proj)

and decode_proof_at (st : t) (off : D.offset) : LLProof.t =
  if off = -1 then raise (D.Fail ("null ref in decode_proof_at", off));
  match Int_tbl.find st.proof_cache off with
  | p -> p
  | exception Not_found ->
    let cmd = cmd_of_offset st off in
    let c = cursor_of_offset st off in
    let step, clause_off = decode_step_node st c cmd off in
    let clause = decode_clause st clause_off in
    let p = LLProof.mk_ clause step in
    Int_tbl.add st.proof_cache off p;
    p

and decode_step_node (st : t) (c : cursor) (cmd : string) (off : D.offset) :
    LLProof.step * D.offset =
  Util.debugf ~section 3 "decode_step_node off=%d cmd=%s" (fun k -> k off cmd);
  match cmd with
  | "s.i" ->
    let role_str = read_string c in
    let cl_ref = read_ref c in
    let role = role_of_string role_str in
    let step =
      match role with
      | Proof.R_assert -> LLProof.Assert
      | Proof.R_goal -> LLProof.Goal
      | Proof.R_lemma -> LLProof.Goal
      | Proof.R_def | Proof.R_decl -> LLProof.Trivial
    in
    step, cl_ref
  | "s.triv" ->
    let cl_ref = read_ref c in
    LLProof.Trivial, cl_ref
  | "s.inf" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents, tags = read_parents_and_tags st c in
    LLProof.Inference { name = rule_str; tags; parents }, cl_ref
  | "s.simp" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents, tags = read_parents_and_tags st c in
    LLProof.Inference { name = rule_str; tags; parents }, cl_ref
  | "s.esa" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents = read_parents_skip_subst st c in
    if c.pos < Array.length c.args && c.args.(c.pos) = D.String "|" then
      c.pos <- c.pos + 1;
    if c.pos < Array.length c.args then
      raise (D.Fail ("unexpected trailing data after parents", 0));
    ( LLProof.Esa (rule_str, List.map (fun p -> p.LLProof.p_proof) parents),
      cl_ref )
  | "s.def" ->
    let id_str = read_string c in
    let cl_ref = read_ref c in
    let parents = read_parents_no_tags st c in
    let name = Name.make id_str in
    let step =
      if parents = [] then
        LLProof.By_def name
      else
        LLProof.Define name
    in
    step, cl_ref
  | _ -> raise (D.Fail ("unexpected step command: " ^ cmd, off))

and read_parent (st : t) (c : cursor) : LLProof.parent option =
  if c.pos >= Array.length c.args then
    None
  else (
    match c.args.(c.pos) with
    | D.Ref p_ref ->
      c.pos <- c.pos + 1;
      let p = decode_proof_at st p_ref in
      let s_off =
        if
          c.pos < Array.length c.args
          &&
          match c.args.(c.pos) with
          | D.Ref _ -> true
          | _ -> false
        then
          read_ref c
        else
          -1
      in
      if s_off >= 0 then (
        let subst = decode_subst st s_off in
        let inst = subst_to_inst subst in
        Some (LLProof.p_inst p inst)
      ) else
        Some (LLProof.p_of p)
    | D.Null ->
      c.pos <- c.pos + 1;
      read_parent st c
    | D.String _ -> None
    | _ -> None
  )

and read_parents_skip_subst (st : t) (c : cursor) : LLProof.parent list =
  let rec loop acc =
    if c.pos >= Array.length c.args then
      List.rev acc
    else (
      match c.args.(c.pos) with
      | D.Ref p_ref ->
        c.pos <- c.pos + 1;
        (* skip the substitution ref/null slot *)
        let _subst_off = read_ref c in
        let p = decode_proof_at st p_ref in
        loop (LLProof.p_of p :: acc)
      | D.Null ->
        c.pos <- c.pos + 1;
        loop acc
      | _ -> List.rev acc
    )
  in
  loop []

and read_parents (st : t) (c : cursor) : LLProof.parent list =
  let rec loop acc =
    match read_parent st c with
    | Some p -> loop (p :: acc)
    | None -> List.rev acc
  in
  loop []

and read_tags (c : cursor) : Builtin.Tag.t list =
  let rec loop acc =
    if c.pos >= Array.length c.args then
      List.rev acc
    else (
      match c.args.(c.pos) with
      | D.String "|" ->
        c.pos <- c.pos + 1;
        loop acc
      | D.Int64 n ->
        c.pos <- c.pos + 1;
        loop (tag_of_int (Int64.to_int n) :: acc)
      | D.String _ -> List.rev acc
      | _ -> raise (D.Fail ("expected int for tag", 0))
    )
  in
  loop []

and read_parents_and_tags (st : t) (c : cursor) :
    LLProof.parent list * Builtin.Tag.t list =
  let parents = read_parents st c in
  let _ : unit =
    if c.pos < Array.length c.args && c.args.(c.pos) = D.String "|" then
      c.pos <- c.pos + 1
  in
  let tags = read_tags c in
  parents, tags

and read_parents_no_tags (st : t) (c : cursor) : LLProof.parent list =
  let parents = read_parents st c in
  let _ : unit =
    if c.pos < Array.length c.args && c.args.(c.pos) = D.String "|" then
      c.pos <- c.pos + 1
  in
  if c.pos < Array.length c.args then
    raise (D.Fail ("unexpected trailing data after parents", 0));
  parents

type footer_val =
  [ `Str of string
  | `Ref of D.offset
  ]

let read_footer (st : t) (footer_off : D.offset) : (string * footer_val) list =
  let cmd = cmd_of_offset st footer_off in
  if cmd <> "zip.footer" then
    raise (D.Fail ("expected zip.footer node", footer_off));
  let fc = cursor_of_offset st footer_off in
  let rec loop acc =
    if fc.pos >= Array.length fc.args then
      List.rev acc
    else (
      let key = read_string fc in
      if fc.pos >= Array.length fc.args then
        List.rev acc
      else (
        match fc.args.(fc.pos) with
        | D.String v ->
          fc.pos <- fc.pos + 1;
          loop ((key, `Str v) :: acc)
        | D.Ref off ->
          fc.pos <- fc.pos + 1;
          loop ((key, `Ref off) :: acc)
        | _ ->
          raise (D.Fail ("expected string or ref in footer value", footer_off))
      )
    )
  in
  loop []

let find_footer_offset (st : t) : D.offset =
  let raw = D.raw_string st.data in
  if st.total_len < 6 then raise (D.Fail ("file too small for mdag.end", 0));
  let u32_bytes = Bytes.of_string (String.sub raw (st.total_len - 5) 4) in
  Int32.to_int (Bytes.get_int32_le u32_bytes 0)

let create (s : string) : t =
  {
    data = D.create s;
    total_len = String.length s;
    node_cache = Int_tbl.create 256;
    term_cache = Int_tbl.create 256;
    name_cache = Int_tbl.create 64;
    proof_cache = Int_tbl.create 64;
    clause_cache = Int_tbl.create 64;
    subst_cache = Int_tbl.create 64;
    hvar_cache = HVar_cache.create 256;
  }

let decode_proof (st : t) : LLProof.t * (string * string) list =
  let footer_off = find_footer_offset st in
  if footer_off >= st.total_len then
    raise (D.Fail ("invalid footer offset", footer_off));
  let footer_kv = read_footer st footer_off in
  let result_off =
    match
      List.find_map
        (function
          | "result-offset", `Ref off -> Some off
          | _ -> None)
        footer_kv
    with
    | Some off -> off
    | None -> raise (D.Fail ("zip.footer missing result-offset", footer_off))
  in
  let cmd = cmd_of_offset st result_off in
  let proof =
    match cmd with
    | "result.unsat" ->
      let rc = cursor_of_offset st result_off in
      let proof_ref = read_ref rc in
      decode_proof_at st proof_ref
    | _ -> raise (D.Fail ("unsupported result type: " ^ cmd, result_off))
  in
  let str_pairs =
    List.filter_map
      (function
        | k, `Str v -> Some (k, v)
        | _ -> None)
      footer_kv
  in
  proof, str_pairs
