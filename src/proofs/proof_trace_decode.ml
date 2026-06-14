(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proof Trace Decoder} *)

open Logtk
module D = Zipperposition_mdag.Decode
module Int_tbl = CCHashtbl.Make (CCInt)

type t = {
  data: D.t;
  total_len: int;
  conv_ctx: Term.Conv.ctx;
  mutable node_cache: (string * D.value array) Int_tbl.t;
  mutable term_cache: Term.t Int_tbl.t;
  mutable name_cache: Name.t Int_tbl.t;
  mutable proof_cache: Proof.t Int_tbl.t;
  mutable clause_cache: Literal.t array Int_tbl.t;
  mutable subst_cache: Subst.Projection.t Int_tbl.t;
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
  | "¬" -> Some (Builtin.make_view Builtin_gen.Not)
  | "∧" -> Some (Builtin.make_view Builtin_gen.And)
  | "∨" -> Some (Builtin.make_view Builtin_gen.Or)
  | "⇒" -> Some (Builtin.make_view Builtin_gen.Imply)
  | "≡" -> Some (Builtin.make_view Builtin_gen.Equiv)
  | "<~>" -> Some (Builtin.make_view Builtin_gen.Xor)
  | "=" -> Some (Builtin.make_view Builtin_gen.Eq)
  | "≠" -> Some (Builtin.make_view Builtin_gen.Neq)
  | ":" -> Some (Builtin.make_view Builtin_gen.HasType)
  | "true" -> Some (Builtin.make_view Builtin_gen.True)
  | "false" -> Some (Builtin.make_view Builtin_gen.False)
  | "→" -> Some (Builtin.make_view Builtin_gen.Arrow)
  | "_" -> Some (Builtin.make_view Builtin_gen.Wildcard)
  | "Ms" -> Some (Builtin.make_view Builtin_gen.Multiset)
  | "type" -> Some (Builtin.make_view Builtin_gen.TType)
  | "prop" -> Some (Builtin.make_view Builtin_gen.Prop)
  | "ι" -> Some (Builtin.make_view Builtin_gen.Term)
  | "·∀" -> Some (Builtin.make_view Builtin_gen.ForallConst)
  | "·∃" -> Some (Builtin.make_view Builtin_gen.ExistsConst)
  | "·ε" -> Some (Builtin.make_view Builtin_gen.ChoiceConst)
  | "★" -> Some (Builtin.make_view Builtin_gen.Grounding)
  | "int" -> Some (Builtin.make_view Builtin_gen.TyInt)
  | "rat" -> Some (Builtin.make_view Builtin_gen.TyRat)
  | "real" -> Some (Builtin.make_view Builtin_gen.TyReal)
  | "floor" -> Some (Builtin.make_view Builtin_gen.Floor)
  | "ceiling" -> Some (Builtin.make_view Builtin_gen.Ceiling)
  | "truncate" -> Some (Builtin.make_view Builtin_gen.Truncate)
  | "round" -> Some (Builtin.make_view Builtin_gen.Round)
  | "prec" -> Some (Builtin.make_view Builtin_gen.Prec)
  | "succ" -> Some (Builtin.make_view Builtin_gen.Succ)
  | "uminus" -> Some (Builtin.make_view Builtin_gen.Uminus)
  | "×" -> Some (Builtin.make_view Builtin_gen.Product)
  | "quotient_e" -> Some (Builtin.make_view Builtin_gen.Quotient_e)
  | "quotient_t" -> Some (Builtin.make_view Builtin_gen.Quotient_t)
  | "quotient_f" -> Some (Builtin.make_view Builtin_gen.Quotient_f)
  | "remainder_e" -> Some (Builtin.make_view Builtin_gen.Remainder_e)
  | "remainder_t" -> Some (Builtin.make_view Builtin_gen.Remainder_t)
  | "remainder_f" -> Some (Builtin.make_view Builtin_gen.Remainder_f)
  | "is_int" -> Some (Builtin.make_view Builtin_gen.Is_int)
  | "is_rat" -> Some (Builtin.make_view Builtin_gen.Is_rat)
  | "to_int" -> Some (Builtin.make_view Builtin_gen.To_int)
  | "to_rat" -> Some (Builtin.make_view Builtin_gen.To_rat)
  | "≤" -> Some (Builtin.make_view Builtin_gen.Lesseq)
  | "≥" -> Some (Builtin.make_view Builtin_gen.Greatereq)
  | "<box>" -> Some (Builtin.make_view Builtin_gen.Box_opaque)
  | "distinct" -> Some (Builtin.make_view Builtin_gen.Distinct)
  | "B" -> Some (Builtin.make_view Builtin_gen.BComb)
  | "C" -> Some (Builtin.make_view Builtin_gen.CComb)
  | "I" -> Some (Builtin.make_view Builtin_gen.IComb)
  | "K" -> Some (Builtin.make_view Builtin_gen.KComb)
  | "S" -> Some (Builtin.make_view Builtin_gen.SComb)
  | "+" -> Some (Builtin.make_view Builtin_gen.Sum)
  | "-" -> Some (Builtin.make_view Builtin_gen.Difference)
  | "/" -> Some (Builtin.make_view Builtin_gen.Quotient)
  | "<" -> Some (Builtin.make_view Builtin_gen.Less)
  | ">" -> Some (Builtin.make_view Builtin_gen.Greater)
  | _ ->
    if String.length s > 3 && String.sub s 0 3 = "db_" then (
      try
        Some
          (Builtin.make_payload
             (Builtin.Pseudo_de_bruijn
                (int_of_string (String.sub s 3 (String.length s - 3)))))
      with _ -> None
    ) else if String.length s > 0 && s.[0] >= '0' && s.[0] <= '9' then
      if String.contains s '/' then (
        try Some (Builtin.make_payload (Builtin.Rat (Q.of_string s)))
        with _ -> Some (Builtin.make_payload (Builtin.Int (Z.of_string s)))
      ) else (
        try Some (Builtin.make_payload (Builtin.Int (Z.of_string s)))
        with _ -> None
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

let clause_to_form (lits : Literal.t array) : Term.t =
  Term.Form.or_l (Array.to_list lits |> List.map Literal.to_ho_term)

let term_to_form (ctx : Term.Conv.ctx) (t : Term.t) : Proof.form =
  Term.Conv.to_simple_term ~allow_free_db:true ctx t

let builtin_result_ty (b : Builtin.t) : Type.t =
  match Builtin.ty b with
  | `Int -> Type.int
  | `Rat -> Type.rat
  | `Other ->
    (match Builtin.view b with
    | Some Arrow
    | Some TType
    | Some Prop
    | Some Term
    | Some TyInt
    | Some TyRat
    | Some TyReal
    | Some Multiset
    | Some HasType ->
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
    Term.var_of_int ~ty id
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
    let dom_ty = ty_of_term (decode_term st dom_ref) in
    let cod_ty = ty_of_term (decode_term st cod_ref) in
    term_of_type (Type.arrow [ dom_ty ] cod_ty)
  | _ -> raise (D.Fail ("unexpected term command: " ^ cmd, off))

and decode_name (st : t) (off : D.offset) : Name.t =
  match Int_tbl.find st.name_cache off with
  | n -> n
  | exception Not_found ->
    let c = cursor_of_offset st off in
    let s = read_string c in
    let n = Name.make s in
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
      bindings := (Scoped.make v scope, Scoped.make rhs scope) :: !bindings
    done;
    let fo_subst = Subst.FO.of_list' (List.rev !bindings) in
    let proj =
      Subst.Projection.make Subst.Renaming.none (Scoped.make fo_subst scope)
    in
    Int_tbl.add st.subst_cache off proj;
    proj

and decode_proof_at (st : t) (off : D.offset) : Proof.t =
  if off = -1 then raise (D.Fail ("null ref in decode_proof_at", off));
  match Int_tbl.find st.proof_cache off with
  | p -> p
  | exception Not_found ->
    let cmd = cmd_of_offset st off in
    let c = cursor_of_offset st off in
    let step, clause_off = decode_step_node st c cmd off in
    let clause_lits = decode_clause st clause_off in
    let clause_term = clause_to_form clause_lits in
    let form = term_to_form st.conv_ctx clause_term in
    let result = Proof.Result.of_form form in
    let p = Proof.S.mk step result in
    Int_tbl.add st.proof_cache off p;
    p

and decode_step_node (st : t) (c : cursor) (cmd : string) (off : D.offset) :
    Proof.Step.t * D.offset =
  match cmd with
  | "s.i" ->
    let role_str = read_string c in
    let cl_ref = read_ref c in
    let role = role_of_string role_str in
    Proof.Step.intro (Proof.Src.internal []) role, cl_ref
  | "s.triv" ->
    let cl_ref = read_ref c in
    Proof.Step.trivial, cl_ref
  | "s.inf" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents, tags = read_parents_and_tags st c in
    Proof.Step.inference ~tags ~rule:(Proof.Rule.mk rule_str) parents, cl_ref
  | "s.simp" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents, tags = read_parents_and_tags st c in
    Proof.Step.simp ~tags ~rule:(Proof.Rule.mk rule_str) parents, cl_ref
  | "s.esa" ->
    let rule_str = read_string c in
    let cl_ref = read_ref c in
    let parents = read_parents_no_tags st c in
    Proof.Step.esa ~rule:(Proof.Rule.mk rule_str) parents, cl_ref
  | "s.def" ->
    let id_str = read_string c in
    let cl_ref = read_ref c in
    let parents = read_parents_no_tags st c in
    let name = Name.make id_str in
    let step =
      if parents = [] then
        Proof.Step.by_def name
      else
        Proof.Step.define name (Proof.Src.internal []) parents
    in
    step, cl_ref
  | _ -> raise (D.Fail ("unexpected step command: " ^ cmd, off))

and read_parent (st : t) (c : cursor) : Proof.Parent.t option =
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
      if s_off >= 0 then
        Some (Proof.Parent.from_subst_proj p (decode_subst st s_off))
      else
        Some (Proof.Parent.from p)
    | D.Null ->
      c.pos <- c.pos + 1;
      read_parent st c
    | D.String _ -> None
    | _ -> None
  )

and read_parents (st : t) (c : cursor) : Proof.Parent.t list =
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
      | D.Int64 i ->
        c.pos <- c.pos + 1;
        loop (tag_of_int (Int64.to_int i) :: acc)
      | _ -> raise (D.Fail ("expected int for tag", 0))
    )
  in
  loop []

and read_parents_and_tags (st : t) (c : cursor) :
    Proof.Parent.t list * Builtin.Tag.t list =
  let parents = read_parents st c in
  let _ : unit =
    if c.pos < Array.length c.args && c.args.(c.pos) = D.String "|" then
      c.pos <- c.pos + 1
  in
  let tags = read_tags c in
  parents, tags

and read_parents_no_tags (st : t) (c : cursor) : Proof.Parent.t list =
  let parents = read_parents st c in
  if c.pos < Array.length c.args then
    raise (D.Fail ("unexpected trailing data after parents", 0));
  parents

(* ── footer helpers ── *)

type footer_val =
  [ `Str of string
  | `Ref of D.offset
  ]

let read_footer (st : t) (footer_off : D.offset) : (string * footer_val) list =
  let cmd = cmd_of_offset st footer_off in
  if cmd <> "footer" then raise (D.Fail ("expected footer node", footer_off));
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
  if st.total_len < 6 then
    raise (D.Fail ("file too small for footer-offset", 0));
  let u32_bytes = Bytes.of_string (String.sub raw (st.total_len - 5) 4) in
  Int32.to_int (Bytes.get_int32_le u32_bytes 0)

(* ── main API ── *)

let create (s : string) : t =
  {
    data = D.create s;
    total_len = String.length s;
    conv_ctx = Term.Conv.create ();
    node_cache = Int_tbl.create 256;
    term_cache = Int_tbl.create 256;
    name_cache = Int_tbl.create 64;
    proof_cache = Int_tbl.create 64;
    clause_cache = Int_tbl.create 64;
    subst_cache = Int_tbl.create 64;
  }

let decode_proof (st : t) : Proof.t * (string * string) list =
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
    | None -> raise (D.Fail ("footer missing result-offset", footer_off))
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
